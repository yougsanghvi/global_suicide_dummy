# ----- I. Libraries and File Paths ------------
#' Helper function to install and load CRAN packages if they are missing
#'
#' @param pkg Character string: The name of the package to install and load.
#' @param ... Additional arguments passed to `install.packages()`.
install_if_missing <- function(pkg, ...) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(
            pkg,
            repos = "https://cloud.r-project.org",
            ...
        )
    }
    library(pkg, character.only = TRUE)
}

source(
    "/global/home/users/yougsanghvi/global_suicide_dummy/code/y_utils/config.R"
)

install_if_missing("crayon")
cat(blue("loading required datasets and libraries \n"))
install_if_missing("dplyr")
install_if_missing("lubridate")
install_if_missing("fixest")
install_if_missing("sf")
install_if_missing("readstata13")
install_if_missing("tidyr")
install_if_missing("data.table")
install_if_missing("gtable")
install_if_missing("patchwork")
install_if_missing("ggplot2")
install_if_missing("stringr")

dir_path <- "/global/scratch/users/yougsanghvi"

regression_beta_fn <- "regression_coefficients_USA_poly4_lag11.csv"
regression_beta_fp <- file.path(dir_path, regression_beta_fn)

era5_filepath <- file.path(ERA5_AGG_FOLDER, "era5_usa_agg_1979_2004.csv")

# Define paths for usa county shapefile
usa_county_dir <- file.path(dir_path, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Define paths for geocode crosswalks file
geocode_folder <- file.path("data", "raw", "USA", "geocode")
geocode_filename <- "geocode_91_93.csv"
geocode_filepath <- file.path(dir_path, geocode_folder, geocode_filename)

models <- c("MIROC6", "ACCESS-ESM1-5", "CanESM5", "MRI-ESM2-0", "NorESM2-LM", "ACCESS-CM2")

for (model in models) {
    cat(blue(sprintf("\n--- Running model: %s ---\n", model)))

    # Create output directory for this model
    model_output_dir <- file.path(PROJECT_FOLDER, "results", model)
    if (!dir.exists(model_output_dir)) {
        dir.create(model_output_dir, recursive = TRUE)
    }

    # Construct expected output file path
    output_csv_name <- sprintf("attribution_output_%s.csv", model)
    output_csv_path <- file.path(model_output_dir, output_csv_name)

    # Skip if file already exists
    if (file.exists(output_csv_path)) {
        cat(yellow(sprintf("Skipping %s: output already exists at %s\n", model, output_csv_path)))
        next
    }

    # Update GDNat input file path for current model
    gdnat_file_path <- file.path(
        GDNAT_USA_AGG_ALL_MODELS_FP,
        model,
        sprintf("gdnat_usa_agg_%s_1979_2004.csv", model)
    )

    
    # Load the required datasets
    gdnat_data <- read.csv(gdnat_file_path)
    regression_betas <- read.csv(regression_beta_fp)
    era5_data <- read.csv(era5_filepath)
    usa_shapefile <- st_read(usa_county_path)
    geocode_file <- read.csv(geocode_filepath)

    # ----- II. Data Cleaning # ------------
    cat(magenta("cleaning data \n"))

    #1. Cleaning regression betas data
    # Filter to only temperature polynomial terms
    regression_betas_filtered <- regression_betas %>%
        select(-fe)

    # 2. cleaning gdnat and era5 data 
    # === Reusable cleaning function ===
    clean_agg_data <- function(data, source = c("era5", "gdnat")) {
    source <- match.arg(source)
    
    cat("Processing", source, "data...\n")
    
    # Identify counties with any NA
    counties_with_na <- data %>%
        filter(if_any(everything(), is.na)) %>%
        distinct(poly_id)
    
    cat("Number of counties with NA in data:", nrow(counties_with_na), "\n")
    
    # Remove counties with NA and drop duplicates
    data_clean <- data %>%
        filter(!(poly_id %in% counties_with_na$poly_id)) %>%
        distinct()
    
    # Rename the order_x_avg columns
    rename_suffix <- paste0("_", source)
    old_cols <- paste0("order_", 1:4, "_avg")
    existing_old_cols <- intersect(old_cols, colnames(data_clean))
    
    rename_map <- setNames(
        existing_old_cols,
        paste0(existing_old_cols, rename_suffix)
    )
    
    # Apply renaming and add integer poly_id
    data_processed <- data_clean %>%
        mutate(poly_id_int = as.integer(poly_id)) %>%
        rename(!!!rename_map)
    
    return(data_processed)
    }

    # === Apply to GDNat and ERA5 data ===
    print("gdnat data colnames:")
    print(colnames(gdnat_data))
    print("era5 data colnames:")
    print(colnames(era5_data))

    gdnat_processed <- clean_agg_data(gdnat_data, "gdnat")
    era5_processed <- clean_agg_data(era5_data, "era5")

    # 4. Merging gdnat and era5 data
    merged_data_panel <- full_join(
        gdnat_processed,
        era5_processed,
        by = c(
            "poly_id_int", # Match on the ID
            "month", # Match on the month column (assuming it's named 'month' in both)
            "year" # Match on the year column (assuming it's named 'year' in both)
        )
    )

    # === NA diagnostics per column ===
    na_summary <- sapply(merged_data_panel, function(x) sum(is.na(x)))
    na_percent <- round(na_summary / nrow(merged_data_panel) * 100, 2)

    na_diagnostics <- data.frame(
    column = names(na_summary),
    n_na = na_summary,
    pct_na = na_percent
    )

    na_diagnostics <- na_diagnostics %>% filter(n_na > 0)

    cat("=== NA Diagnostics by Column ===\n")
    print(na_diagnostics)

    # === Total rows with any NA ===
    n_rows_with_na <- sum(!complete.cases(merged_data_panel))
    cat("\nTotal rows with any NA:", n_rows_with_na, "\n")

    # === Unique counties with any NA row ===
    rows_with_na <- merged_data_panel %>% filter(!complete.cases(.))
    unique_counties_with_na <- n_distinct(rows_with_na$poly_id_int)

    total_unique_counties <- n_distinct(merged_data_panel$poly_id_int)
    pct_counties_with_na <- round(unique_counties_with_na / total_unique_counties * 100, 2)

    cat("\nUnique counties with any NA row:", unique_counties_with_na, "of", total_unique_counties, 
        sprintf("(%s%%)", pct_counties_with_na), "\n")

    merged_data_panel_clean <- merged_data_panel %>%
        mutate(
            diff_poly1 = order_1_avg_era5 - order_1_avg_gdnat,
            diff_poly2 = order_2_avg_era5 - order_2_avg_gdnat,
            diff_poly3 = order_3_avg_era5 - order_3_avg_gdnat,
            diff_poly4 = order_4_avg_era5 - order_4_avg_gdnat
        ) %>%
        # Generate 11 lags for each diff variable, grouped by county
        arrange(poly_id_int, year, month) %>%
        group_by(poly_id_int) %>%
        mutate(across(
            starts_with("diff_poly"),
            list(
                l0 = ~.,
                l1 = ~ lag(., 1),
                l2 = ~ lag(., 2),
                l3 = ~ lag(., 3),
                l4 = ~ lag(., 4),
                l5 = ~ lag(., 5),
                l6 = ~ lag(., 6),
                l7 = ~ lag(., 7),
                l8 = ~ lag(., 8),
                l9 = ~ lag(., 9),
                l10 = ~ lag(., 10),
                l11 = ~ lag(., 11)
            ),
            .names = "{.col}_l{gsub('l', '', .fn)}"
        )) %>%
        ungroup()

    # ----- III. Running predictions for the models -------
    cat(green("running predictions for the models \n"))

    # Rename terms: "tavg_poly1_l0" → "diff_poly1_l0", etc.
    regression_betas_filtered <- regression_betas_filtered %>%
        mutate(term = str_replace(term, "tavg_poly", "diff_poly"))

    # 1. Extract coefficient vector from regression betas:
    coef_vector <- setNames(
        regression_betas_filtered$beta,
        regression_betas_filtered$term
    )

    # 2. Select predictor columns from your dataset matching the names in coef_vector
    X_diff <- merged_data_panel_clean[, names(coef_vector)]

    # 3. Convert to matrix
    X_mat <- as.matrix(X_diff)

    # 4. Calculate predicted value vector (dot product of X_mat and coef_vector)
    predicted_values <- X_mat %*% coef_vector

    # 5. Add predictions as new column to your data frame
    merged_data_panel_clean$y_hat_diff <- as.vector(predicted_values)

    # ----- IV. Save the results to a CSV file ------

    cat(yellow("saving the results to a CSV file \n"))    

    # Replace the write_csv call at the end with:
    readr::write_csv(merged_data_panel_clean, output_csv_path)
    cat(green(sprintf("Saved model results to %s\n", output_csv_path)))
}
