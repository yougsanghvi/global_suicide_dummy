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

install_if_missing("crayon")
cat(blue("loading required datasets and libraries"))
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

# Set file paths
dir_path <- "/global/scratch/users/yougsanghvi"

results_folder <- "aggregated_results_gdnat_usa"
results_file_name <- "gdnat_usa_agg_all_years.csv"
results_file_path <- file.path(dir_path, results_folder, results_file_name)

regression_beta_fn <- "regression_coefficients_USA_poly4_lag11.csv"
regression_beta_fp <- file.path(dir_path, regression_beta_fn)

era5_folderpath <- file.path("merged", "USA")
era5_filename <- "USA_adm2_1968_2004_monthly.dta"
era5_filepath <- file.path(dir_path, era5_folderpath, era5_filename)

# era5 non merged file path
era5_folderpath <- file.path("data", "climatedata", "USA")
era5_filename <- "usa_area_era5_temp_average_1968_2004_polynomial_5_area_crop_weights.csv"
era5_filepath <- file.path(dir_path, era5_folderpath, era5_filename)

# Define paths for usa county shapefile
usa_county_dir <- file.path(dir_path, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Define paths for geocode crosswalks file
geocode_folder <- file.path("data", "raw", "USA", "geocode")
geocode_filename <- "geocode_91_93.csv"
geocode_filepath <- file.path(dir_path, geocode_folder, geocode_filename)

# output path
output_path <- file.path(dir_path, "gdnat_era5_compare_output")

# Load the required datasets
stagg_results <- read.csv(results_file_path)
regression_betas <- read.csv(regression_beta_fp)
era5_results <- read.csv(era5_filepath)
usa_shapefile <- st_read(usa_county_path)
geocode_file <- read.csv(geocode_filepath)

# ----- II. Data Cleaning # ------------
cat(magenta("cleaning data"))
#1. Cleaning gdnat data

# dropping duplicates
stagg_results_unique <- stagg_results %>%
    distinct()

# NA checking done in python, for now we just drop the NA values
# It seems the NA values are all counties with water in them
stagg_results_na_drop <- stagg_results_unique[
    complete.cases(stagg_results_unique),
]

#2. Cleaning ERA5 data

# Checking NAs for ERA5 Data -- checks for GDNat data are done in python
na_counts <- sapply(era5_results, function(x) sum(is.na(x)))
era5_results_na <- era5_results[is.na(era5_results$order_1), ]

era5_results_clean <- era5_results %>%
  mutate(month = as.numeric(sub("month_", "", month)))

# ----- III. Running predictions for the models -------
cat(green("creating predicted suicide rate"))

# ----- Function to Predict -------#

compute_lagged_stagg_yhat <- function(stagg_data, regression_betas) {
    # Step 1: Create date column
    stagg_data <- stagg_data %>%
        mutate(date = as.Date(paste(year, month, "01", sep = "-")))

    # Step 2: Days in month
    stagg_data <- stagg_data %>%
        mutate(days_in_month = lubridate::days_in_month(date))

    # Step 3: Compute monthly average for orders 1-4
    stagg_data <- stagg_data %>%
        mutate(across(
            order_1:order_4,
            ~ . / days_in_month,
            .names = "{.col}_avg"
        )) %>%
        arrange(poly_id, date) %>%
        group_by(poly_id) %>%
        mutate(across(
            ends_with("_avg"),
            list(
                `0` = ~.,
                `1` = ~ lag(., 1),
                `2` = ~ lag(., 2),
                `3` = ~ lag(., 3),
                `4` = ~ lag(., 4),
                `5` = ~ lag(., 5),
                `6` = ~ lag(., 6),
                `7` = ~ lag(., 7),
                `8` = ~ lag(., 8),
                `9` = ~ lag(., 9),
                `10` = ~ lag(., 10),
                `11` = ~ lag(., 11)
            ),
            .names = "{.col}_lag{.fn}"
        )) %>%
        ungroup()

    # Step 4: Rename lagged variable names to match regression term names
    rename_vars <- expand.grid(order = 1:4, lag = 0:11) %>%
        mutate(
            old = paste0("order_", order, "_avg_lag", lag),
            new = paste0("tavg_poly", order, "_l", lag)
        )

    names(stagg_data)[match(
        rename_vars$old,
        names(stagg_data)
    )] <- rename_vars$new

    # Step 5: Remove first 11 months per poly_id (due to lagging)
    stagg_data <- stagg_data %>%
        arrange(poly_id, date) %>%
        group_by(poly_id) %>%
        slice(-(1:11)) %>%
        ungroup()

    # Step 6: Compute predicted y_hat using regression betas
    coef_vector <- setNames(regression_betas$beta, regression_betas$term)

    if (!all(names(coef_vector) %in% names(stagg_data))) {
        stop("Some regression terms are not in the dataset columns.")
    }

    # Multiply the coefficients with the corresponding columns
    # and compute the predicted y_hat using matrix multiplication
    X <- as.matrix(stagg_data[, names(coef_vector)])
    stagg_data$y_hat <- as.vector(X %*% coef_vector)

    return(stagg_data)
}

# Predicting for GDNat
stagg_results_lagged_clean <- compute_lagged_stagg_yhat(
    stagg_results_na_drop,
    regression_betas
)

# Predicting for ERA5
era5_results_lagged_clean <- compute_lagged_stagg_yhat(
    era5_results_clean,
    regression_betas
)

era5_results_lagged_clean <- era5_results_lagged_clean %>%
    rename(y_hat_era5 = y_hat)

stagg_results_lagged_clean <- stagg_results_lagged_clean %>%
    rename(y_hat_gdnat = y_hat)

# ----- IV. Merging Predictions -------

cat(yellow("merging and saving the final dataset"))
# Pre-processing before merge

# merging in county names to verify the keys...
# ... are accurate for merging later on
usa_shapefile_filtered <- usa_shapefile %>%
    mutate(GEOID_int = as.integer(GEOID)) %>%
    select(GEOID_int, NAME, NAMELSAD, ID_1, ID_2, geometry)

gdnat_final <- stagg_results_lagged_clean %>%
    filter(year < 2005 & year > 1979) %>% # 1979 only has values for december ...
    # ... need to check if this is an issue of just data format
    mutate(poly_id_int = as.integer(poly_id)) %>%
    arrange(poly_id_int, year, month) %>%
    left_join(
        usa_shapefile_filtered,
        by = c(
            "poly_id_int" = "GEOID_int"
        )
    ) %>%
    select(
        year,
        month,
        y_hat_gdnat,
        poly_id_int,
        NAME,
        NAMELSAD,
        ID_1,
        ID_2,
        date,
        order_1_avg
    )


era5_final <- era5_results_lagged_clean %>%
    select(
        year,
        month,
        order_1_avg,
        date,
        y_hat_era5,
        poly_id
    ) %>%
    filter(year > 1979) # we only have gdnat data post 1979

merged_data_panel_clean <- full_join(
  era5_final,
  gdnat_final,
  by = c(
    "poly_id" = "poly_id_int",
    "year" = "year",
    "month" = "month"
  )
)

# rename the order one averages to include era5 or gdnat 

colSums(is.na(merged_data_panel_clean))


panel_output_filename <- "merged_data_panel_extended.csv"
panel_output_filepath <- file.path(output_path, panel_output_filename)

readr::write_csv(merged_data_panel_clean, panel_output_filepath)