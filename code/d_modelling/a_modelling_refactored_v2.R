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

# Set file paths
dir_path <- "/global/scratch/users/yougsanghvi"

gdnat_file_path <- file.path(
    GDNAT_USA_AGG_ALL_MODELS_FP,
    "ACCESS-CM2",
    "gdnat_usa_agg_ACCESS-CM2_1979_2004.csv"
)

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

# output path
output_path <- file.path(PROJECT_FOLDER, "gdnat_era5_compare_output")

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

#2. Cleaning gdnat data
# filtering era5 data to 1979-2004
gdnat_data_79_04 <- gdnat_data %>%
    filter(year >= 1979 & year <= 2004) %>%
    # filtering two counties 2016 and 2185 which have missing data
    # for only particular months.. this needs to be fixed
    filter(!(poly_id %in% c(2016, 2185)))

# filering out other counties with missing data
# Identify counties (poly_id) with any NA
counties_with_na_gdnat <- gdnat_data_79_04 %>%
    filter(if_any(everything(), is.na)) %>%
    distinct(poly_id)

# Remove all rows from those counties
gdnat_data_clean <- gdnat_data_79_04 %>%
    filter(!(poly_id %in% counties_with_na_gdnat$poly_id)) %>%
    # there are duplicates in my gdnat stagg output which needs to be fixed
    distinct()

# merging in county names to verify the keys...
# ... are accurate for merging later on
usa_shapefile_filtered <- usa_shapefile %>%
    mutate(GEOID_int = as.integer(GEOID)) %>%
    select(GEOID_int, NAME, NAMELSAD, ID_1, ID_2, geometry)

gdnat_processed <- gdnat_data_clean %>%
    mutate(poly_id_int = as.integer(poly_id)) %>%
    arrange(poly_id_int, year, month) %>%
    left_join(
        usa_shapefile_filtered,
        by = c("poly_id_int" = "GEOID_int")
    ) %>%
    mutate(
        date = as.Date(paste(year, month, "01", sep = "-")),
        days_in_month = lubridate::days_in_month(date)
    ) %>%
    mutate(
        order_1_avg = order_1 / days_in_month,
        order_2_avg = order_2 / days_in_month,
        order_3_avg = order_3 / days_in_month,
        order_4_avg = order_4 / days_in_month
    ) %>%
    select(
        year,
        month,
        poly_id_int,
        order_1_avg,
        order_2_avg,
        order_3_avg,
        order_4_avg,
        NAME,
        NAMELSAD
    )


#3. Cleaning ERA5 data
era5_data_79_04 <- era5_data %>%
    filter(year >= 1979 & year <= 2004) %>%
    # Now, filtering only for age = 0 and gender = 0...
    # Since these are the "total" cateogories
    filter(gender == 0 & agegroup == 0) %>%
    dplyr::select(-gender, -agegroup)

# filering out other counties with missing data
# Identify counties (fips) with any NA
counties_with_na_era5 <- era5_data_79_04 %>%
    filter(is.na(tavg_poly1_aw)) %>%
    distinct(adm2_id)

# Remove all rows from those counties
era5_data_clean <- era5_data_79_04 %>%
    filter(!(adm2_id %in% counties_with_na_era5$adm2_id))

# 4, Merging gdnat and era5 data
merged_data_panel <- left_join(
    era5_data_clean,
    gdnat_processed,
    by = c(
        "adm2_id" = "poly_id_int", # Match on the ID
        "month", # Match on the month column (assuming it's named 'month' in both)
        "year" # Match on the year column (assuming it's named 'year' in both)
    )
)

# removing places that don't have corresponding gdnat data
# this is likely due to population weighting as well as
# an un-corrected stagg function (check Maren's correction)

merged_nas <- merged_data_panel %>%
    filter(is.na(order_1_avg)) %>%
    distinct(adm2_id)

merged_data_panel_clean <- merged_data_panel %>%
    filter(!(adm2_id %in% merged_nas$adm2_id)) %>%
    rename(
        era5_poly1 = tavg_poly1_aw,
        era5_poly2 = tavg_poly2_aw,
        era5_poly3 = tavg_poly3_aw,
        era5_poly4 = tavg_poly4_aw,
        era5_poly5 = tavg_poly5_aw,
        gdnat_poly1 = order_1_avg,
        gdnat_poly2 = order_2_avg,
        gdnat_poly3 = order_3_avg,
        gdnat_poly4 = order_4_avg
    ) %>%
    mutate(
        diff_poly1 = era5_poly1 - gdnat_poly1,
        diff_poly2 = era5_poly2 - gdnat_poly2,
        diff_poly3 = era5_poly3 - gdnat_poly3,
        diff_poly4 = era5_poly4 - gdnat_poly4
    ) %>%
    # Generate 11 lags for each diff variable, grouped by county
    arrange(adm2_id, year, month) %>%
    group_by(adm2_id) %>%
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

panel_output_filename <- "merged_data_panel_extended_v2.csv"
panel_output_filepath <- file.path(output_path, panel_output_filename)

readr::write_csv(merged_data_panel_clean, panel_output_filepath)
