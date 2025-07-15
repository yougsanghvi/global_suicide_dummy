# ----- Refactored: Early Merge and Difference Regression -----
# This script merges GDNat and ERA5 data early, computes lagged differences, and runs regression on those differences.

# Load libraries
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", ...)
  }
  library(pkg, character.only = TRUE)
}

install_if_missing("dplyr")
install_if_missing("lubridate")
install_if_missing("fixest")
install_if_missing("readr")
install_if_missing("crayon")

# Colored print helper
green <- crayon::green
yellow <- crayon::yellow
blue <- crayon::blue
red <- crayon::red

cat(blue("[Step 1] Libraries loaded.\n"))

# Set file paths
# ...existing code for file paths...
dir_path <- "/global/scratch/users/yougsanghvi"
results_folder <- "aggregated_results_gdnat_usa"
results_file_name <- "gdnat_usa_agg_all_years.csv"
results_file_path <- file.path(dir_path, results_folder, results_file_name)
era5_folderpath <- file.path("merged", "USA")
era5_filename <- "USA_adm2_1968_2004_monthly.dta"
era5_filepath <- file.path(dir_path, era5_folderpath, era5_filename)
output_path <- file.path(dir_path, "gdnat_era5_compare_output")

cat(blue("[Step 2] File paths set.\n"))

# Load data
cat(crayon::yellow("Loading GDNat and ERA5 data...\n"))
stagg_results <- read.csv(results_file_path)
era5_results <- readstata13::read.dta13(era5_filepath)

cat(blue("[Step 3] Loaded GDNat and ERA5 data.\n"))

# Load county shapefile for metadata
cat(crayon::yellow("Loading county shapefile metadata...\n"))
library(sf)
usa_county_dir <- file.path(dir_path, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)
usa_shapefile <- st_read(usa_county_path)

cat(blue("[Step 4] Loaded county shapefile metadata.\n"))

# Clean GDNat data
cat(crayon::blue("Cleaning GDNat data...\n"))
gdnat <- stagg_results %>% distinct() %>% filter(complete.cases(.)) %>% mutate(poly_id_int = as.integer(poly_id))

cat(green("[Step 5] Cleaned GDNat data.\n"))

# Add county metadata to GDNat
cat(crayon::blue("Adding county metadata to GDNat...\n"))
usa_shapefile_filtered <- usa_shapefile %>%
  mutate(GEOID_int = as.integer(GEOID)) %>%
  select(GEOID_int, NAME, NAMELSAD, ID_1, ID_2)
gdnat <- gdnat %>%
  left_join(usa_shapefile_filtered, by = c("poly_id_int" = "GEOID_int"))

cat(green("[Step 6] Added county metadata to GDNat.\n"))

# Clean ERA5 data
cat(crayon::blue("Cleaning ERA5 data...\n"))
era5 <- era5_results %>% filter(!is.na(tavg_poly1_aw), gender == 0, agegroup == 0) %>% select(-gender, -agegroup) %>% mutate(adm2_id_int = as.integer(adm2_id))

cat(green("[Step 7] Cleaned ERA5 data.\n"))

# Merge early on county, year, month
cat(crayon::magenta("Merging GDNat and ERA5 data on county, year, month...\n"))
merged_data <- inner_join(
  gdnat,
  era5,
  by = c("poly_id_int" = "adm2_id_int", "year", "month")
)

cat(yellow("[Step 8] Merged GDNat and ERA5 data.\n"))

# Feature engineering: create lagged features and compute differences
cat(crayon::cyan("Creating lagged features and computing differences...\n"))
merged_data <- merged_data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         days_in_month = lubridate::days_in_month(date))

cat(yellow("[Step 9] Added date and days_in_month features.\n"))

# Example: create lagged features for order_1_avg (GDNat) and tavg_poly1_aw (ERA5)
merged_data <- merged_data %>%
  mutate(order_1_avg_gdnat = order_1 / days_in_month,
         tavg_poly1_aw_avg_era5 = tavg_poly1_aw / days_in_month)

cat(yellow("[Step 10] Created normalized temperature features.\n"))

for (lag in 0:11) {
  merged_data[[paste0("order_1_avg_gdnat_lag", lag)]] <- dplyr::lag(merged_data$order_1_avg_gdnat, lag)
  merged_data[[paste0("tavg_poly1_aw_avg_era5_lag", lag)]] <- dplyr::lag(merged_data$tavg_poly1_aw_avg_era5, lag)
  merged_data[[paste0("diff_avg_temp_lag", lag)]] <- merged_data[[paste0("tavg_poly1_aw_avg_era5_lag", lag)]] - merged_data[[paste0("order_1_avg_gdnat_lag", lag)]]
}
for (poly in 1:4) {
  for (lag in 0:11) {
    # Construct regression feature name, e.g. tavg_poly1_l0
    reg_name <- paste0("tavg_poly", poly, "_l", lag)
    # Find corresponding column in merged_data
    era5_col <- paste0("tavg_poly", poly, "_aw_lag", lag)
    # If column exists, copy to regression name
    if (era5_col %in% colnames(merged_data)) {
      merged_data[[reg_name]] <- merged_data[[era5_col]]
    } else if (poly == 1 && (paste0("tavg_poly1_aw_avg_era5_lag", lag) %in% colnames(merged_data))) {
      # For poly1, handle special case for avg column
      merged_data[[reg_name]] <- merged_data[[paste0("tavg_poly1_aw_avg_era5_lag", lag)]]
    }
  }
}
cat(yellow("[Step 11b] Renamed ERA5 lagged columns to match regression file features.\n"))

cat(yellow("[Step 11] Computed lagged features and differences.\n"))

# Remove first 11 rows per county (due to lagging)
cat(crayon::cyan("Removing first 11 rows per county due to lagging...\n"))
merged_data <- merged_data %>%
  arrange(poly_id_int, date) %>%
  group_by(poly_id_int) %>%
  slice(-(1:11)) %>%
  ungroup()

cat(yellow("[Step 12] Removed first 11 rows per county due to lagging.\n"))

# Step 13: Run predictions using external regression coefficients
cat(crayon::green("Loading regression coefficients and running predictions...\n"))
regression_file <- "/global/scratch/users/yougsanghvi/regression_coefficients_USA_poly4_lag11.csv"
betas <- read.csv(regression_file)
cat(blue("Loaded regression coefficients.\n"))

# Filter betas to only temperature polynomial terms (tavg columns)
betas_tavg <- betas[grepl("^tavg", betas$term), ]
cat(blue("Filtered regression coefficients to temperature polynomial terms.\n"))

# Prepare feature matrix for prediction
feature_names <- betas_tavg$term
# Ensure all required features exist in merged_data
missing_features <- setdiff(feature_names, colnames(merged_data))
if (length(missing_features) > 0) {
  cat(red(paste("Missing features in merged_data:", paste(missing_features, collapse=", "), "\n")))
}

# Prediction: y_hat_diff = sum_i (beta_i * X_i)
merged_data$y_hat_diff <- 0
for (i in seq_along(feature_names)) {
  term <- feature_names[i]
  beta <- betas_tavg$estimate[i]
  if (term %in% colnames(merged_data)) {
    merged_data$y_hat_diff <- merged_data$y_hat_diff + beta * merged_data[[term]]
  }
}
cat(green("[Step 13] Predictions computed using external regression coefficients.\n"))


# Save output with county metadata and predictions
cat(crayon::green("Saving output with county metadata and lagged difference features...\n"))
panel_output_filename <- "merged_data_panel_differences.csv"
panel_output_filepath <- file.path(output_path, panel_output_filename)
readr::write_csv(merged_data %>% select(year, month, NAME, NAMELSAD, ID_1, ID_2, everything()), panel_output_filepath)

cat(green("Saved merged panel with predictions to output directory.\n"))
cat(green("[Step 14] Output saved to output directory.\n"))
