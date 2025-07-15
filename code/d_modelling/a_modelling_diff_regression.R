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

cat(blue("Loading data files...\n"))

# Load data
stagg_results <- read.csv(results_file_path)
era5_results <- readstata13::read.dta13(era5_filepath)

# Load county shapefile for metadata
library(sf)
usa_county_dir <- file.path(dir_path, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)
usa_shapefile <- st_read(usa_county_path)

cat(green("Cleaning and preparing GDNat and ERA5 data...\n"))

# Clean GDNat data
gdnat <- stagg_results %>% distinct() %>% filter(complete.cases(.)) %>% mutate(poly_id_int = as.integer(poly_id))

# Add county metadata to GDNat
usa_shapefile_filtered <- usa_shapefile %>%
  mutate(GEOID_int = as.integer(GEOID)) %>%
  select(GEOID_int, NAME, NAMELSAD, ID_1, ID_2)
gdnat <- gdnat %>%
  left_join(usa_shapefile_filtered, by = c("poly_id_int" = "GEOID_int"))

cat(blue("Merging GDNat and ERA5 datasets...\n"))

# Clean ERA5 data
era5 <- era5_results %>% filter(!is.na(tavg_poly1_aw), gender == 0, agegroup == 0) %>% select(-gender, -agegroup) %>% mutate(adm2_id_int = as.integer(adm2_id))

# Merge early on county, year, month
merged_data <- inner_join(
  gdnat,
  era5,
  by = c("poly_id_int" = "adm2_id_int", "year", "month")
)

# Output: percent of unique counties in ERA5 matched by GDNat
era5_unique_counties <- length(unique(era5$adm2_id_int))
matched_counties <- length(unique(merged_data$poly_id_int))
percent_matched <- round(100 * matched_counties / era5_unique_counties, 2)
cat(yellow(paste0("Matched ", matched_counties, " of ", era5_unique_counties, " unique counties in ERA5 (", percent_matched, "%)\n")))

cat(green("Feature engineering: creating lagged features and computing differences...\n"))

# Feature engineering: create lagged features and compute differences
merged_data <- merged_data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         days_in_month = lubridate::days_in_month(date))

# Example: create lagged features for order_1_avg (GDNat) and tavg_poly1_aw (ERA5)
merged_data <- merged_data %>%
  mutate(order_1_avg_gdnat = order_1 / days_in_month,
         tavg_poly1_aw_avg_era5 = tavg_poly1_aw / days_in_month)

for (lag in 0:11) {
  merged_data[[paste0("order_1_avg_gdnat_lag", lag)]] <- dplyr::lag(merged_data$order_1_avg_gdnat, lag)
  merged_data[[paste0("tavg_poly1_aw_avg_era5_lag", lag)]] <- dplyr::lag(merged_data$tavg_poly1_aw_avg_era5, lag)
  merged_data[[paste0("diff_avg_temp_lag", lag)]] <- merged_data[[paste0("tavg_poly1_aw_avg_era5_lag", lag)]] - merged_data[[paste0("order_1_avg_gdnat_lag", lag)]]
}

cat(blue("Filtering out first 11 rows per county due to lagging...\n"))

# Remove first 11 rows per county (due to lagging)
merged_data <- merged_data %>%
  arrange(poly_id_int, date) %>%
  group_by(poly_id_int) %>%
  slice(-(1:11)) %>%
  ungroup()

# Load regression betas
regression_beta_fn <- "regression_coefficients_USA_poly4_lag11.csv"
regression_beta_fp <- file.path(dir_path, regression_beta_fn)

regression_betas <- read.csv(regression_beta_fp)
# Filter to only tavg columns
regression_betas <- regression_betas[grepl("^tavg", regression_betas$term), ]

# Prepare regression betas
coef_vector <- setNames(regression_betas$beta, regression_betas$term)

# Create lagged polynomial features for difference prediction
for (order in 1:4) {
  for (lag in 0:11) {
    merged_data[[paste0("tavg_poly", order, "_diff_lag", lag)]] <- dplyr::lag(
      (merged_data[[paste0("tavg_poly", order, "_aw")]] / merged_data$days_in_month) -
      (merged_data[[paste0("order_", order)]] / merged_data$days_in_month), lag)
  }
}

# Compute y_hat_diff using difference lagged features
diff_feature_names <- names(coef_vector)
X_diff <- merged_data[, paste0("tavg_poly", rep(1:4, each=12), "_diff_lag", rep(0:11, 4))]
colnames(X_diff) <- diff_feature_names
merged_data$y_hat_diff <- as.vector(as.matrix(X_diff) %*% coef_vector)

# Regression on differences
reg_terms <- paste0("diff_avg_temp_lag", 0:11)
reg_formula <- paste("suiciderate ~", paste(reg_terms, collapse = " + "))
regression_result <- fixest::feols(as.formula(reg_formula), data = merged_data)
summary(regression_result)

cat(magenta("Running regression on lagged differences...\n"))

# Save output

# Save output with county metadata
panel_output_filename <- "merged_data_panel_differences.csv"
panel_output_filepath <- file.path(output_path, panel_output_filename)
readr::write_csv(merged_data %>% select(year, month, NAME, NAMELSAD, ID_1, ID_2, everything()), panel_output_filepath)

cat(green("Saved merged panel and regression summary to output directory.\n"))

# Optionally save regression summary
regression_summary_filename <- "regression_differences_summary.txt"
regression_summary_filepath <- file.path(output_path, regression_summary_filename)
writeLines(capture.output(summary(regression_result)), regression_summary_filepath)
