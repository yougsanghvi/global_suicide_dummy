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

# Load data
stagg_results <- read.csv(results_file_path)
era5_results <- readstata13::read.dta13(era5_filepath)

# Load county shapefile for metadata
library(sf)
usa_county_dir <- file.path(dir_path, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)
usa_shapefile <- st_read(usa_county_path)

# Clean GDNat data
gdnat <- stagg_results %>% distinct() %>% filter(complete.cases(.)) %>% mutate(poly_id_int = as.integer(poly_id))

# Add county metadata to GDNat
usa_shapefile_filtered <- usa_shapefile %>%
  mutate(GEOID_int = as.integer(GEOID)) %>%
  select(GEOID_int, NAME, NAMELSAD, ID_1, ID_2)
gdnat <- gdnat %>%
  left_join(usa_shapefile_filtered, by = c("poly_id_int" = "GEOID_int"))

# Clean ERA5 data
era5 <- era5_results %>% filter(!is.na(tavg_poly1_aw), gender == 0, agegroup == 0) %>% select(-gender, -agegroup) %>% mutate(adm2_id_int = as.integer(adm2_id))

# Merge early on county, year, month
merged_data <- inner_join(
  gdnat,
  era5,
  by = c("poly_id_int" = "adm2_id_int", "year", "month")
)

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

# Remove first 11 rows per county (due to lagging)
merged_data <- merged_data %>%
  arrange(poly_id_int, date) %>%
  group_by(poly_id_int) %>%
  slice(-(1:11)) %>%
  ungroup()


# Save output with county metadata and predictions
panel_output_filename <- "merged_data_panel_differences.csv"
panel_output_filepath <- file.path(output_path, panel_output_filename)
readr::write_csv(merged_data %>% select(year, month, NAME, NAMELSAD, ID_1, ID_2, everything()), panel_output_filepath)

cat(green("Saved merged panel with predictions to output directory.\n"))
