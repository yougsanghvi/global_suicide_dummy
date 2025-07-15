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

regression_betas <- read.csv(regression_beta_fp)
usa_shapefile <- st_read(usa_county_path)
geocode_file <- read.csv(geocode_filepath)

# 1. Load datasets as before

stagg_results <- read.csv(results_file_path) %>%
  distinct() %>%
  filter(complete.cases(.)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         days_in_month = lubridate::days_in_month(date)) %>%
  mutate(across(order_1:order_4, ~ . / days_in_month, .names = "{.col}_avg")) %>%
  select(year, month, poly_id, date, days_in_month, starts_with("order_"), starts_with("order_"), everything())

era5_results <- readstata13::read.dta13(era5_filepath) %>%
  filter(gender == 0 & agegroup == 0) %>%
  filter(!is.na(tavg_poly1_aw)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         days_in_month = lubridate::days_in_month(date))

# 2. Rename GDNat temp polynomial columns to match ERA5 naming
#    For merging convenience, rename order_1_avg, order_2_avg,... as tavg_poly1_aw, tavg_poly2_aw, ...
stagg_results <- stagg_results %>%
  rename(
    tavg_poly1_aw = order_1_avg,
    tavg_poly2_aw = order_2_avg,
    tavg_poly3_aw = order_3_avg,
    tavg_poly4_aw = order_4_avg
  )

# 3. Prepare IDs for merge
# Convert GDNat poly_id to integer for matching with ERA5 adm2_id
stagg_results <- stagg_results %>%
  mutate(poly_id_int = as.integer(poly_id))

era5_results <- era5_results %>%
  rename(adm2_id = adm2_id) # just to be explicit, make sure column is adm2_id

# 4. Select relevant columns for merge
gdnat_select <- stagg_results %>%
  select(year, month, poly_id_int, date, starts_with("tavg_poly"))

era5_select <- era5_results %>%
  select(year, month, adm2_id, date, starts_with("tavg_poly"))

# 5. Merge early on year, month, and county ID
merged_climate <- inner_join(
  era5_select,
  gdnat_select,
  by = c("year", "month", "date", "adm2_id" = "poly_id_int"),
  suffix = c("_era5", "_gdnat")
)

# 6. Compute differences for each temperature polynomial variable
poly_orders <- 1:4
for (order in poly_orders) {
  era5_col <- paste0("tavg_poly", order, "_aw_era5")
  gdnat_col <- paste0("tavg_poly", order, "_aw_gdnat")
  diff_col <- paste0("tavg_poly", order, "_diff")
  merged_climate[[diff_col]] <- merged_climate[[era5_col]] - merged_climate[[gdnat_col]]
}

# 7. Create lagged variables for each difference variable (lags 0 to 11)
library(tidyr)

merged_climate <- merged_climate %>%
  arrange(adm2_id, date) %>%
  group_by(adm2_id) %>%
  mutate(across(
    all_of(paste0("tavg_poly", poly_orders, "_diff")),
    list(
      `l0` = ~ .,
      `l1` = ~ lag(., 1),
      `l2` = ~ lag(., 2),
      `l3` = ~ lag(., 3),
      `l4` = ~ lag(., 4),
      `l5` = ~ lag(., 5),
      `l6` = ~ lag(., 6),
      `l7` = ~ lag(., 7),
      `l8` = ~ lag(., 8),
      `l9` = ~ lag(., 9),
      `l10` = ~ lag(., 10),
      `l11` = ~ lag(., 11)
    ),
    .names = "{.col}_l{str_remove(.fn, 'l')}"
  )) %>%
  ungroup()

# 8. Remove first 11 months of data per county (due to lagging)
merged_climate_clean <- merged_climate %>%
  group_by(adm2_id) %>%
  slice(-(1:11)) %>%
  ungroup()

# 9. Rename lagged difference variables to match regression terms
# e.g. tavg_poly1_diff_l0 => tavg_poly1_l0, tavg_poly2_diff_l3 => tavg_poly2_l3, etc.
rename_map <- expand.grid(order = poly_orders, lag = 0:11) %>%
  mutate(
    old = paste0("tavg_poly", order, "_diff_l", lag),
    new = paste0("tavg_poly", order, "_l", lag)
  )
names(merged_climate_clean)[match(rename_map$old, names(merged_climate_clean))] <- rename_map$new

# 10. Filter regression betas to tavg terms only (if not done yet)
regression_betas <- regression_betas %>% filter(grepl("^tavg", term))
coef_vector <- setNames(regression_betas$beta, regression_betas$term)

# 11. Extract predictor matrix (lagged diff variables) for prediction
X_diff <- merged_climate_clean[, names(coef_vector)] |> as.matrix()

# 12. Compute predicted difference in suicide rate
merged_climate_clean$y_hat_diff <- as.vector(X_diff %*% coef_vector)

# 13. (Optional) Add metadata like year, month, county IDs for output
output_df <- merged_climate_clean %>%
  select(year, month, adm2_id, y_hat_diff)

# 14. Save the output
output_filepath <- file.path(output_path, "suicide_rate_predicted_diff.csv")
readr::write_csv(output_df, output_filepath)

cat("Prediction of suicide rate differences complete, output saved.\n")
