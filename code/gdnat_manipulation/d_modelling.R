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

install_if_missing("dplyr")
install_if_missing("lubridate")
install_if_missing("fixest")
install_if_missing("sf")
install_if_missing("readstata13")

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

shapefile_path <- st_read(era5_filepath)

# Define paths for world county shapefile.
usa_county_filename <- "gadm36_usa_counties.shp"
usa_county_path <- file.path(dir_path, usa_county_filename)
usa_shapefile <- vect(usa_county_path)

# Load the required datasets
stagg_results <- read.csv(results_file_path)
regression_betas <- read.csv(regression_beta_fp)
era5_results <- readstata13::read.dta13(era5_filepath)

# ----- II. Running NA checks # ------------

# 1. Checking for GDNAt Data
stagg_results_na <- stagg_results[!complete.cases(stagg_results), ]
# NAs found only in 2 counties: [1] "USA.10.44_1" "USA.24.39_1" - lake of the woods and monroe county

# dropping these *for now*
# !!! MUST be revisited

stagg_results_na_drop <- stagg_results[complete.cases(stagg_results), ]

#2. Checking for ERA5 Data
na_counts <- sapply(era5_results, function(x) sum(is.na(x)))
era5_results_na <- era5_results[is.na(era5_results$tavg_poly1_aw), ]

# Aleutian Islands has missing temperature data and there are several...
# ... rows with NA county names which will be dropped
# This must be verified

era5_results_na_drop <- era5_results[!is.na(era5_results$tavg_poly1_aw), ]

# ----- III. Running predictions for the models -------
# ----- Predicting for GDNat -------#

# Assume stagg_results has year, month, poly_id, order_1..order_4

# Step 1: Create a date column for easier lagging and days calculation
stagg_results_na_drop <- stagg_results_na_drop %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# Step 2: Create days in month
stagg_results_na_drop <- stagg_results_na_drop %>%
  mutate(days_in_month = lubridate::days_in_month(date))

# Step 3: divide total temp by nmber of days in each month
# create lag variables
stagg_results_lagged <- stagg_results_na_drop %>%
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

rename_vars <- expand.grid(order = 1:4, lag = 0:11) %>%
  mutate(
    old = paste0("order_", order, "_avg_lag", lag),
    new = paste0("tavg_poly", order, "_l", lag)
  )

names(stagg_results_lagged)[match(
  rename_vars$old,
  names(stagg_results_lagged)
)] <- rename_vars$new

# removing first 11 dates
stagg_results_lagged_clean <- stagg_results_lagged %>%
  arrange(poly_id, date) %>%
  group_by(poly_id) %>%
  slice(-(1:11)) %>%
  ungroup()

# Create named vector of regression betas using 'term' column
coef_vector <- setNames(regression_betas$beta, regression_betas$term)

# Extract predictor matrix from dataset using variable names from coef_vector
X <- stagg_results_lagged_clean[, names(coef_vector)] |> as.matrix()

# Compute predicted y_hat as matrix multiplication
stagg_results_lagged_clean$y_hat <- as.vector(X %*% coef_vector)

# ----- Predicting for ERA5 -------#

# Step 1: Create a date column for easier lagging and days calculation
era5_results_na_drop <- era5_results_na_drop %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# Step 2: Create days in month
era5_results_na_drop <- era5_results_na_drop %>%
  mutate(days_in_month = lubridate::days_in_month(date))

# Step 3: divide total temp by number of days in each month
# create lag variables
era5_results_lagged <- era5_results_na_drop %>%
  mutate(across(
    tavg_poly1_aw:tavg_poly4_aw,
    ~ . / days_in_month,
    .names = "{.col}_avg"
  )) %>%
  arrange(countyname, date) %>%
  group_by(countyname) %>%
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

# Create rename mapping for era5 variables
rename_vars <- expand.grid(order = 1:4, lag = 0:11) %>%
  mutate(
    old = paste0("tavg_poly", order, "_aw_avg_lag", lag),
    new = paste0("tavg_poly", order, "_l", lag)
  )

names(era5_results_lagged)[match(
  rename_vars$old,
  names(era5_results_lagged)
)] <- rename_vars$new

# removing first 11 dates
era5_results_lagged_clean <- era5_results_lagged %>%
  arrange(countyname, date) %>%
  group_by(countyname) %>%
  slice(-(1:11)) %>%
  ungroup()

# Create named vector of regression betas using 'term' column
coef_vector <- setNames(regression_betas$beta, regression_betas$term)

# Extract predictor matrix from dataset using variable names from coef_vector
X <- era5_results_lagged_clean[, names(coef_vector)] |> as.matrix()

# Compute predicted y_hat as matrix multiplication
era5_results_lagged_clean$y_hat <- as.vector(X %*% coef_vector)

# Add GID2 for consistency


# ----- Sense checks - plotting -------#
