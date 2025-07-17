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
stagg_results <- read.csv(results_file_path)
regression_betas <- read.csv(regression_beta_fp)
era5_results <- readstata13::read.dta13(era5_filepath)
usa_shapefile <- st_read(usa_county_path)
geocode_file <- read.csv(geocode_filepath)

# ----- II. Data Cleaning # ------------
cat(magenta("cleaning data \n"))

# Filter to only temperature polynomial terms
regression_betas <- regression_betas %>%
  filter(grepl("^tavg", term))

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
era5_results_na <- era5_results[is.na(era5_results$tavg_poly1_aw), ]

# Aleutian Islands has missing temperature data and there are several...
# ... rows with NA county names which will be dropped
# This must be verified

era5_results_na_drop <- era5_results[!is.na(era5_results$tavg_poly1_aw), ]

# Now, filtering only for age = 0 and gender = 0...
# Since these are the "total" cateogories

era5_results_na_drop <- era5_results_na_drop %>%
  filter(gender == 0 & agegroup == 0)

era5_results_na_drop <- era5_results_na_drop %>%
  dplyr::select(-gender, -agegroup)

# ----- III. Running predictions for the models -------
# ----- Predicting for GDNat -------#

cat(green("creating predicted suicide rate \n"))
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
stagg_results_lagged_clean$y_hat_gdnat <- as.vector(X %*% coef_vector)

# ----- Predicting for ERA5 -------#

# Step 1: Create a date column for easier lagging and days calculation
era5_results_na_drop <- era5_results_na_drop %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# Step 3: divide total temp by number of days in each month
# create lag variables
era5_results_lagged <- era5_results_na_drop %>%
  arrange(countyname, date) %>%
  group_by(countyname) %>%
  mutate(across(
    starts_with("tavg"),
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
    .names = "{.col}_l{.fn}"
  )) %>%
  ungroup()

era5_results_lagged <- era5_results_lagged %>%
  select(
    year,
    month,
    num_of_suicide,
    suiciderate,
    adm2_id,
    statename,
    countyname,
    starts_with("tavg"),
    date
  )

# removing first 11 dates
era5_results_lagged_clean <- era5_results_lagged %>%
  arrange(countyname, date) %>%
  group_by(countyname) %>%
  slice(-(1:11)) %>%
  ungroup()

# Create rename mapping for era5 variables
rename_vars <- expand.grid(order = 1:5, lag = 0:11) %>%
  mutate(
    old = paste0("tavg_poly", order, "_aw_l", lag),
    new = paste0("tavg_poly", order, "_l", lag)
  )

names(era5_results_lagged_clean)[match(
  rename_vars$old,
  names(era5_results_lagged_clean)
)] <- rename_vars$new

# Create named vector of regression betas using 'term' column
coef_vector <- setNames(regression_betas$beta, regression_betas$term)

# Extract predictor matrix from dataset using variable names from coef_vector
X <- era5_results_lagged_clean[, names(coef_vector)] |> as.matrix()

# Compute predicted y_hat as matrix multiplication
era5_results_lagged_clean$y_hat_era5 <- as.vector(X %*% coef_vector)

# ----- IV. Merging Predictions -------

cat(yellow("merging and saving the final dataset \n"))
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
    order_1_avg
  )  %>%
  rename(gdnat_avg_temp = order_1_avg)

era5_final <- era5_results_lagged_clean %>%
  rename(era5_avg_temp = tavg_poly1_aw) %>%
  select(-starts_with("tavg")) %>%
  filter(year > 1979) # we only have gdnat data post 1979

merged_data_panel <- left_join(
  era5_final,
  gdnat_final,
  by = c(
    "adm2_id" = "poly_id_int", # Match on the ID
    "month", # Match on the month column (assuming it's named 'month' in both)
    "year" # Match on the year column (assuming it's named 'year' in both)
  )
)

# create weather delta columns
merged_data_panel <- merged_data_panel %>%
  mutate(
    temp_diff = era5_avg_temp - gdnat_avg_temp)

panel_output_filename <- "merged_data_panel_extended.csv"
panel_output_filepath <- file.path(output_path, panel_output_filename)

readr::write_csv(merged_data_panel, panel_output_filepath)


# ----- Early Merge and Difference Regression (Refactored) -----
cat(green("merging cleaned datasets early and computing differences for regression\n"))

# Prepare keys for merging
gdnat_final <- gdnat_final %>% mutate(poly_id_int = as.integer(poly_id_int))
era5_final <- era5_final %>% mutate(adm2_id_int = as.integer(adm2_id))

# Merge on county, year, month
merged_data <- inner_join(
  gdnat_final,
  era5_final,
  by = c("poly_id_int" = "adm2_id_int", "year", "month")
)

# Feature engineering: create lagged features and compute differences
merged_data <- merged_data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         days_in_month = lubridate::days_in_month(date))

# Example: create lagged features for gdnat_avg_temp and era5_avg_temp
for (lag in 0:11) {
  merged_data[[paste0("gdnat_avg_temp_lag", lag)]] <- dplyr::lag(merged_data$gdnat_avg_temp, lag)
  merged_data[[paste0("era5_avg_temp_lag", lag)]] <- dplyr::lag(merged_data$era5_avg_temp, lag)
  merged_data[[paste0("diff_avg_temp_lag", lag)]] <- merged_data[[paste0("era5_avg_temp_lag", lag)]] - merged_data[[paste0("gdnat_avg_temp_lag", lag)]]
}

# Remove first 11 rows per county (due to lagging)
merged_data <- merged_data %>%
  arrange(poly_id_int, date) %>%
  group_by(poly_id_int) %>%
  slice(-(1:11)) %>%
  ungroup()

# Regression on differences
cat(yellow("running regression on differences\n"))
reg_terms <- paste0("diff_avg_temp_lag", 0:11)
reg_formula <- paste("suiciderate ~", paste(reg_terms, collapse = " + "))
regression_result <- fixest::feols(as.formula(reg_formula), data = merged_data)
summary(regression_result)

# Save output
panel_output_filename <- "merged_data_panel_differences.csv"
panel_output_filepath <- file.path(output_path, panel_output_filename)
readr::write_csv(merged_data, panel_output_filepath)

# Optionally save regression summary
regression_summary_filename <- "regression_differences_summary.txt"
regression_summary_filepath <- file.path(output_path, regression_summary_filename)
writeLines(capture.output(summary(regression_result)), regression_summary_filepath)