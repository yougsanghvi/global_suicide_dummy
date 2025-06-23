# Load necessary libraries 

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

# Set file paths 
dir_path <- "/global/scratch/users/yougsanghvi"

results_folder <- "aggregated_results_gdnat_usa"
results_file_name <- "gdnat_usa_agg_all_years.csv"
results_file_path <- file.path(dir_path, results_folder, results_file_name)

regression_beta_fn <- "regression_coefficients_USA_poly4_lag11.csv"
regression_beta_fp <- file.path(dir_path, regression_beta_fn)

era5_folderpath <- "tavg"
era5_filename <- "temp_polynomial_Adm0_USA_ERA5_pop_weights"
era5_filepath <- file.path(dir_path, era5_folderpath, era5_filename)

# Load the required datasets
stagg_results <- read.csv(results_file_path)
regression_betas <- read.csv(regression_beta_fp)
era5_results <- read.csv(era5_filepath)

# ----- Running NA checks # ------------

stagg_results_na <- stagg_results[!complete.cases(stagg_results), ]
# NAs found only in 2 counties: [1] "USA.10.44_1" "USA.24.39_1" - lake of the woods and monroe county 

# dropping these *for now*
# !!! MUST be revisited 

stagg_results_na_drop <- stagg_results[complete.cases(stagg_results), ]

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
  mutate(across(order_1:order_4, ~ . / days_in_month, .names = "{.col}_avg")) %>%
  arrange(poly_id, date) %>%
  group_by(poly_id) %>%
  mutate(across(ends_with("_avg"), 
                list(`0` = ~., 
                     `1` = ~lag(., 1), `2` = ~lag(., 2), `3` = ~lag(., 3),
                     `4` = ~lag(., 4), `5` = ~lag(., 5), `6` = ~lag(., 6),
                     `7` = ~lag(., 7), `8` = ~lag(., 8), `9` = ~lag(., 9),
                     `10` = ~lag(., 10), `11` = ~lag(., 11)),
                .names = "{.col}_lag{.fn}")) %>%
  ungroup()

rename_vars <- expand.grid(order = 1:4, lag = 0:11) %>%
  mutate(old = paste0("order_", order, "_avg_lag", lag),
        new = paste0("tavg_poly", order, "_l", lag))

names(stagg_results_lagged)[match(rename_vars$old, names(stagg_results_lagged))] <- rename_vars$new

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

era5_results_lagged <- stagg_results_na_drop %>%
  mutate(across(order_1:order_4, ~ . / days_in_month, .names = "{.col}_avg")) %>%
  arrange(poly_id, date) %>%
  group_by(poly_id) %>%
  mutate(across(ends_with("_avg"), 
                list(`0` = ~., 
                     `1` = ~lag(., 1), `2` = ~lag(., 2), `3` = ~lag(., 3),
                     `4` = ~lag(., 4), `5` = ~lag(., 5), `6` = ~lag(., 6),
                     `7` = ~lag(., 7), `8` = ~lag(., 8), `9` = ~lag(., 9),
                     `10` = ~lag(., 10), `11` = ~lag(., 11)),
                .names = "{.col}_lag{.fn}")) %>%
  ungroup()

rename_vars <- expand.grid(order = 1:4, lag = 0:11) %>%
  mutate(old = paste0("order_", order, "_avg_lag", lag),
        new = paste0("tavg_poly", order, "_l", lag))

names(stagg_results_lagged)[match(rename_vars$old, names(stagg_results_lagged))] <- rename_vars$new

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

# ----- Sense checks - plotting -------#

library(dplyr)
library(ggplot2)

# Step 1: Add 'year' column if needed
stagg_results_lagged_clean <- stagg_results_lagged_clean %>%
  mutate(year = lubridate::year(date))

# Step 2: Calculate annual average y_hat across all counties
yhat_annual <- stagg_results_lagged_clean %>%
  group_by(year) %>%
  summarise(yhat_avg = mean(y_hat, na.rm = TRUE), .groups = "drop")

# Step 3: Plot
plot_yhat_annual <- ggplot(yhat_annual, aes(x = year, y = yhat_avg)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Annual Average Predicted Suicide Rate Across USA Counties",
       x = "Year",
       y = "Average Predicted Suicide Rate (y_hat)") +
  theme_minimal()

print(plot_yhat_annual)

# Step 4: Save
ggsave("yhat_annual_trend.png", plot = plot_yhat_annual, width = 8, height = 5, dpi = 300)
