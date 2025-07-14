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
regression_betas <- read.csv(regression_beta_fp)