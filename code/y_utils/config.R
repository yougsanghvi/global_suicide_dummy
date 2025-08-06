# Base scratch directory
SCRATCH <- "/global/scratch/users/yougsanghvi"

# Shapefile paths
SHAPEFILE_DIR <- file.path(SCRATCH, "shapefiles")
USA_COUNTY_SHAPEFILE_FP <- file.path(SHAPEFILE_DIR, "tl_2016_us_county_mortality.shp")

# Attribution outputs
ATTRIBUTION_OUTPUT_DIR <- file.path(SCRATCH, "gdnat_era5_compare_output")
ATTRIBUTION_V1_FP <- file.path(ATTRIBUTION_OUTPUT_DIR, "merged_data_panel_extended.csv")
ATTRIBUTION_V2_FP <- file.path(ATTRIBUTION_OUTPUT_DIR, "merged_data_panel_extended_v2.csv")

# Regression beta
REGRESSION_BETA_FP <- file.path(SCRATCH, "regression_coefficients_USA_poly4_lag11.csv")

# Suicide panel
SUICIDE_PROJECT_FOLDER <- file.path(SCRATCH, "data", "merged", "USA")
SUICIDE_PANEL_FP <- file.path(SUICIDE_PROJECT_FOLDER, "USA_adm2_1968_2004_monthly.dta")

# GDNAT aggregated
STAGG_FOLDER_PATH <- file.path(SCRATCH, "aggregated_results_era5_usa")
GDNAT_COUNTY_FP <- file.path(STAGG_FOLDER_PATH, "gdnat_usa_agg_all_years.csv")

get_gdnat_agg_yearly <- function(year) {
  file.path(STAGG_FOLDER_PATH, sprintf("gdnat_usa_agg_%d.csv", year))
}

get_era5_agg_yearly <- function(year) {
  file.path(STAGG_FOLDER_PATH, sprintf("era5_usa_agg_%d.csv", year))
}

# Raw GDNAT TIFF
RAW_GDNAT_FOLDER <- file.path(SCRATCH, "gdnat_tiff_files_by_yr")
get_gdnat_raw_yearly <- function(year) {
  file.path(RAW_GDNAT_FOLDER, sprintf("gdnat_%d.tif", year))
}

# Raw GDNAT Zarr
GDNAT_ZARR_DIR <- file.path(SCRATCH, "global_suicide")
GDNAT_ZARR_1979_1999_FP <- file.path(GDNAT_ZARR_DIR, "gdnat_ACCESS-CM2_tas_1979-1999_v2025-02-11.zarr")
GDNAT_ZARR_PATH_2000_2020_FP <- file.path(GDNAT_ZARR_DIR, "gdnat_ACCESS-CM2_tas_2000-2020_v2025-02-11.zarr")

# Degree day file (Maren)
ERA5_DD_FP <- file.path(SCRATCH, "data", "climatedata", "USA", "temp_degreedays_1986_1999_USA_ERA5_pop_weights.csv")

# ERA5 raw
ERA5_RAW_FOLDER <- file.path(SCRATCH, "era5_hourly_by_year")
get_era5_raw_yearly <- function(year) {
  file.path(ERA5_RAW_FOLDER, sprintf("era5_data_%d.grib", year))
}

# ERA5 daily aggregated
ERA5_DAILY_FOLDER <- file.path(SCRATCH, "era5_daily_by_year")
get_era5_daily_yearly <- function(year) {
  file.path(ERA5_DAILY_FOLDER, sprintf("era5_daily_mean_%d.nc", year))
}

# ERA5 county aggregated
ERA5_COUNTY_AREA_FP <- file.path(SCRATCH, "data", "climatedata", "USA", "usa_area_era5_temp_average_1968_2004_polynomial_5_area_crop_weights.csv")



# === CONDO PATH ===
CONDO_PATH <- "/global/scratch/projects/co_carleton/carleton_colab"
CONDO_DATA_DIR <- file.path(CONDO_PATH, "data")


# === ERA5 AGGREGATED PATHS ===
ERA5_AGG_FOLDER <- file.path(CONDO_DATA_DIR, "era5_t2m","aggregated", "usa_pop_county")

# === Raw GDNat Paths ===
GDNAT_DIR <- file.path(CONDO_DATA_DIR, "gdnat_allmodels_Jul24")
GDNAT_TIFF_DIR <- file.path(GDNAT_DIR, "tiff_by_yr_model")
GDNAT_DATA_FP <- file.path(GDNAT_DIR, "gdnat_tas_1979-2020_v2025-02-11.zarr")

GDNAT_TIFF_OUTPUT_DIR <- file.path(GDNAT_DIR, "tiff_by_yr_model")

# Aggregated GDNAT paths
GDNAT_USA_AGG_ALL_MODELS_FP <- file.path(GDNAT_DIR, "aggregated", "usa_pop_county_na_ignore")

get_gdnat_agg_yearly <- function(year, model) {
  file.path(GDNAT_USA_AGG_ALL_MODELS_FP, model, sprintf("gdnat_usa_agg_%s_%d.csv", model, year))
}

# project folder
PROJECT_FOLDER <- file.path(CONDO_PATH, "projects", "global_suicide")

# current trend attribution output
CURRENT_TREND_ATTR <- file.path(
    PROJECT_FOLDER, "merged_data_panel_extended_v3.csv")
