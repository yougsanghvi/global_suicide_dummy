import os

# === BASE DIRECTORY ===
SCRATCH = "/global/scratch/users/yougsanghvi"

# === SHAPEFILE PATHS ===
SHAPEFILE_DIR = os.path.join(SCRATCH, "shapefiles")
USA_COUNTY_FILENAME = "tl_2016_us_county_mortality.shp"
USA_COUNTY_SHAPEFILE_FP = os.path.join(SHAPEFILE_DIR, USA_COUNTY_FILENAME)

# === ATTRIBUTION OUTPUT ===
ATTRIBUTION_OUTPUT_DIR = os.path.join(SCRATCH, "gdnat_era5_compare_output")
ATTRIBUTION_OUTPUT_FILENAME_V1 = "merged_data_panel_extended.csv"
ATTRIBUTION_OUTPUT_FILENAME_V2 = "merged_data_panel_extended_v2.csv"

ATTRIBUTION_V1_FP = os.path.join(
    ATTRIBUTION_OUTPUT_DIR, ATTRIBUTION_OUTPUT_FILENAME_V1
)
ATTRIBUTION_V2_FP = os.path.join(
    ATTRIBUTION_OUTPUT_DIR, ATTRIBUTION_OUTPUT_FILENAME_V2
)

# === REGRESSION COEFFICIENTS ===
REGRESSION_BETA_FILENAME = "regression_coefficients_USA_poly4_lag11.csv"
REGRESSION_BETA_FP = os.path.join(SCRATCH, REGRESSION_BETA_FILENAME)

# === SUICIDE PANEL DATA ===
SUICIDE_PROJECT_FOLDER = "data"
SUICIDE_PANEL_SUBDIR = os.path.join("merged", "USA")
SUICIDE_PANEL_FILENAME = "USA_adm2_1968_2004_monthly.dta"
SUICIDE_PANEL_FP = os.path.join(
    SCRATCH, SUICIDE_PROJECT_FOLDER, SUICIDE_PANEL_SUBDIR, SUICIDE_PANEL_FILENAME
)

# === GDNat AGGREGATED FILES ===
GDNAT_FOLDER_PATH = os.path.join(SCRATCH, "aggregated_results_gdnat_usa")
STAGG_FILENAME_ALL_YEARS = "gdnat_usa_agg_all_years.csv"
GDNAT_COUNTY_FP = os.path.join(GDNAT_FOLDER_PATH, STAGG_FILENAME_ALL_YEARS)
GDNAT_FILE_FORMAT = "gdnat_usa_agg_{year}.csv"

def get_gdnat_agg_yearly_old(year: int) -> str:
    """
    Returns the full path to the STAGG aggregated CSV file for a specific year.
    Example: get_stagg_data_path_for_year(2000) -> ".../gdnat_usa_agg_2000.csv"
    """
    filename = f"gdnat_usa_agg_{year}.csv"
    return os.path.join(GDNAT_FOLDER_PATH, filename)

# === RAW GDNAT TIFF FILES ===
RAW_GDNAT_FOLDER = "gdnat_tiff_files_by_yr"

def get_gdnat_raw_yearly(year: int) -> str:
    """
    Returns the full path to a raw GDNAT .tif file for a specific year.
    Example: get_raw_gdnat_tif_path_for_year(1979) -> ".../gdnat_tiff_files_by_yr/gdnat_1979.tif"
    """
    filename = f"gdnat_{year}.tif"
    return os.path.join(SCRATCH, RAW_GDNAT_FOLDER, filename)

# === OLD RAW GDNAT ZARR FILES ===
GDNAT_ZARR_DIR = os.path.join(SCRATCH, "global_suicide")

GDNAT_ZARR_1979_1999_FP = os.path.join(
    GDNAT_ZARR_DIR,
    "gdnat_ACCESS-CM2_tas_1979-1999_v2025-02-11.zarr",
)

GDNAT_ZARR_PATH_2000_2020_FP = os.path.join(
    GDNAT_ZARR_DIR,
    "gdnat_ACCESS-CM2_tas_2000-2020_v2025-02-11.zarr",
)

# === ERA5 DEGREE DAYS FILE (MAREN'S FILE) ===
ERA5_DD_FOLDER = os.path.join("data", "climatedata", "USA")
ERA5_DD_FILENAME = "temp_degreedays_1986_1999_USA_ERA5_pop_weights.csv"
ERA5_DD_FP = os.path.join(SCRATCH, ERA5_DD_FOLDER, ERA5_DD_FILENAME)

# === RAW ERA5 FILE ===
ERA5_RAW_FOLDER = os.path.join(SCRATCH, "era5_hourly_by_year")
def get_era5_raw_yearly(year: int) -> str:
    """
    Returns the full path to a raw ERA5 file for a specific year.
    """
    filename = f"era5_data_{year}.grib"
    return os.path.join(ERA5_RAW_FOLDER, filename)

# === ERA5 AGGREGATED FILES ===
ERA5_AGG_FOLDER = os.path.join(SCRATCH, "aggregated_results_era5_usa")
ERA5_AGG_FILE_FORMAT = "era5_usa_agg_{year}.csv"
def get_era5_agg_yearly(year: int) -> str:
    """
    Returns the full path to a aggregated ERA5 file for a specific year.
    """
    filename = f"era5_usa_agg_{year}.csv"
    return os.path.join(ERA5_AGG_FOLDER, filename)

# === ERA5 DAILY AGGREGATED FILES ===
ERA5_DAILY_FOLDER = os.path.join(SCRATCH, "era5_daily_by_year")
def get_era5_daily_yearly(year: int) -> str:
    """
    Returns the full path to a daily aggregated ERA5 .nc file for a specific year.
    Example: get_era5_daily_nc_path_for_year(2020) -> ".../era5_daily_by_year/era5_daily_mean_2020.nc"
    """
    filename = f"era5_daily_mean_{year}.nc"
    return os.path.join(ERA5_DAILY_FOLDER, filename)

# === ERA5 COUNTY-LEVEL AGGREGATED FILE ===
ERA5_COUNTY_AREA_FP = os.path.join(
    SCRATCH,
    "data", "climatedata", "USA",
    "usa_area_era5_temp_average_1968_2004_polynomial_5_area_crop_weights.csv"
)

# === CONDO PATH ===
CONDO_PATH = "/global/scratch/projects/co_carleton/carleton_colab" 
CONDO_DATA_DIR = os.path.join(CONDO_PATH, "data")

# === Raw GDNat Paths ===
GDNAT_DIR = os.path.join(CONDO_DATA_DIR, "gdnat_allmodels_Jul24")
GDNAT_DATA_FP = os.path.join(GDNAT_DIR, "gdnat_tas_1979-2020_v2025-02-11.zarr")

# === GDNAT TIFF OUTPUT PATHS ===
GDNAT_TIFF_OUTPUT_DIR = os.path.join(GDNAT_DIR, "tiff_by_yr_model")

# === GDNat Aggregated All Models ===
GDNAT_USA_AGG_ALL_MODELS_FP = os.path.join(
    GDNAT_DIR, "aggregated", "usa_pop_county"
)

# GDNat getter new

def get_gdnat_agg_yearly(year: int, model: str) -> str:
    """
    Returns the full path to the STAGG aggregated CSV file for a specific year.
    Example: get_stagg_data_path_for_year(2000) -> ".../gdnat_usa_agg_2000.csv"
    """
    filename = f"gdnat_usa_agg_{model}_{year}.csv"
    return os.path.join(GDNAT_USA_AGG_ALL_MODELS_FP, model, filename)

# Project folder 
PROJECT_FOLDER = os.path.join(CONDO_PATH, "global_suicide")
