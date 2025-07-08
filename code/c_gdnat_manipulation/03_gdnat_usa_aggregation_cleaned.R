
# ------------------------------------------------------------------------------
# GDNat USA Aggregation Script (Cleaned)
# ------------------------------------------------------------------------------
# This script aggregates GDNat climate data (GeoTIFF) to US counties using the
# stagg package, optionally with population weighting. It outputs yearly and
# combined CSVs of county-level temperature statistics.
#
# Author: Youg Sanghvi
# Date: July 7, 2025
#
# Requirements:
#   - R packages: sf, dplyr, ggplot2, terra, stagg, remotes (for stagg install)
#   - r-spatial module loaded if on a cluster (see below)
#   - Input: GDNat GeoTIFFs, US county shapefile, (optional) population raster
#   - Output: Aggregated CSVs per year and a combined CSV
#
# Usage notes:
#   - Load r-spatial: module load r-spatial
#   - This script is not fully standalone; see TODOs and warnings below.
# ------------------------------------------------------------------------------

# ------------------- 1. Library Management -------------------

# Helper function to install and load CRAN packages if missing
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", ...)
  }
  library(pkg, character.only = TRUE)
}

# Install/load required packages
install_if_missing("sf")
install_if_missing("dplyr")
install_if_missing("ggplot2")
install_if_missing("terra")

# Install/load stagg from GitHub if needed
if (!requireNamespace("stagg", quietly = TRUE)) {
  install_if_missing("remotes")
  remotes::install_github("tcarleton/stagg")
}
library(stagg)


# ------------------- 2. Configuration: File Paths & Inputs -------------------

# Base directory for data storage
dir <- "/global/scratch/users/yougsanghvi"
dir_tiff <- file.path(dir, "gdnat_tiff_files_by_yr")

# Path to US county shapefile (must exist)
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Filename pattern for yearly GDNat GeoTIFFs (e.g., gdnat_1979.tif)
gdnat_tiff_filename_pattern <- "gdnat_%d.tif"

# Output directory for aggregated results
output_dir <- file.path(dir, "aggregated_results_gdnat_usa")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

# Range of years to process
start_year <- 1979
end_year <- 2004

# Overwrite existing output files? (TRUE recommended for now)
overwrite <- TRUE

# Weighting scheme: TRUE = population-weighted, FALSE = area-weighted only
# TODO: Add logic to change file names based on weighting
pop_weight <- TRUE


# ------------------- 3. Polygon Data Preparation (USA Counties) -------------------

# Read US county shapefile as sf object
usa_counties <- st_read(usa_county_path, quiet = TRUE)
ext(usa_counties) # Print extent for reference

# Ensure CRS is WGS84 (EPSG:4326)
usa_counties <- sf::st_transform(usa_counties, 4326)

# Fix invalid geometries (required for spatial ops)
usa_counties <- st_make_valid(usa_counties)

# Optionally crop to continental US (uncomment to use)
# bbox_poly <- st_as_sfc(st_bbox(
#   c(xmin = -140, ymin = 15, xmax = -50, ymax = 50),
#   crs = st_crs(usa_counties)
# ))
# usa_counties <- st_filter(usa_counties, bbox_poly)


# ------------------- 4. Calculate Overlay Weights -------------------
# Calculate grid-to-county overlay weights (area or population-weighted)
if (pop_weight) {
  county_weights <- stagg::overlay_weights(
    usa_counties,
    "GEOID",
    secondary_weights = pop_world_2015_era5
  )
} else {
  county_weights <- stagg::overlay_weights(usa_counties, "GEOID")
}


# ------------------- 5. Raster Aggregation Across Years -------------------
# Loop through each year, aggregate raster to counties, and save CSV
all_years_aggregated_data <- list()
for (year in start_year:end_year) {
  message(sprintf("Processing year: %d", year))
  current_tiff_filename <- sprintf(gdnat_tiff_filename_pattern, year)
  current_tiff_path <- file.path(dir_tiff, current_tiff_filename)
  if (!file.exists(current_tiff_path)) {
    warning(sprintf(
      "Skipping year %d: File not found at %s",
      year, current_tiff_path
    ))
    next
  }
  # Overwrite logic: always recompute for now
  # Read raster, rotate if needed, crop to counties
  r <- terra::rast(current_tiff_path)
  r_shifted <- terra::rotate(r)
  r_crop <- terra::crop(r_shifted, usa_counties)
  # Convert Kelvin to Celsius
  r_crop_celsius <- r_crop - 273.15
  # Aggregate to counties using stagg
  message(sprintf("  Running stagg::staggregate_polynomial for %d...", year))
  temp_out <- stagg::staggregate_polynomial(
    data = r_crop_celsius,
    overlay_weights = county_weights,
    start_date = sprintf("%d-01-01 00:00:00", year),
    time_interval = "24 hour",
    daily_agg = "none",
    time_agg = "month",
    degree = 4
  )
  temp_out$year <- year
  output_filename <- sprintf("gdnat_usa_agg_%d.csv", year)
  output_filepath <- file.path(output_dir, output_filename)
  write.csv(temp_out, output_filepath, row.names = FALSE)
  message(sprintf(
    "  Saved aggregated data for %d to %s",
    year, output_filepath
  ))
  all_years_aggregated_data[[as.character(year)]] <- temp_out
}


# ------------------- 6. Combine All Years to Single CSV -------------------
# List and combine all yearly CSVs into one file
all_csv_files <- list.files(
  path = output_dir,
  pattern = "^gdnat_usa_agg_.*\\.csv$",
  full.names = TRUE
)
all_csv_files <- sort(all_csv_files)
list_of_dfs <- lapply(all_csv_files, read.csv, stringsAsFactors = FALSE)
combined_df <- do.call(rbind, list_of_dfs)
output_filepath_all_years <- file.path(output_dir, "gdnat_usa_agg_all_years.csv")
write.csv(combined_df, output_filepath_all_years, row.names = FALSE)
message(sprintf(
  "All aggregated data merged and saved to: %s",
  output_filepath_all_years
))

# ------------------- 7. Notes & TODOs -------------------
# - This code may output duplicate rows; drop duplicates in downstream analysis.
# - File naming and merging logic should be improved for robustness.
# - Ensure all required input files exist before running.
# - Add error handling and logging for production use.
