# ==============================================================================
# ERA5 Climate Data Spatial-Temporal Aggregation Script
# ==============================================================================
# Purpose: Aggregate hourly ERA5 temperature data to monthly county-level 
#          averages with polynomial transformations for US counties (1979-2004)
# 
# Prerequisites: Load r-spatial module before running ('module load r-spatial')
# ==============================================================================

# ------ 1. LIBRARY MANAGEMENT ------

#' Helper function to install and load CRAN packages if they are missing
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Set up personal library if it doesn't exist
    if (!dir.exists(Sys.getenv("R_LIBS_USER"))) {
      dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
    }
    
    cat(sprintf("Installing package: %s\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = Sys.getenv("R_LIBS_USER"), ...)
  }
  library(pkg, character.only = TRUE)
}

# Install required packages
install_if_missing("sf")        # Spatial data handling
install_if_missing("dplyr")     # Data manipulation
install_if_missing("ggplot2")   # Plotting
install_if_missing("terra")     # Raster operations
install_if_missing("crayon")    # Colored console messages
install_if_missing("tictoc")    # Code timing

# Install stagg package from GitHub
if (!requireNamespace("stagg", quietly = TRUE)) {
  install_if_missing("remotes")
  remotes::install_github("tcarleton/stagg")
}
library(stagg)

cat(crayon::green("All required packages are installed and loaded.\n"))

# ------ 2. CONFIGURATION ------

# File paths
dir <- "/global/scratch/users/yougsanghvi"
dir_tiff <- file.path(dir, "era5_hourly_by_year")

# Define paths for usa county shapefile.
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Define an output directory for the aggregated results
output_dir <- file.path(dir, "aggregated_results_era5_usa")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

# File naming patterns
filename_pattern <- "era5_data_%d.grib"  # Input files: era5_data_YYYY.grib
file_format_prefix <- "era5_usa_agg_"    # Output files: era5_usa_agg_YYYY.csv
output_filepath_all_years <- file.path(output_dir, "era5_usa_agg_all_years.csv")

# Processing parameters
# Get year from command line argument or use default
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  year <- as.numeric(args[1])
  cat(crayon::blue(sprintf("Processing year %d from command line argument\n", year)))
} else {
  year <- 1979  # Default for standalone execution
  cat(crayon::yellow("No command line argument provided, using default year 1979\n"))
}

# Validate year
if (is.na(year) || year < 1979 || year > 2004) {
  stop("Invalid year provided. Must be between 1979 and 2004.")
}

overwrite <- TRUE     # Set to FALSE to skip existing files
pop_weight <- TRUE    # TRUE: population-weighted, FALSE: area-weighted only
daily <- FALSE        # TRUE if data is already daily aggregated

# ------ 3. SPATIAL DATA PREPARATION ------

cat(crayon::blue("Loading and preparing county shapefile...\n"))

# Load US county shapefile
usa_counties <- st_read(usa_county_path, quiet = TRUE)

# Ensure consistent CRS (WGS84)
usa_counties <- sf::st_transform(usa_counties, 4326)

# Fix invalid geometries
usa_counties <- st_make_valid(usa_counties)

# ------ 4. CALCULATE OVERLAY WEIGHTS ------

cat(crayon::blue("Calculating overlay weights...\n"))

if (pop_weight) {
  county_weights <- stagg::overlay_weights(
    usa_counties, 
    "GEOID",
    secondary_weights = pop_world_2015_era5
  )
} else {
  county_weights <- stagg::overlay_weights(usa_counties, "GEOID")
}

# ------ 5. RASTER PROCESSING AND AGGREGATION ------

cat(crayon::magenta(sprintf("Processing year %d...\n", year)))

# Construct file paths
current_filename <- sprintf(filename_pattern, year)
current_filepath <- file.path(dir_tiff, current_filename)
output_filename <- sprintf("%s%d.csv", file_format_prefix, year)
output_filepath <- file.path(output_dir, output_filename)

# Check if input file exists
if (!file.exists(current_filepath)) {
  stop(sprintf("Input file not found: %s", current_filepath))
}

# Check if output file exists and if overwrite is FALSE
if (!overwrite && file.exists(output_filepath)) {
  message(sprintf("Output file already exists: %s", output_filepath))
  message("Set overwrite=TRUE to regenerate or delete the existing file.")
  quit(status = 0)
}

# Load and process raster data
cat(sprintf("Reading raster data for %d...\n", year))
tic("Reading raster")
r <- terra::rast(current_filepath)
toc()

# Rotate longitude if needed (0-360 to -180-180)
if (terra::xmax(r) > 180) {
  r <- terra::rotate(r)
}

# Crop to US counties extent
cat("Cropping raster to USA counties...\n")
tic("Cropping raster")
r_crop <- terra::crop(r, usa_counties)
toc()

# Convert Kelvin to Celsius
r_crop_celsius <- r_crop - 273.15

# Run spatial-temporal aggregation
cat("Running stagg aggregation...\n")
tic("Stagg aggregation")
if (!daily) {
  temp_out <- stagg::staggregate_polynomial(
    data = r_crop_celsius,
    overlay_weights = county_weights,
    daily_agg = "average",
    time_agg = "month",
    start_date = paste0(year, "-01-01 00:00:00"),
    time_interval = "1 hour",
    degree = 4
  )
} else {
  temp_out <- stagg::staggregate_polynomial(
    data = r_crop_celsius,
    overlay_weights = county_weights,
    start_date = sprintf("%d-01-01 00:00:00", year),
    time_interval = "24 hour",
    daily_agg = "none",
    time_agg = "month",
    degree = 4
  )
}
toc()

# Add year column and save
temp_out$year <- year
write.csv(temp_out, output_filepath, row.names = FALSE)

cat(crayon::green(sprintf("Successfully processed and saved year %d to: %s\n", year, output_filepath)))
