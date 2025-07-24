# ==============================================================================
# ERA5 Climate Data Spatial-Temporal Aggregation Script
# ==============================================================================
# Purpose: Aggregate hourly ERA5 temperature data to monthly county-level 
#          averages with polynomial transformations for US counties (1979-2004)
# 
# Prerequisites: Load r-spatial module before running ('module load r-spatial')
# ==============================================================================

invisible(
  .libPaths(
    c(
      "/global/home/users/yougsanghvi/R/x86_64-pc-linux-gnu-library/4.4",
      "/global/software/rocky-8.x86_64/manual/modules/r/4.4.0/r-spatial",       
      "/global/software/rocky-8.x86_64/manual/modules/langs/r-packages/r4.4.0",     
      "/global/software/rocky-8.x86_64/manual/modules/langs/r/4.4.0/lib64/R/library"
    )
  )
)

# ------ 1. LIBRARY MANAGEMENT ------

# Load required packages (must be pre-installed)
library(sf)        # Spatial data handling
library(dplyr)     # Data manipulation
library(ggplot2)   # Plotting
library(terra)     # Raster operations
library(crayon)    # Colored console messages
library(tictoc)    # Code timing
library(remotes)   # For GitHub packages
library(stagg)     # Spatial-temporal aggregation

cat(crayon::green("All required packages loaded.\n"))

library(pryr)  # for mem_used()

print_memory <- function(label = "") {
  gc()
  used <- pryr::mem_used()
  if (used > 1e9) {
    mem_str <- sprintf("%.2f GB", used / 1e9)
  } else if (used > 1e6) {
    mem_str <- sprintf("%.2f MB", used / 1e6)
  } else {
    mem_str <- sprintf("%.2f kB", used / 1e3)
  }
  cat(sprintf("[%s] Memory used: %s\n", label, mem_str))
}



# ------ 2. CONFIGURATION ------

print_memory("Start of script")
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

overwrite <- FALSE     # Set to FALSE to skip existing files
pop_weight <- TRUE    # TRUE: population-weighted, FALSE: area-weighted only
daily <- FALSE        # TRUE if data is already daily aggregated

print_memory("After configuration")

# ------ 3. SPATIAL DATA PREPARATION ------

cat(crayon::blue("Loading and preparing county shapefile...\n"))

# Load US county shapefile
usa_counties <- st_read(usa_county_path, quiet = TRUE)

# Ensure consistent CRS (WGS84)
# usa_counties <- sf::st_transform(usa_counties, 4326)

# Fix invalid geometries
# usa_counties <- st_make_valid(usa_counties)

print_memory("After loading USA counties shapefile")

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

print_memory("After calculating overlay weights")

# ------ 5. RASTER PROCESSING AND AGGREGATION ------

cat(crayon::magenta(sprintf("Processing year %d...\n", year)))
cat("\t constructing file paths")
# Construct file paths
current_filename <- sprintf(filename_pattern, year)
current_filepath <- file.path(dir_tiff, current_filename)
output_filename <- sprintf("%s%d.csv", file_format_prefix, year)
output_filepath <- file.path(output_dir, output_filename)

cat("\t checking input file paths")
# Check if input file exists
if (!file.exists(current_filepath)) {
  stop(sprintf("Input file not found: %s", current_filepath))
}

cat("\t checking output file paths")
# Check if output file exists and if overwrite is FALSE
if (!overwrite && file.exists(output_filepath)) {
  message(sprintf("Output file already exists: %s", output_filepath))
  message("Set overwrite=TRUE to regenerate or delete the existing file.")
  quit(status = 0)
}

print_memory("before reading raster data")

# Load and process raster data
cat(sprintf("Reading raster data for %d...\n", year))
r <- terra::rast(current_filepath)

print_memory("After reading raster data")

# Rotate longitude if needed (0-360 to -180-180)
if (terra::xmax(r) > 190) {
  print("correcting lat long coordinates")
  r <- terra::rotate(r)
}

if (is.na(crs(r, describe = TRUE)$code) || crs(r, describe = TRUE)$code != 4326) {
  message("Reprojecting raster to EPSG:4326 (WGS 84)...")
  r <- project(r, "EPSG:4326")
} else {
  message("Raster is already in WGS 84 (EPSG:4326).")
}

# ways to make cropping faster:

# Method 1: (not as fast but very scaleable)
# Crop using extent (bounding box only)
# this also fails due to memory issues 
# cat("Cropping raster to USA counties using ext...\n")
# r_crop <- terra::crop(r, terra::ext(usa_counties))

# Method 2: Manual bbox (removed ext overhead)
# xmin <- -125
# xmax <- -66
# ymin <- 24
# ymax <- 50
# Create extent
# bbox_ext <- terra::ext(xmin, xmax, ymin, ymax)
# Crop raster by this bbox
# r_cropped <- terra::crop(r, bbox_ext)
 
# Initial Method: Crop to US counties extent
# cat("Cropping raster to USA counties using ext...\n")

#print_memory("Before cropping raster data")
#tic("Cropping raster data to USA counties extent")
r_crop <- terra::crop(r, terra::vect(usa_counties))
#toc()
#print_memory("After cropping raster data")

# Convert Kelvin to Celsius
# cat("converting raster data from Kelvin to Celsius...\n")
# r_crop_celsius <- r_crop - 273.15

# print_memory("After converting to Celsius")

# Run spatial-temporal aggregation
cat("Running stagg aggregation...\n")
tic("Stagg aggregation")

print_memory("Before stagg aggregation")
if (!daily) {
  temp_out <- stagg::staggregate_polynomial(
    data = r, # must change name here if crop code is uncommented
    overlay_weights = county_weights,
    daily_agg = "average",
    time_agg = "month",
    start_date = paste0(year, "-01-01 00:00:00"),
    time_interval = "1 hour",
    degree = 4
  )
} else {
  temp_out <- stagg::staggregate_polynomial(
    data = r, # must change name here if crop code is uncommented
    overlay_weights = county_weights,
    start_date = sprintf("%d-01-01 00:00:00", year),
    time_interval = "24 hour",
    daily_agg = "none",
    time_agg = "month",
    degree = 4
  )
}
toc() # End of aggregation

print_memory("After stagg aggregation")

# Add year column and save
temp_out$year <- year
write.csv(temp_out, output_filepath, row.names = FALSE)

cat(crayon::green(sprintf("Successfully processed and saved year %d to: %s\n", year, output_filepath)))
