# ==============================================================================
# ERA5 / GDNat Climate Data Spatial-Temporal Aggregation Script
# ==============================================================================
# Purpose: Aggregate climate raster data to monthly county-level 
#          averages with polynomial transformations for US counties (1979–2020)
# Prerequisites: Load r-spatial module ('module load r-spatial')
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

source("/global/home/users/yougsanghvi/global_suicide_dummy/code/y_utils/config.R")

# ------ 1. LIBRARY MANAGEMENT ------

library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(crayon)
library(tictoc)
library(remotes)
library(stagg)
library(pryr)

cat(crayon::green("All required packages loaded.\n"))

print_memory <- function(label = "") {
  gc()
  used <- pryr::mem_used()
  mem_str <- if (used > 1e9) sprintf("%.2f GB", used / 1e9) else if (used > 1e6) sprintf("%.2f MB", used / 1e6) else sprintf("%.2f kB", used / 1e3)
  cat(sprintf("[%s] Memory used: %s\n", label, mem_str))
}

# ------ 2. CONFIGURATION ------

print_memory("Start of script")

# File paths
dir <- "/global/scratch/users/yougsanghvi"
DATA_SOURCE <- "gdnat"  # or "era5"
is_era5 <- DATA_SOURCE == "era5"

# Get year from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  year <- as.numeric(args[1])
  cat(crayon::blue(sprintf("Processing year %d from command line argument\n", year)))
} else {
  year <- 1979
  cat(crayon::yellow("No command line argument provided, using default year 1979\n"))
}
if (is.na(year) || year < 1979 || year > 2004) stop("Invalid year provided. Must be between 1979 and 2020.")

overwrite <- FALSE
pop_weight <- TRUE

# Shapefile setup
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Load US counties
cat(crayon::blue("Loading and preparing county shapefile...\n"))
usa_counties <- st_read(usa_county_path, quiet = TRUE)
print_memory("After loading USA counties shapefile")

# Overlay weights
cat(crayon::blue("Calculating overlay weights...\n"))
if (pop_weight) {
  county_weights <- stagg::overlay_weights(usa_counties, "GEOID", secondary_weights = pop_world_2015_era5)
} else {
  county_weights <- stagg::overlay_weights(usa_counties, "GEOID")
}
print_memory("After calculating overlay weights")

# ------ 3. MODEL LOOP (for GDNat) OR Single-run (ERA5) ------

if (is_era5) {
  model_list <- c("ERA5")
} else {
  model_list <- c("MIROC6", "ACCESS-ESM1-5", "CanESM5", "MRI-ESM2-0", "NorESM2-LM")
}

for (model_name in model_list) {

  cat(crayon::blue(sprintf("\n==================== Processing: %s (year %d) ====================\n", model_name, year)))

  if (is_era5) {
    dir_tiff <- ERA5_RAW_FOLDER
    filename_pattern <- "era5_data_%d.grib"
    file_format_prefix <- "era5_usa_agg_"
    output_dir <- ERA5_AGG_FOLDER
    daily <- FALSE
  } else {
    filename_pattern <- "gdnat_%d.tif"
    dir_tiff <- file.path(GDNAT_TIFF_DIR, model_name)
    file_format_prefix <- paste0("gdnat_usa_agg_", model_name, "_")
    output_dir <- file.path(GDNAT_DIR, "aggregated", "usa_pop_county", model_name)
    daily <- TRUE
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }

  output_filepath_all_years <- file.path(output_dir, paste0(file_format_prefix, "all_years.csv"))
  current_filename <- sprintf(filename_pattern, year)
  current_filepath <- file.path(dir_tiff, current_filename)
  output_filename <- sprintf("%s%d.csv", file_format_prefix, year)
  output_filepath <- file.path(output_dir, output_filename)

  # Check if input exists
  if (!file.exists(current_filepath)) stop(sprintf("Input file not found: %s", current_filepath))
  if (!overwrite && file.exists(output_filepath)) {
    message(sprintf("Output already exists: %s", output_filepath))
    next
  }

  print_memory("Before reading raster data")
  cat(sprintf("Reading raster data for %d...\n", year))
  r <- terra::rast(current_filepath)
  print_memory("After reading raster data")

  if (terra::xmax(r) > 190) {
    print("Correcting longitudes...")
    r <- terra::rotate(r)
  }

  cat("Cropping raster to USA extent...\n")
  r_crop <- terra::crop(r, terra::vect(usa_counties))
  r_crop_celsius <- r_crop - 273.15

  cat("Running stagg aggregation...\n")
  tic("Stagg aggregation")
  print_memory("Before stagg aggregation")

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
  print_memory("After stagg aggregation")

  temp_out$year <- year
  write.csv(temp_out, output_filepath, row.names = FALSE)
  cat(crayon::green(sprintf("Saved output: %s\n", output_filepath)))
}
