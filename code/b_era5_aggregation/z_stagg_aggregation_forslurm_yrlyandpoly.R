# ==============================================================================
# ERA5 Climate Data Spatial-Temporal Aggregation Script
# ==============================================================================
# Purpose: Aggregate hourly ERA5 temperature data to monthly county-level
#          averages with polynomial transformations for US counties (1979-2004)
#
# Prerequisites: Load r-spatial module before running ('module load r-spatial')
# ==============================================================================
options(
  warning.length = 8170, # Increase max length if needed
  warning.expression = quote({
    warning_message <- geterrmessage()
    cat("[WARNING] ", warning_message, "\n", file = stderr())
    flush.console()
  })
)

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

# Load required packages (must be pre-installed)
library(sf) # Spatial data handling
library(dplyr) # Data manipulation
library(ggplot2) # Plotting
library(terra) # Raster operations
library(crayon) # Colored console messages
library(tictoc) # Code timing
library(remotes) # For GitHub packages
library(stagg) # Spatial-temporal aggregation

cat(crayon::green("All required packages loaded.\n"))

library(pryr) # for mem_used()

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
# Set data source manually here
DATA_SOURCE <- "era5"  # or "gdnat"
is_era5 <- DATA_SOURCE == "era5"

dir <- "/global/scratch/users/yougsanghvi"
if (is_era5) {
  dir_tiff <- ERA5_RAW_FOLDER
  filename_pattern <- "era5_data_%d.grib"
  file_format_prefix <- "era5_usa_agg_"
  output_dir <- ERA5_AGG_FOLDER
  daily <- FALSE
} else {
  # modify this to represent new per model structure 
  dir_tiff <- file.path(dir, "gdnat_hourly_by_year")
  filename_pattern <- "gdnat_data_%d.nc"
  file_format_prefix <- "gdnat_usa_agg_"
  output_dir <- file.path(dir, "aggregated_results_gdnat_usa")
  daily <- TRUE
}

# Define paths for usa county shapefile.
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Define an output directory for the aggregated results
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

output_filepath_all_years <- file.path(output_dir, paste0(file_format_prefix, "all_years.csv"))

# Processing parameters
# Get year from command line argument or use default
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  year <- as.numeric(args[1])
  cat(crayon::blue(sprintf(
    "Processing year %d from command line argument\n",
    year
  )))
} else {
  year <- 1989 # Default for standalone execution
  cat(crayon::yellow(
    "No command line argument provided, using default year 1979\n"
  ))
}

# Validate year
if (is.na(year) || year < 1979 || year > 2004) {
  stop("Invalid year provided. Must be between 1979 and 2004.")
}

overwrite <- FALSE # Set to FALSE to skip existing files
pop_weight <- TRUE # TRUE: population-weighted, FALSE: area-weighted only
daily <- FALSE # TRUE if data is already daily aggregated

print_memory("After configuration")

# ------ 3. SPATIAL DATA PREPARATION ------

cat(crayon::blue("Loading and preparing county shapefile...\n"))

# Load US county shapefile
usa_counties <- st_read(usa_county_path, quiet = TRUE)

# Ensure consistent CRS (WGS84)
usa_counties <- sf::st_transform(usa_counties, 4326)

# Fix invalid geometries
usa_counties <- st_make_valid(usa_counties)
print(ext(usa_counties))
print(sf::st_crs(usa_counties))

print_memory("After loading USA counties shapefile")

# ------ 4. RASTER PROCESSING AND AGGREGATION ------

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

cat("\t checking output file paths \n")
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

print(ext(r))
print(sf::st_crs(r))

print_memory("After reading raster data")

# Rotate longitude if needed (0-360 to -180-180)
if (terra::xmax(r) > 190) {
  print("correcting lat long coordinates")
  r <- terra::rotate(r)
}

results_list <- list()

for (i in seq_len(nrow(usa_counties))) {
  # if (i > 1) {
  #  break
  # }

  tic("Processing county: ", i) # Start timing for the current county
  county <- usa_counties[i, ]
  county_id <- county$GEOID

  print(sprintf("county-level shapefile details for ", county_id))
  print(ext(county))
  print(sf::st_crs(county))

  cat(sprintf(
    "=== Processing county %s (%d of %d) ===\n",
    county_id,
    i,
    nrow(usa_counties)
  ))

  cat("calculating overlay weights for county ", county_id, "\n")
  # Calculate overlay weights
  overlay_weights <- tryCatch(
    {
      if (pop_weight) {
        stagg::overlay_weights(
          county,
          "GEOID",
          secondary_weights = pop_world_2015_era5
        )
      } else {
        stagg::overlay_weights(county, "GEOID")
      }
    },
    error = function(e) {
      warning(sprintf(
        "Overlay weight failed for county %s: %s",
        county_id,
        e$message
      ))
      return(NULL)
    }
  )

  class(overlay_weights)
  print(overlay_weights)

  if (is.null(overlay_weights) || nrow(overlay_weights) == 0) {
    warning(sprintf("No valid overlay weights for %s, skipping.", county_id))
    next
  }

  cat("dimensions of raster before cropping:\n")
  print(ext(r))
  print(sf::st_crs(r))
  cat("dimensions of county before cropping:\n")
  print(ext(county))
  print(sf::st_crs(county))
  
  cat("cropping raster to county ", county_id, "\n")
  tic("time taken to crop raster")
  # Crop raster to county
  r_crop <- tryCatch(
    {
      cat("inside tryCatch for cropping raster\n")
      # terra::crop(r, terra::vect(county))
      terra::crop(r, terra::vect(county))
    },
    error = function(e) {
      warning(sprintf("Raster crop failed for %s: %s", county_id, e$message))
      return(NULL)
    }
  )

  if (is.null(r_crop)) {
    warning(sprintf("Skipping county %s due to failed raster crop.", county_id))
    next
  }
  toc() # End timing for the raster crop

  print(ext(r_crop))
  print(sf::st_crs(r_crop))
  terra::spatSample(r_crop, size = 5, method = "regular", values = TRUE)

  cat("running staggregate_polynomial for county ", county_id, "\n")
  # Run staggregate_polynomial with daily conditional
  county_result <- tryCatch(
    {
      if (!daily) {
        stagg::staggregate_polynomial(
          data = r_crop,
          overlay_weights = overlay_weights,
          daily_agg = "average",
          time_agg = "month",
          start_date = paste0(year, "-01-01 00:00:00"),
          time_interval = "1 hour",
          degree = 4
        )
      } else {
        stagg::staggregate_polynomial(
          data = r_crop,
          overlay_weights = overlay_weights,
          daily_agg = "none",
          time_agg = "month",
          start_date = paste0(year, "-01-01 00:00:00"),
          time_interval = "24 hour",
          degree = 4
        )
      }
    },
    error = function(e) {
      warning(sprintf(
        "Stagg aggregation failed for %s: %s",
        county_id,
        e$message
      ))
      return(NULL)
    }
  )

  if (!is.null(county_result) && nrow(county_result) > 0) {
    cat("county result is good!\n")
    print(county_result)
    county_result$GEOID <- county_id
    county_result$year <- year
    results_list[[length(results_list) + 1]] <- county_result
  } else {
    cat("county result is not good")
    print(county_result)
  }

  print(county_result)
  print_memory(sprintf("After county %s", county_id))
  flush.console()

  toc() # End timing for the current county
}

# Combine results and save
if (length(results_list) > 0) {
  temp_out <- dplyr::bind_rows(results_list)
  write.csv(temp_out, output_filepath, row.names = FALSE)
  cat(crayon::green(sprintf(
    "Saved per-county aggregated data for year %d\n",
    year
  )))
} else {
  stop("No county results generated.")
}
