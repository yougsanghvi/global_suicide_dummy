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
    install.packages(pkg, repos = "https://cloud.r-project.org", ...)
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
start_year <- 1979
end_year <- 2004
overwrite <- TRUE     # Set to FALSE to skip existing files; not implemented yet, keep FALSE
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

cat(crayon::magenta("Starting raster data processing and aggregation...\n"))
# Initialize an empty list to store aggregated data for all years
all_years_aggregated_data <- list()

# Process each year individually
for (year in start_year:end_year) {
  tic(paste("Processing year:", year))
  message(sprintf("Processing year: %d", year))

  # Construct file paths
  current_filename <- sprintf(filename_pattern, year)
  current_filepath <- file.path(dir_tiff, current_filename)

  # Check if input file exists
  if (!file.exists(current_filepath)) {
    warning(sprintf(
      "Skipping year %d: File not found at %s",
      year,
      current_filepath
    ))
    next # Skip to the next iteration if file is missing
  }

  # Check if output file exists and if overwrite is FALSE
  if (!overwrite && file.exists(current_filepath)) {
    message(sprintf(
      "  Skipping year %d: Output file '%s' already exists and overwrite is FALSE.",
      year,
      basename(current_filepath)
    ))
    # If we are skipping, we still need to load the existing data to combine later
    # This assumes the existing CSV is valid and has the same structure
    # This code doesn't work, please set overwrite to TRUE for now
    tryCatch(
      {
        existing_data <- read.csv(current_filepath, stringsAsFactors = FALSE)
        all_years_aggregated_data[[as.character(year)]] <- existing_data
      },
      error = function(e) {
        warning(sprintf(
          "  Could not load existing data for year %d from '%s'. Error: %s",
          year,
          basename(current_filepath),
          e$message
        ))
        message(
          "  Proceeding to compute for this year instead of skipping due to load error."
        )
        # If loading fails, don't skip; proceed with computation
      }
    )
    next # Skip to the next iteration if conditions met and existing data loaded
  } # END OF IF STATEMENT

  # Read the raster data for the current year
  cat(sprintf(
    "  Reading raster data for %d from %s...\n",
    year,
    current_filepath
  ))

  tic()
  r <- terra::rast(current_filepath)
  toc() # Print the time taken for reading the raster

  if (terra::xmax(r) > 180) {
    r <- terra::rotate(r)
  }

  # Crop the shifted raster to the extent of the USA counties polygons.
  cat("Cropping raster to USA counties...\n")
  tic("Cropping raster to USA counties")
  r_crop <- terra::crop(r, usa_counties)
  toc() # Print the time taken for cropping

  # Convert Kelvin to Celsius
  r_crop_celsius <- r_crop - 273.15
  #cat("raster extent after cropping to USA counties:\n")
  #ext(r_crop_celsius)

  # Run `stagg::staggregate_polynomial` for the current year's data.
  # Run `stagg::staggregate_polynomial` for the current year's data.
  tic()
  message(sprintf("  Running stagg::staggregate_polynomial for %d...", year))

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
  toc() # Print the time taken for aggregation

  # Add year column and save
  temp_out$year <- year

  # Save the current year's aggregated data separately
  output_filename <- sprintf("%s%d.csv", file_format_prefix, year)
  output_filepath <- file.path(output_dir, output_filename)
  write.csv(temp_out, output_filepath, row.names = FALSE)
  message(sprintf(
    "  Saved aggregated data for %d to %s",
    year,
    output_filepath
  ))

  # Store the current year's aggregated data in the list for combined output
  all_years_aggregated_data[[as.character(year)]] <- temp_out
  toc() # End timing for the current year
} # END OF FOR LOOP

# Combine all yearly aggregated data into a single data frame

# List all aggregated CSV files in the output directory
# !!! the below will have to be changed every time the file name changes
# Make this an automatic change in the future by setting file names dynamically

# List all yearly aggregated CSV files in the output directory.

pattern <- sprintf("^%s.*\\.csv$", file_format_prefix)

all_csv_files <- list.files(
  path = output_dir,
  pattern = pattern,
  full.names = TRUE
)

cat(crayon::yellow(sprintf(
  "Merging %d yearly aggregated CSV files into one...\n",
  length(all_csv_files)
)))
# Sort files chronologically for correct data order.
all_csv_files <- sort(all_csv_files)

# Read each CSV file into a list of data frames.
list_of_dfs <- lapply(all_csv_files, read.csv, stringsAsFactors = FALSE)

# Combine all data frames into a single data frame by row.
combined_df <- do.call(rbind, list_of_dfs)

# Save final combined dataset
write.csv(combined_df, output_filepath_all_years, row.names = FALSE)

cat(crayon::green(sprintf(
  "Final aggregated dataset saved to: %s\n", 
  output_filepath_all_years
)))

# Note: This output may contain duplicate rows that should be removed in analysis
