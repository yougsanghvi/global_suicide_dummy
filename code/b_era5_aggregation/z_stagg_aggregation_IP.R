# !!! this code is not complete so be cautious while running it

# Please load r-spatial using 'module-load r-spatial' before running this code

# ------ 1. Library Management ------
# replace some of these with pacman

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

# Install and load CRAN packages essential for spatial data handling and manipulation.
install_if_missing("sf")
install_if_missing("dplyr")
install_if_missing("ggplot2") # For plotting results
install_if_missing("terra") # For raster operations
install_if_missing("crayon") # For printing colored messages in the console
install_if_missing("tictoc") # For timing code execution

# Install `stagg` from GitHub if it's not already installed.
# `remotes` is required to install packages directly from GitHub.
if (!requireNamespace("stagg", quietly = TRUE)) {
  install_if_missing("remotes")
  remotes::install_github("tcarleton/stagg")
}
library(stagg) # Load the stagg package

cat(crayon::green("All required packages are installed and loaded.\n"))

# ------ 2. Configuration: Setting File Paths and Other Inputs ------

# Define the base directory for data storage.
dir <- "/global/scratch/users/yougsanghvi"
dir_tiff <- file.path(dir, "era5_hourly_by_year")

# Define paths for usa county shapefile.
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Define the generic filename pattern for yearly NetCDF/TIFF files.
# The `{year}` placeholder will be replaced in the loop.
filename_pattern <- "era5_data_%d.grib" # e.g., era5_data_1979.grib

# Define an output directory for the aggregated results
output_dir <- file.path(dir, "aggregated_results_era5_usa")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

file_format_prefix <- "era5_usa_agg_" # the year will be appended to this prefix

# Define the full path for the final merged output file.
output_filepath_all_years <- file.path(
  output_dir,
  "era5_usa_agg_all_years.csv"
)

# Define the range of years to process
start_year <- 1979
end_year <- 2004

# DO NOT CHANGE -- no overwrite functionality can be added soon...
# ... but not already present
overwrite <- TRUE

# Set weighting scheme
# TRUE: add population secondary weights, FALSE: keep only area weighted

# !!! add functionality to change file names based on this
pop_weight <- TRUE

# variable to set if data is already daily aggregated
# set to TRUE if data is already daily aggregated
# for eg for GDNAT data
daily <- FALSE

# ------ 3. Polygon Data Preparation (USA Counties) ------

cat(crayon::blue("Calculating overlay weights...\n"))
# Read the world county shapefile into an sf object.
usa_counties <- st_read(usa_county_path, quiet = TRUE)
ext(usa_counties)

# Ensure the Coordinate Reference System (CRS) is WGS84 (EPSG:4326) for consistency.

usa_counties <- sf::st_transform(usa_counties, 4326)

# Fix any invalid geometries within the `usa_counties` dataset.
# Invalid geometries can cause issues in spatial operations.
usa_counties <- st_make_valid(usa_counties)

# ------ 4. Calculate Overlay Weights ------

if (pop_weight) {
  county_weights <- stagg::overlay_weights(
    usa_counties,
    "GEOID",
    secondary_weights = pop_world_2015_era5
  )
} else {
  county_weights <- stagg::overlay_weights(usa_counties, "GEOID")
}

# ------ 5. Raster Data Preparation and Aggregation Across Years ------

cat(crayon::magenta("Starting raster data processing and aggregation...\n"))
# Initialize an empty list to store aggregated data for all years
all_years_aggregated_data <- list()

# Loop through each year, process the corresponding raster, and aggregate
for (year in start_year:end_year) {
  tic("Processing year: ", year) # Start timing for the current year
  # for each loop instead
  message(sprintf("Processing year: %d", year))

  # Construct the full path for the current year's TIFF file
  current_filename <- sprintf(filename_pattern, year)
  current_filepath <- file.path(dir_tiff, current_filename)

  # Check if the file exists before processing
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

  #tic("Raster extent before cropping:\n")
  #print(ext(r)) # Print the extent of the raster
  #toc() # Print the time taken for printing the extent

  #tic("Raster cropping")
  #r <- terra::crop(r, bbox_extent) # remove this to run whole USA -- only debugging
  #toc() # Print the time taken for cropping

  #r_crop <- r[[which(format(time(r), "%m") %in% c("01", "02"))]]

  # Crop the shifted raster to the extent of the USA counties polygons.
  cat("Cropping raster to USA counties...\n")
  tic("Cropping raster to USA counties")
  r_crop <- terra::crop(r, usa_counties)
  toc() # Print the time taken for cropping

  # Convert temperature values from Kelvin to Celsius
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

  # Add a 'year' column to the aggregated data for later combination
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

# Save the combined data frame to a new CSV file.
write.csv(combined_df, output_filepath_all_years, row.names = FALSE)

# Display a confirmation message.
message(sprintf(
  "All aggregated data merged and saved to: %s",
  output_filepath_all_years
))

# !!! this code currently outputs dupicated rows.
# this seems harmless and i am simply dropping duplicate ones
# but this step needs to be done in analyses
# Will need explore this issue soon

# the file name also needs to be changed and the code reran
# currently it is merging in old files
