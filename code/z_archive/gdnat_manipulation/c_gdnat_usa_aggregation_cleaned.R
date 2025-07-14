# !!! this code is not complete so be cautious while running it

# Please load r-spatial using 'module-load r-spatial' before running this code

# ------ 1. Library Management ------

#' Helper function to install and load CRAN packages if they are missing
#'
#' @param pkg Character string: The name of the package to install and load.
#' @param ... Additional arguments passed to `install.packages()`.
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(
      pkg,
      repos = "[https://cloud.r-project.org](https://cloud.r-project.org)",
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

# Install `stagg` from GitHub if it's not already installed.
# `remotes` is required to install packages directly from GitHub.
if (!requireNamespace("stagg", quietly = TRUE)) {
  install_if_missing("remotes")
  remotes::install_github("tcarleton/stagg")
}
library(stagg) # Load the stagg package

# ------ 2. Configuration: Setting File Paths and Other Inputs ------

# Define the base directory for data storage.
dir <- "/global/scratch/users/yougsanghvi"
dir_tiff <- file.path(dir, "gdnat_tiff_files_by_yr")

# Define paths for world county shapefile.
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Define the generic filename pattern for yearly NetCDF/TIFF files.
# The `{year}` placeholder will be replaced in the loop.
gdnat_tiff_filename_pattern <- "gdnat_%d.tif" # e.g., gdnat_1979.tif

# Define an output directory for the aggregated results
output_dir <- file.path(dir, "aggregated_results_gdnat_usa")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

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

# ------ 3. Polygon Data Preparation (USA Counties) ------

# Read the world county shapefile into an sf object.
usa_counties <- st_read(usa_county_path, quiet = TRUE)
ext(usa_counties)

# Ensure the Coordinate Reference System (CRS) is WGS84 (EPSG:4326) for consistency.

usa_counties <- sf::st_transform(usa_counties, 4326)


# Fix any invalid geometries within the `usa_counties` dataset.
# Invalid geometries can cause issues in spatial operations.
usa_counties <- st_make_valid(usa_counties)

# Crop the `usa_counties` data to the continental US bounding box.
# This helps reduce processing time and avoids issues with non-contiguous states like Alaska and Hawaii.
# bbox_poly <- st_as_sfc(st_bbox(
#  c(xmin = -140, ymin = 15, xmax = -50, ymax = 50),
#  crs = st_crs(usa_counties)
# ))
# usa_counties <- st_filter(usa_counties, bbox_poly)

# ------ 4. Calculate Overlay Weights ------

# This function calculates how much each grid cell overlaps with each polygon,
# optionally weighting by a secondary raster (e.g., population density).
# county_weights <- overlay_weights(
#  polygons = usa_counties,
#  polygon_id_col = "GID_2",
#  grid = era5_grid,
#  secondary_weights = pop_world_2015_era5
# )

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

# Initialize an empty list to store aggregated data for all years
all_years_aggregated_data <- list()

# Loop through each year, process the corresponding raster, and aggregate
for (year in start_year:end_year) {
  message(sprintf("Processing year: %d", year))

  # Construct the full path for the current year's TIFF file
  current_tiff_filename <- sprintf(gdnat_tiff_filename_pattern, year)
  current_tiff_path <- file.path(dir_tiff, current_tiff_filename)

  # Check if the file exists before processing
  if (!file.exists(current_tiff_path)) {
    warning(sprintf(
      "Skipping year %d: File not found at %s",
      year,
      current_tiff_path
    ))
    next # Skip to the next iteration if file is missing
  }

  # Check if output file exists and if overwrite is FALSE
  if (!overwrite && file.exists(current_tiff_path)) {
    message(sprintf(
      "  Skipping year %d: Output file '%s' already exists and overwrite is FALSE.",
      year,
      basename(current_tiff_path)
    ))
    # If we are skipping, we still need to load the existing data to combine later
    # This assumes the existing CSV is valid and has the same structure
    # This code doesn't work, please set overwrite to TRUE for now
    tryCatch(
      {
        existing_data <- read.csv(current_tiff_path, stringsAsFactors = FALSE)
        all_years_aggregated_data[[as.character(year)]] <- existing_data
      },
      error = function(e) {
        warning(sprintf(
          "  Could not load existing data for year %d from '%s'. Error: %s",
          year,
          basename(current_tiff_path),
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
  r <- terra::rast(current_tiff_path)

  # Rotate the raster if necessary.
  r_shifted <- terra::rotate(r)

  # Crop the shifted raster to the extent of the USA counties polygons.
  r_crop <- terra::crop(r_shifted, usa_counties)

  # Convert temperature values from Kelvin to Celsius
  r_crop_celsius <- r_crop - 273.15

  # Run `stagg::staggregate_polynomial` for the current year's data.
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

  # Add a 'year' column to the aggregated data for later combination
  temp_out$year <- year

  # Save the current year's aggregated data separately
  output_filename <- sprintf("gdnat_usa_agg_%d.csv", year)
  output_filepath <- file.path(output_dir, output_filename)
  write.csv(temp_out, output_filepath, row.names = FALSE)
  message(sprintf(
    "  Saved aggregated data for %d to %s",
    year,
    output_filepath
  ))

  # Store the current year's aggregated data in the list for combined output
  all_years_aggregated_data[[as.character(year)]] <- temp_out
} # END OF FOR LOOP

# Combine all yearly aggregated data into a single data frame

# List all aggregated CSV files in the output directory
# !!! the below will have to be changed every time the file name changes
# Make this an automatic change in the future by setting file names dynamically

# List all yearly aggregated CSV files in the output directory.
all_csv_files <- list.files(
  path = output_dir,
  pattern = "^gdnat_usa_agg_.*\\.csv$", # Matches files like "gdnat_usa_agg_YYYY.csv"
  full.names = TRUE # Get full file paths
)

# Sort files chronologically for correct data order.
all_csv_files <- sort(all_csv_files)

# Read each CSV file into a list of data frames.
list_of_dfs <- lapply(all_csv_files, read.csv, stringsAsFactors = FALSE)

# Combine all data frames into a single data frame by row.
combined_df <- do.call(rbind, list_of_dfs)

# Define the full path for the final merged output file.
output_filepath_all_years <- file.path(
  output_dir,
  "gdnat_usa_agg_all_years.csv"
)

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