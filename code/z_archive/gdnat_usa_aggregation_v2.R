# Please load r-spatial using 'module-load r-spatial' before running this code

# ------ 1. Library Management ------

#' Helper function to install and load CRAN packages if they are missing
#'
#' @param pkg Character string: The name of the package to install and load.
#' @param ... Additional arguments passed to `install.packages()`.
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "[https://cloud.r-project.org](https://cloud.r-project.org)", ...)
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

# ------ 2. Configuration: Setting File Paths ------

# Define the base directory for data storage.
dir <- "/global/scratch/users/yougsanghvi"

# Define paths for world county shapefile.
world_county_dir <- file.path(dir, "gadm36_levels_shp")
world_county_filename <- "gadm36_2.shp"
world_county_path <- file.path(world_county_dir, world_county_filename)

# Define path for the NetCDF/TIFF file to be processed later.
gdnat_tiff_filename <- "gdnat_1979.tif"
tiff_path <- file.path(dir, gdnat_tiff_filename)


# ------ 3. Polygon Data Preparation (USA Counties) ------

# Read the world county shapefile into an sf object.
world_counties <- st_read(world_county_path, quiet = TRUE)

# Filter the `world_counties` data to include only United States counties.
usa_counties <- world_counties %>%
  dplyr::filter(NAME_0 %in% c("United States of America", "USA", "United States"))

# Ensure the Coordinate Reference System (CRS) is WGS84 (EPSG:4326) for consistency.
# This is crucial for correct spatial operations.
usa_counties <- sf::st_transform(usa_counties, 4326)
#print("Bounding box of US Counties after CRS transformation:")
# print(st_bbox(usa_counties))

# Fix any invalid geometries within the `usa_counties` dataset.
# Invalid geometries can cause issues in spatial operations.
usa_counties <- st_make_valid(usa_counties)
# print("Bounding box of US Counties after validating geometries:")
# print(st_bbox(usa_counties))

# Crop the `usa_counties` data to the continental US bounding box.
# This helps reduce processing time and avoids issues with non-contiguous states like Alaska and Hawaii.
bbox_poly <- st_as_sfc(st_bbox(
  c(xmin = -140, ymin = 20, xmax = -50, ymax = 50),
  crs = st_crs(usa_counties)
))
usa_counties <- st_filter(usa_counties, bbox_poly)
# print("Bounding box of US Counties after cropping to continental US:")
# print(st_bbox(usa_counties))


# ------ 4. Calculate Overlay Weights ------

# This function calculates how much each grid cell overlaps with each polygon,
# optionally weighting by a secondary raster (e.g., population density).
county_weights <- overlay_weights(
  polygons = usa_counties,
  polygon_id_col = "GID_2",
  grid = era5_grid,
  secondary_weights = pop_world_2015_era5
)

# ------ 5. Verification of Overlay Weights ------

# Check for NA values in the calculated weights.
# There might be some NAs due to the great lakes etc, see plot...
# ... below to explore 
# na_count <- sum(is.na(county_weights))
# print(paste("Number of NA values in county_weights:", na_count))

# Print dimensions and a preview of the `county_weights` data for verification.
# print("County weights dimensions:")
# dim(county_weights)
# print("County weights preview:")
# head(county_weights)

# Code to verify there are no major missing values and that all...
# ... weights add up to 1 
# Summarize the total weight per polygon (county).
#weights_summary <- county_weights %>%
#  as.data.frame() %>% # Convert from data.table to data.frame if necessary
#  dplyr::group_by(poly_id) %>%
#  dplyr::summarise(total_weight = sum(weight, na.rm = TRUE))

# Join the summarized weights back to the `usa_counties` spatial dataframe for plotting.
# usa_counties_weights <- usa_counties %>%
#  dplyr::left_join(weights_summary, by = c("GID_2" = "poly_id"))

# Plot the counties, colored by their total overlay weight, to visually verify the weights.
#overlay_plot <- ggplot(usa_counties_weights) +
#  geom_sf(aes(fill = total_weight), color = NA) +
#  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
#  labs(
#    title = "Overlay Weights per US County",
#    fill = "Total Weight"
#  ) +
#  theme_minimal()

# Save the generated plot to a PNG file.
#ggsave(
#  filename = "usa_counties_overlay_weights.png",
#  plot = overlay_plot,
#  width = 10,
#  height = 7,
#  dpi = 300
#)

# ------ 6. Raster Data Preparation and Aggregation ------

# Read the raster data (e.g., temperature data) from the specified TIFF file.
r <- terra::rast(tiff_path)

# Rotate the raster if necessary. This is often required for global datasets
# to align with standard coordinate systems (e.g., -180 to 180 longitude).
r_shifted <- terra::rotate(r)

# Crop the shifted raster to the extent of the USA counties polygons.
# This reduces the data size and focuses processing on the region of interest.
r_crop <- terra::crop(r_shifted, usa_counties)

# Convert temperature values from Kelvin to Celsius (assuming Kelvin input).
r_crop_celsius <- r_crop - 273.15

# Optional: Plot the first band of the processed raster data for visual inspection.
# Uncomment the following lines to generate and save a plot of the first time slice.
# png("raster_plot.png", width = 1000, height = 600)
# plot(r_crop_celsius[[1]], main = "First Time Slice (Celsius)", col = terrain.colors(100))
# dev.off()

# Run `stagg::staggregate_polynomial` to aggregate the raster data to county level.
# This function applies polynomial aggregation based on the calculated overlay weights.
cat("Running stagg::staggregate_polynomial...\n")
temp_out <- stagg::staggregate_polynomial(
  data = r_crop_celsius,
  overlay_weights = county_weights,
  start_date = paste0(1979, "-01-01 00:00:00"), # Ensure start_date matches the year of the input raster data.
  daily_agg = "none", # No daily aggregation, assumes input is already daily or coarser.
  time_agg = "month", # Aggregate data to monthly summaries.
  degree = 4 # Use a 4th-degree polynomial for aggregation.
)


# ------ 7. Testing and Visualization of Aggregated Data ------

# Join the aggregated temperature data back to the `usa_counties` spatial polygons.
# This prepares the data for mapping the aggregated results.
# usa_counties_with_data <- usa_counties %>%
#  dplyr::left_join(temp_out, by = c("GID_2" = "poly_id"))

# Filter the data to plot a specific year and month (e.g., January of the first available year).
# Adjust `min(year)` and `month == 1` as needed to visualize different time slices.
#plotting_data <- usa_counties_with_data %>%
#  dplyr::filter(year == min(year, na.rm = TRUE), month == 1)

# Create a plot of the aggregated data for the selected time slice.
# `order_1` represents the first-order polynomial term (often the mean or primary effect).
#test_plot <- ggplot(data = plotting_data) +
#  geom_sf(aes(fill = order_1), color = NA) + # Fill polygons based on the 'order_1' polynomial term.
#  scale_fill_viridis_c() + # Use a continuous color scale (Viridis is perceptually uniform).
#  labs(
#    title = paste("Aggregated TAS (Order 1) for January,", min(plotting_data$year, na.rm = TRUE)),
#    fill = "TAS Value"
#  ) +
#  theme_minimal()

# Save the generated test plot to a PNG file.
#ggsave(filename = "test_plot.png", plot = test_plot)


# ------ 8. Data Exploration and Diagnostics ------

# Filter the aggregated output to identify any counties with missing (NA) year or month values.
# This can indicate issues during the aggregation process for specific polygons.
df_filtered <- temp_out %>%
  dplyr::filter(is.na(year) | is.na(month))

# Print a diagnostic message if NA values are found.
if (nrow(df_filtered) > 0) {
  message(
    paste(
      "Warning: Found", nrow(df_filtered),
      "entries in temp_out with NA values for year or month. These should be explored."
    )
  )
  print(head(df_filtered))
} else {
  message("No NA values found for year or month in the aggregated output.")
}