# If working on remote compute, must load module r-spatial and netcdf (use 'module load r-spatial' and 'module load netcdf-c/4.9.2')

# ------ install and load required libraries ------
# Helper function to install and load CRAN packages if they are missing
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", ...)
  }
  library(pkg, character.only = TRUE)
}

# Install and load CRAN packages
install_if_missing("sf")
install_if_missing("stars")
install_if_missing("raster")
install_if_missing("sp")

# Install stagg from GitHub if it's not already installed
if (!requireNamespace("stagg", quietly = TRUE)) {
  install_if_missing("remotes") # Ensure remotes is installed to use install_github
  remotes::install_github("tcarleton/stagg")
}
library(stagg)

# Install ncdf4 with specific configure arguments if needed for your system
# This is crucial for reading NetCDF files like ERA5 on some HPC systems.
# IMPORTANT: You MUST run 'module load netcdf-c/4.9.2' in your terminal BEFORE running this R script.
if (!requireNamespace("ncdf4", quietly = TRUE)) {
    nc_config_path <- "/global/software/rocky-8.x86_64/gcc/linux-rocky8-x86_64/gcc-11.4.0/netcdf-c-4.9.2-4au53ukmrllskvcwqexja4lp44lxt6fo/bin/nc-config"
    install_if_missing(
      "ncdf4",
      configure.args = paste0("--with-nc-config=", nc_config_path)
    )
} else {
    library(ncdf4)
}


# --- Setting working directories ---
dir <- "/global/scratch/users/yougsanghvi"
world_county_dir <- file.path(dir, "gadm36_levels_shp")
world_county_filename <- "gadm36_2.shp"
world_county_path <- file.path(world_county_dir, world_county_filename)


# --- Reading and preparing polygon data ---
world_counties <- st_read(world_county_path)

# Filter for United States counties
usa_counties <- world_counties[
  world_counties$NAME_0 %in%
    c("United States of America", "USA", "United States"),
]

# Ensure CRS is WGS84 (lon/lat) and fix any invalid geometries
usa_counties <- sf::st_transform(usa_counties, 4326)

print("Bounding box of US Counties after transform:")
print(st_bbox(usa_counties))

usa_counties <- st_make_valid(usa_counties)
print("Bounding box of US Counties after make valid:")
print(st_bbox(usa_counties))

# Crop to the continental US to reduce processing time and avoid issues with Alaska/Hawaii
bbox_poly <- st_as_sfc(st_bbox(c(xmin = -140, ymin = 20, xmax = -50, ymax = 50), crs = st_crs(usa_counties)))
usa_counties <- st_filter(usa_counties, bbox_poly)

print("Bounding box of US Counties after cropping:")
print(st_bbox(usa_counties))


# --- Calculate overlay weights with corrected grids ---
# This function calculates how much each grid cell overlaps with each polygon,
# optionally weighting by a secondary raster (like population).
county_weights <- overlay_weights(
  polygons = usa_counties,
  polygon_id_col = "GID_2",
  grid = era5_grid_corrected, # Use the corrected grid
  secondary_weights = pop_world_2015_era5 # Use the corrected population grid
)


# --- Verification ---
# Check for NAs. This should now be 0 or a very small number.
na_count <- sum(is.na(county_weights))
print(paste("Number of NA values in county_weights:", na_count))
print("county weights dimensions: ")
dim(county_weights)
print("county weights preview: ")
head(county_weights)

# plotting to verify 

library(dplyr)
library(ggplot2)
library(sf)

# Summarize total weight per polygon (county)
weights_summary <- county_weights %>%
  as.data.frame() %>%  # convert from data.table to data.frame if needed
  group_by(poly_id) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE))

# Join summarized weights back to usa_counties spatial dataframe
usa_counties_weights <- usa_counties %>%
  left_join(weights_summary, by = c("GID_2" = "poly_id"))

# Plot the counties colored by total overlay weight
ggplot(usa_counties_weights) +
  geom_sf(aes(fill = total_weight), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Overlay Weights per US County",
    fill = "Total Weight"
  ) +
  theme_minimal()

ggsave(
  filename = "usa_counties_overlay_weights.png",
  plot = last_plot(),  # saves the last plot created
  width = 10,          # width in inches
  height = 7,          # height in inches
  dpi = 300            # resolution
)

# all counties should sum to 1

# ----- STEP 2 -------

gdnat_nc_filename <- "gdnat_stagg_1979.nc"
nc1_path <- file.path(dir, gdnat_nc_filename)
nc_data <- nc_open(nc1_path)

#sf::gdal_utils("info", nc1_path)

print("opening and cropping nc file")
terra::rast(nc1_path, drivers = "netcdf")



 |>
  terra::crop(usa_counties)  # Assuming usa_counties is already defined




# Convert from K to C
cat("Converting from K to C\n")
clim_raster_tmp <- clim_raster_tmp - 273.15

# Run staggregate_polynomial
cat("Running stagg::staggregate_polynomial\n")
temp_out <- stagg::staggregate_polynomial(
  data = clim_raster_tmp,
  overlay_weights = county_weights,
  start_date = paste0(1979, "-01-01 00:00:00"),  # match file year
  daily_agg = "none",
  time_agg = "month",
  degree = 4
)



# Step 2 

gdnat_nc1_filename <- "gdnat_squeezed_1979.nc"
nc1_path <- file.path(dir, gdnat_nc1_filename)

# loading in nc1

variable_name <- "tas"

nc_data <- nc_open(nc1_path)

data_array <- ncvar_get(nc_data, variable_name)
lon_vals <- ncvar_get(nc_data, "lon")
lat_vals <- ncvar_get(nc_data, "lat")
time_vals_raw <- ncvar_get(nc_data, "time")

time_units <- ncatt_get(nc_data, "time", "units")$value
time_origin <- sub(".*since ", "", time_units)
time_dates <- as.POSIXct(time_vals_raw, origin = time_origin, tz = "GMT")

nc_close(nc_data)

# Assuming data_array dimensions are [lon, lat, time, model] in R
# based on common ncdf4 behavior and your xarray output.
# This selects the first (and only) slice of the 'model' dimension.
clim_data_final <- data_array

ext <- terra::ext(min(lon_vals), max(lon_vals), min(lat_vals), max(lat_vals))

clim_raster_tmp <- terra::rast(clim_data_final, ext = ext, crs = "EPSG:4326")

terra::time(clim_raster_tmp) <- time_dates

print(clim_raster_tmp)

# Rotate the raster's longitude range from 0-360 to -180-180
clim_raster_rotated <- terra::rotate(clim_raster_tmp)

clim_raster_cropped <- terra::crop(clim_raster_rotated, usa_counties)

# convert from K to C
cat("Converting from K to C\n")
clim_raster_tmp <- clim_raster_tmp - 273.15

poly_order <- 4
first_date <- min(terra::time(clim_raster_cropped))
time_interval <- '1 day'
temp_out <- stagg::staggregate_polynomial(
  data = clim_raster_cropped,
  overlay_weights = county_weights,
  daily_agg = 'none',
  time_agg = 'month',
  degree = poly_order, 
  start_date = first_date, # <--- Add this
  time_interval = time_interval_hours
)


# ------- Testing -------- #

library(dplyr)
library(ggplot2)

# Join the aggregated data to the spatial polygons
# 'by' argument specifies the matching columns in both data frames
usa_counties_with_data <- usa_counties %>%
  left_join(temp_out, by = c("GID_2" = "poly_id"))

# Filter for a specific year and month to plot (e.g., January of the first year)
# Adjust 'min(year)' and 'month == 1' as needed for your desired slice of time
plotting_data <- usa_counties_with_data %>%
  filter(year == min(year, na.rm = TRUE), month == 1)

# Create the plot
# Replace 'order_1' with the specific polynomial term you want to visualize (e.g., 'order_2', 'order_3', 'order_4')
ggplot(data = plotting_data) +
  geom_sf(aes(fill = order_1), color = NA) + # 'aes(fill = ...)' colors the polygons
  scale_fill_viridis_c() + # A good continuous color scale
  labs(
    title = paste("Aggregated TAS (Order 1) for January,", min(plotting_data$year, na.rm = TRUE)),
    fill = "TAS Value"
  ) +
  theme_minimal()