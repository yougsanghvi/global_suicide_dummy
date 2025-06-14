# If working on remote compute, must load module r-spatial and netcdf (use 'module load r-spatial' and 'module load netcdf-c/4.9.2')
# Install any of the below libraries if not installed already -- add code to do this

# ------ install and load required libraries ------
# Helper function to install CRAN packages
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", ...)
  }
  library(pkg, character.only = TRUE)
}

# Install CRAN packages
install_if_missing("sf")
install_if_missing("stars")

# Install stagg from GitHub
if (!requireNamespace("stagg", quietly = TRUE)) {
  install_if_missing("remotes") # Ensure remotes is installed
  remotes::install_github("tcarleton/stagg")
}
library(stagg)

# Install ncdf4 with specific configure arguments, can check these with 'which nc-config' in bash terminal 
# IMPORTANT: You MUST run 'module load netcdf-c/4.9.2' in your terminal BEFORE running this R script.
nc_config_path <- "/global/software/rocky-8.x86_64/gcc/linux-rocky8-x86_64/gcc-11.4.0/netcdf-c-4.9.2-4au53ukmrllskvcwqexja4lp44lxt6fo/bin/nc-config"
install_if_missing(
  "ncdf4",
  configure.args = paste0("--with-nc-config=", nc_config_path)
)

dir <- "/global/scratch/users/yougsanghvi"

world_county_dir <- file.path(dir, "gadm36_levels_shp")
world_county_filename <- "gadm36_2.shp"
world_county_path <- file.path(world_county_dir, world_county_filename)

world_counties <- st_read(world_county_path)
usa_counties <- world_counties[
  world_counties$NAME_0 %in%
    c("United States of America", "USA", "United States"),
]

usa_counties <- st_crop(
  usa_counties,
  xmin = -125,
  ymin = 24,
  xmax = -66,
  ymax = 50
)

sf::st_transform(usa_counties, 4326)
usa_counties <- st_make_valid(usa_counties)
print(st_bbox(usa_counties))

county_weights <- overlay_weights(
  polygons = usa_counties,
  polygon_id_col = "GID_2",
  grid = era5_grid,
  secondary_weights = pop_world_2015_era5
)

gdnat_nc1_filename <- "gdnat1_1979.nc"
nc1_path <- file.path(dir, gdnat_nc1_filename)

gdnat_nc2_filename <- "gdnat2.nc"
nc2_path <- file.path(dir, gdnat_nc2_filename)

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