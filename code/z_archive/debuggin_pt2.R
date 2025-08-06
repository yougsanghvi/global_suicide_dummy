source("code/y_utils/config.R")

library(tidyverse)    # For dplyr, ggplot2, tibble, etc.
library(terra)        # For reading and working with rasters
library(sf)           # For shapefiles and spatial vector operations
library(lubridate)    # For working with dates
library(stagg)        # For pop_world_2015_era5 and aggregation functions
library(ggplot2)      # For plotting
library(scales)       # For scales in ggplot2
library(ncdf4) # For reading NetCDF files
library(data.table) # For fread and data.table operations
library(dplyr) # For data manipulation

# loading files

# Load USA shapefile
usa_shp <- sf::st_read(USA_COUNTY_SHAPEFILE_FP)

# Load ERA5 county aggregated data (with polynomial temperature variables)
era5_county_df <- readr::read_csv(ERA5_COUNTY_AREA_FP)
era5_shasta <- subset(era5_county_df, poly_id == 6089 & year >= 1979 & year <= 2004)

# 1. Extract Shasta polygon from USA shapefile (assuming GEOID is character)
shasta_poly <- usa_shp %>% filter(GEOID == "06089")

# 2. Convert pop_world_2015_era5 to sf points (assuming it's data.table or data.frame)
pop_sf <- st_as_sf(
  stagg::pop_world_2015_era5,
  coords = c("x", "y"),
  crs = st_crs(usa_shp) # or EPSG:4326
)

# 3. Find points within Shasta polygon
pop_shasta <- st_join(pop_sf, shasta_poly, join = st_within, left = FALSE)

# 4. Extract lon/lat coords of points inside Shasta polygon
shasta_coords <- st_coordinates(pop_shasta)

# Optional: convert back to data.frame with weights for filtering ERA5 read
shasta_pop_points <- data.frame(
  lon = shasta_coords[,1],
  lat = shasta_coords[,2],
  weight = pop_shasta$weight
)

# View number of points inside Shasta polygon
message(sprintf("Number of pop grid cells overlapping Shasta: %d", nrow(shasta_pop_points)))

# Function to find nearest index for a value in a vector
nearest_index <- function(vec, val) {
  which.min(abs(vec - val))
}

# Assume shasta_pop_points is available (from previous step)
# Read lon and lat from one ERA5 file (1979) to get grid coordinates
nc_sample <- nc_open(get_era5_daily_yearly(1979))
lon_all <- ncvar_get(nc_sample, "longitude") # length 1440
lat_all <- ncvar_get(nc_sample, "latitude")  # length 721
nc_close(nc_sample)

# Find indices for each Shasta grid point
shasta_pop_points <- shasta_pop_points %>%
  rowwise() %>%
  mutate(
    lon_idx = nearest_index(lon_all, lon),
    lat_idx = nearest_index(lat_all, lat)
  ) %>%
  ungroup()

# Now read ERA5 data for each year only for those indices
years <- 1979:2004
era5_data_list <- list()

for (yr in years) {
  nc_path <- get_era5_daily_yearly(yr)
  nc <- nc_open(nc_path)
  
  time <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  start_date <- as.Date(sub("days since ", "", time_units))
  dates <- start_date + time
  
  # Initialize matrix to hold data: rows=time, cols=grid points
  t2m_sub <- matrix(NA, nrow=length(time), ncol=nrow(shasta_pop_points))
  
  # Loop over each Shasta grid cell index and read its time series
  for (i in seq_len(nrow(shasta_pop_points))) {
    lon_i <- shasta_pop_points$lon_idx[i]
    lat_i <- shasta_pop_points$lat_idx[i]
    
    # Note: ERA5 stores t2m as [lon, lat, time]
    # so we read a slice: lon_i, lat_i, all time
    vals <- ncvar_get(nc, "t2m", start = c(lon_i, lat_i, 1), count = c(1, 1, -1))
    
    t2m_sub[, i] <- vals
  }
  
  nc_close(nc)
  
  era5_data_list[[as.character(yr)]] <- list(
    t2m = t2m_sub,
    dates = dates,
    lon = shasta_pop_points$lon,
    lat = shasta_pop_points$lat
  )
  
  message(sprintf("Loaded ERA5 Shasta grids for year %d", yr))
}

# For each year, compute mean temp per grid cell across all times
mean_temps_per_year <- lapply(era5_data_list, function(data) {
  # t2m array subset already has filtered lon_idx and lat_idx
  t2m <- data$t2m  # dims: lon x lat x time
  
  # Compute mean over time (3rd dim) for each lon-lat cell
  apply(t2m, c(1,2), mean, na.rm=TRUE)
})

# Combine all years' t2m data by row-binding daily temps across years
all_temps_matrix <- do.call(rbind, lapply(era5_data_list, function(x) x$t2m))
# all_temps_matrix: rows = total days across all years, cols = grid cells (~16)

# Calculate mean temperature per grid cell (average over time)
mean_temps_kelvin <- colMeans(all_temps_matrix, na.rm = TRUE)

# Convert to Celsius
mean_temps_celsius <- mean_temps_kelvin - 273.15

# Check length should be number of grid cells (~16)
length(mean_temps_celsius)



# 1. Create a dataframe for mean temps with lon, lat
mean_temp_df <- data.frame(
  lon = shasta_pop_points$lon,
  lat = shasta_pop_points$lat,
  mean_temp_c = mean_temps_celsius
)

# 2. Get the full pop_world_2015_era5 data (with lon=x, lat=y, weight)
pop_df <- stagg::pop_world_2015_era5 %>%
  rename(lon = x, lat = y)

# 3. Inner join mean temps to pop weights on lon & lat (exact match)
merged_df <- inner_join(mean_temp_df, pop_df, by = c("lon", "lat"))

# 4. Check result
print(merged_df)
