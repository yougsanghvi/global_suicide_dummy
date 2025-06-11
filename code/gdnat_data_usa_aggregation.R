# Load required libraries
library(stagg)
library(terra) # For SpatRaster objects
library(sf)    # For spatial (sf) objects
library(data.table) # For data.table objects, often used with stagg

# --- Placeholder for your input data ---
# In a real scenario, you would load your data here.
# For demonstration, we'll create simple dummy data structures.

# 1. 'polygons': An sf object representing your administrative units
#    (e.g., counties). This should have a column that uniquely identifies
#    each polygon (e.g., GID_0).
#    Replace this with: sf::st_read("path/to/your/shapefile.shp")
polygons <- sf::st_as_sf(data.frame(
  GID_0 = c("US.1", "US.2"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))),
    sf::st_polygon(list(cbind(c(1, 2, 2, 1, 1), c(1, 1, 2, 2, 1))))
  )
))
polygon_id_col <- "GID_0" # Name of the ID column in your 'polygons' sf object

# 2. 'pop_weights_data': A data.frame or data.table containing your secondary
#    weights (e.g., population weights per grid cell).
#    It must have 'x', 'y' (coordinates matching your climate data grid),
#    and 'weight' columns.
#    Replace this with: data.table::fread("path/to/your/population_weights.csv")
pop_weights_data <- data.table::data.table(
  x = c(0.5, 1.5),
  y = c(0.5, 1.5),
  weight = c(0.7, 0.9) # Example population weights
)

# 3. 'climate_raster': A SpatRaster object containing your climate data.
#    This should be for a specific time period (e.g., a month or year),
#    and *already converted to Celsius if that's your desired unit*.
#    Replace this with: terra::rast("path/to/your/era5_temp_YEAR.nc")
#    And ensure it's cropped and converted to Celsius:
#    climate_raster <- terra::rast("path/to/era5_temp_2015.nc")
#    climate_raster <- climate_raster - 273.15 # Convert K to C
#    climate_raster <- terra::crop(climate_raster, terra::vect(polygons)) # Crop
climate_raster <- terra::rast(
  ncols = 2, nrows = 2,
  xmin = 0, xmax = 2, ymin = 0, ymax = 2,
  vals = c(20, 21, 22, 23) # Example temperature values in Celsius
)
names(climate_raster) <- paste0("temp_", 1:4) # Example band names


# 4. 'start_date': The start date of your climate data.
#    Format should be "YYYY-MM-DD HH:MM:SS".
start_date <- "2015-01-01 00:00:00"

# 5. 'poly_degree': The polynomial degree for aggregation.
poly_degree <- 4

# --- Core stagg aggregation ---

# Step 1: Get the overlay weights
# This calculates how much each grid cell contributes to each polygon,
# incorporating the population weights.
message("Calculating overlay weights...")
weights <- stagg::overlay_weights(
  polygons = polygons,
  polygon_id_col = polygon_id_col,
  secondary_weights = pop_weights_data
)

# Step 2: Perform the spatiotemporal aggregation
# This applies the calculated weights and aggregates the climate data
# for each polygon over the specified time period using a polynomial function.
message("Performing spatiotemporal aggregation...")
aggregated_data <- stagg::staggregate_polynomial(
  data = climate_raster,
  overlay_weights = weights,
  start_date = start_date,
  daily_agg = 'average', # How to aggregate daily (e.g., 'average', 'sum')
  time_agg = 'month',    # How to aggregate temporally (e.g., 'month', 'year')
  degree = poly_degree   # Degree of the polynomial to fit
)

# --- Result ---
# The 'aggregated_data' data.table now contains your cleaned, aggregated data.
# You can now view it or perform further analysis.
print(aggregated_data)

# You would typically save this data, but for this minimal example, we just print.
# Example saving (commented out as per your request for no file management initially):
# data.table::fwrite(aggregated_data, file = "my_aggregated_temp_data.csv")
