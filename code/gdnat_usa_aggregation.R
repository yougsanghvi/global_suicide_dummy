library(stagg)
library(sf)
library(stars)
library(ncmeta)

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

gdnat_nc1_filename <- "gdnat1.nc"
nc1_path <- file.path(dir, gdnat_nc1_filename)

gdnat_nc2_filename <- "gdnat2.nc"
nc2_path <- file.path(dir, gdnat_nc2_filename)

# loading in nc1 

clim_raster_tmp <- read_ncdf(nc1_path)
clim_raster_tmp <- st_crop(clim_raster_tmp, usa_counties)
    
# convert from K to C
cat("Converting from K to C\n")
clim_raster_tmp <- clim_raster_tmp - 273.15

poly_order <- 4

temp_out <- stagg::staggregate_polynomial(
      data = clim_raster_tmp,
      overlay_weights = county_weights,
      daily_agg = 'average',
      time_agg = 'month',
      degree = poly_order
    )

# loading in nc2

clim_raster_tmp <- terra::rast(nc2_path) |> 
      terra::crop(terra::vect(usa_counties))
    
    # convert from K to C
    cat("Converting from K to C\n")
    clim_raster_tmp <- clim_raster_tmp - 273.15

poly_order <- 4

temp_out <- stagg::staggregate_polynomial(
      data = clim_raster_tmp,
      overlay_weights = county_weights,
      time_agg = 'month',
      daily_agg = 'average',
      degree = poly_order
    )
