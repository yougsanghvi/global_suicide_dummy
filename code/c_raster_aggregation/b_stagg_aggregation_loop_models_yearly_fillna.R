# ==============================================================================
# ERA5 / GDNat Climate Data Spatial-Temporal Aggregation Script
# ==============================================================================
# Purpose: Aggregate climate raster data to monthly county-level 
#          averages with polynomial transformations for US counties (1979–2020)
# Prerequisites: Load r-spatial module ('module load r-spatial')
# ==============================================================================

overlay_weights_adj <- function(polygons, polygon_id_col, grid = era5_grid, secondary_weights = NULL){

  ## check to make sure climate raster is a spatraster, change if not
  if (!inherits(grid, "SpatRaster")) {
    clim_raster <- terra::rast(grid)
  } else {

    clim_raster <- grid

  }

  ## Raster cell area
  ## -----------------------------------------------
  clim_area_raster <- terra::cellSize(clim_raster, unit = "km")

  ## Raster/polygon alignment
  ## -----------------------------------------------

  message(crayon::yellow('Checking for raster/polygon alignment'))

  ## polygon and raster xmin and xmax values
  poly_xmax <- terra::ext(polygons)$xmax
  rast_xmax <- terra::ext(clim_area_raster)$xmax
  rast_res <-  terra::xres(clim_area_raster)

  ## check if SpatRaster is in geographic coodrinates
  if(!terra::is.lonlat(clim_raster)) {
    stop(crayon::red('Grid does not have geographic coordinates.'))

  }

 ## stop if polygons are not in standard coordinate system
 if(poly_xmax > 180) {

   stop(crayon::red('Polygons must be in standard coordinate system (longitude -180 to 180).'))

 }

  ## check if coordinate systems match, if no shift raster to -180 to 180
  if(rast_xmax > 180 + rast_res) {

    # Make sure the cell widths aren't peculiar otherwise the rotate function will
    # mess things up
    if(360 %% terra::xres(clim_raster) != 0){
      stop(crayon::red('Grid is in climate coordinate system (longitude 0 to 360) and grid cell width does not divide 360 evenly, making accurate alignment impossible.'))
    }

    message(crayon::yellow('Aligning longitudes to standard coordinates.'))

    ## xmin for climate raster
    rast_xmin <- terra::ext(clim_area_raster)$xmin

    ## check if raster needs to be padded, extend if needed
    if(!dplyr::near(rast_xmin, 0, tol = rast_res) | !dplyr::near(rast_xmax, 360, tol = rast_res)) {

      ## create global extent for padding so rotate function can be used
      global_extent <- terra::ext(0, 360, -90, 90)

      ## pad
      clim_area_raster <- terra::extend(clim_area_raster, global_extent)

    }

    ## rotate
    clim_area_raster <- terra::rotate(clim_area_raster)

  }

  # Extend the grid to cover all polygons consistent with the extended rotate
  # above. exact_extract already does this in the background to a certain
  # degree, so this just allows us to be explicit about how we handle NAs later
  # on.
  clim_area_raster <- terra::extend(clim_area_raster, terra::ext(polygons), snap = 'out')

  ## Match raster and polygon crs
  crs_raster <- terra::crs(clim_area_raster)
  polygons_reproj <- sf::st_transform(polygons, crs = crs_raster)

  ## Raster / Polygon overlap (using data.table)
  ## -----------------------------------------------
  message(crayon::green('Extracting raster polygon overlap'))

  overlap <- data.table::rbindlist(exactextractr::exact_extract(clim_area_raster, polygons_reproj, progress = F, include_xy = T), idcol = "poly_id")
  overlap[, ':=' (poly_id = polygons_reproj[[polygon_id_col]][poly_id], cell_area_km2 = value)] # Add the unique id for each polygon based on the input col name


  ## Calculate weights
  ## -----------------------------------------------

  # Calculate area weight per grid cell
  area_weight <- overlap[, .(x, y, poly_id, w_area = coverage_fraction * cell_area_km2)] # area weight = area km2 * coverage fraction

  # IF weights = TRUE, merge secondary weights with area weights
  if(!is.null(secondary_weights)){

    # data.table of secondary weights
    weights_dt <- data.table::as.data.table(secondary_weights)

    ## make sure secondary_weights is not in climate 0-360 coordinates
    s_weight_max <- max(weights_dt$x)

    ## if secondary_weights is in 0-360, adjust x val
    if(s_weight_max > 180 + rast_res / 2) {

      message(crayon::yellow('Adjusting secondary weights longitude to standard coordinates.'))

      weights_dt[, x := data.table::fifelse(x > 180 + rast_res / 2, x - 360, x)]

    }

    ## check if secondary weights fully overlaps with area_weight df
    covers <- min(weights_dt$x) <= min(area_weight$x) &&
              max(weights_dt$x) >= max(area_weight$x) &&
              min(weights_dt$y) <= min(area_weight$y) &&
              max(weights_dt$y) >= max(area_weight$y)

    if (covers) {
      message(crayon::green('Secondary weights fully overlap with the administrative regions.'))
    } else {
      warning(crayon::red('Warning: secondary weights do not fully overlap with the administrative regions. Resulting weights will contain NAs.'))
    }


  ## check if secondary weights table contains NA values
  if(isTRUE(any(is.na(weights_dt[["weight"]])))) {

    ## print warning if there are NAs in the secondary weights
    warning(crayon::red("Warning: secondary weight values contain one or more NAs. The resulting weights for x,y coordinates with NA secondary weight values will be NAs."))

  }

    # Set key column in the merged dt table
    keycols = c("x", "y")
    data.table::setkeyv(area_weight, keycols)

    # Merge with secondary weights, NA for missing values
    w_merged <- merge(area_weight, weights_dt,
                      by = c('x', 'y'),
                      all.x = T)

    # ----- debuggin code to be removed later -------------------------------------------------------------------------

    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
    }
    if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
    install.packages("rnaturalearthdata")
    }

    library(ggplot2)
    library(crayon)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(sf)

    # Step 1: Identify NA grid cells
    na_cells <- w_merged[is.na(weight), .(x, y)]

    # Step 2: Print count
    message(crayon::yellow(sprintf("Number of raster cells with NA secondary weights: %d", nrow(na_cells))))

    # Step 3: Convert to data.frame for ggplot
    na_grid_df <- as.data.frame(na_cells)

    # Step 4: Define bounding box for mainland USA
    bbox <- list(
    xmin = -125,
    xmax = -66.5,
    ymin = 24,
    ymax = 50
    )

    # Step 5: Load landmass and lakes layers
    world <- ne_countries(scale = "medium", returnclass = "sf")
    print("loading lakes...")
    lakes <- ne_download(scale = "medium", type = "lakes", category = "physical", returnclass = "sf")
    print("making lakes valid...")
    lakes <- sf::st_make_valid(lakes)

    # Optional: Crop lakes to mainland USA
    print("cropping lakes...")
    lakes_us <- sf::st_crop(lakes, xmin = bbox$xmin, xmax = bbox$xmax, ymin = bbox$ymin, ymax = bbox$ymax)

    # Step 6: Plot everything
    p <- ggplot() +
        geom_sf(data = world, fill = "gray95", color = NA) +               # land background
        geom_sf(data = lakes_us, fill = "lightblue", color = NA) +         # lakes
        geom_sf(data = usa_counties, fill = NA, color = "black", size = 0.1) +  # county outlines
        geom_point(data = na_grid_df, aes(x = x, y = y), color = "red", size = 0.5) +  # NA grid points
        coord_sf(xlim = c(bbox$xmin, bbox$xmax),
                ylim = c(bbox$ymin, bbox$ymax),
                expand = FALSE) +
        theme_minimal() +
        labs(title = "Grid Cells with NA Secondary Weights",
            subtitle = "With County Boundaries and Lakes",
            x = "Longitude", y = "Latitude")

    # Step 7: Save plot
    ggsave("na_grid_with_counties_and_lakes.png", plot = p, width = 8, height = 5, dpi = 300)

    # Step 8: Confirm
    message(crayon::green("Saved final NA grid plot with counties and lakes to 'na_grid_with_counties_and_lakes.png'"))



    # ------- end of debugging code -------

    # Adjust weights that are NA to make sure that weights are returned (otherwise all NA for large polygons)
    w_merged <- w_merged |> dplyr::mutate(weight = ifelse(is.na(weight),0,weight))

    # Weight in pixel = w_area * weight
    w_merged[, weight := weight * w_area]

    # Create column that determines if entire polygon has a weight == 0
    zero_polys <- w_merged[, .(sum_weight = sum(weight)),
                                by = .(poly_id)]

    zero_polys <- unique(zero_polys[sum_weight == 0, .(poly_id)])

    if(nrow(zero_polys) > 0) {

      warning(crayon::red("Warning: weight = 0 for all pixels in some of your polygons; NAs will be returned for weights"))

    }

    # List any polygons with NA values in 1 or more grid cells
    na_polys <- unique(w_merged[is.na(weight), .(poly_id)])

    # # Warning if there are polygons with NA weight values
    # if(nrow(na_polys > 0)) {
    #
    #   warning(crayon::red("Warning: some of the secondary weights are NA, meaning weights cannot be calculated. NAs will be returned for weights."))
    #
    # }

    # Update the weight to NA for all grid cells in na_polys
    w_merged <- w_merged[, weight := ifelse(poly_id %in% c(na_polys$poly_id,
                                                           zero_polys$poly_id), NA, weight)]

  }

  # Normalize weights by polygon
  if(!is.null(secondary_weights)){

    w_norm <- data.table::copy(w_merged)

    w_norm <- w_norm[, ':=' (w_area = w_area / sum(w_area), weight = weight / sum(weight)), by = poly_id]


  } else {

    w_norm <- data.table::copy(area_weight)

    w_norm <- w_norm[, w_area := w_area / sum(w_area), by = poly_id]
  }


  message(crayon::yellow('Checking sum of weights within polygons'))
  if(!is.null(secondary_weights)){

    check_weights <- w_norm[, lapply(.SD, sum, na.rm = T), by = poly_id,
                            .SDcols = c('w_area', 'weight')]

  } else {
    check_weights <- w_norm[, w_sum := sum(w_area), by=poly_id]
  }

  # Check that polygon weights sum to 1 or 0 if all weights are NA
  if (!is.null(secondary_weights)){

    for(i in seq_len(nrow(check_weights))){

      if(!dplyr::near(check_weights$w_area[i], 1, tol=0.001)) {

        stop(crayon::red('Area weights for polygon', check_weights$poly_id[i], 'do not sum to 1')) }

        if(!check_weights$poly_id[i] %in% c(na_polys$poly_id, zero_polys$poly_id) & !dplyr::near(check_weights$weight[i], 1, tol=0.001)) {

          stop(crayon::red('Weights for polygon', check_weights$poly_id[i], 'do not sum to 1')) }

     }


    } else {

    for(i in seq_len(nrow(check_weights))){

      if(!dplyr::near(check_weights$w_sum[i], 1, tol=0.001)){

        stop(crayon::red('Area weights for polygon', check_weights$poly_id, 'do not sum to 1'))

      }

    }
  }



  # If it doesn't error out then all weight sums = 1
  message(crayon::green('All weights sum to 1.'))

  ## Return table in coordinate system that matches that of the climate data
  ## ------------------------------------------------------------------------

  if(rast_xmax > 180 + rast_res) {

    w_norm[, x := data.table::fifelse(x < 0 + rast_res / 2, x + 360, x)]

  }


  return(w_norm)

}

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

cat("loading required libraries \n")
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(crayon)
library(tictoc)
library(remotes)
library(stagg)
library(pryr)
library(tidyr)
library(lubridate)

cat(crayon::green("All required packages loaded.\n"))

print_memory <- function(label = "") {
  gc()
  used <- pryr::mem_used()
  mem_str <- if (used > 1e9) sprintf("%.2f GB", used / 1e9) else if (used > 1e6) sprintf("%.2f MB", used / 1e6) else sprintf("%.2f kB", used / 1e3)
  cat(sprintf("[%s] Memory used: %s\n", label, mem_str))
}

# ------ 2. CONFIGURATION ------

print_memory("Start of script")

# File paths
dir <- "/global/scratch/users/yougsanghvi"
DATA_SOURCE <- "era5" 
is_era5 <- DATA_SOURCE == "era5"

# Get year from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  year <- as.numeric(args[1])
  cat(crayon::blue(sprintf("Processing year %d from command line argument\n", year)))
} else {
  year <- 1979
  cat(crayon::yellow("No command line argument provided, using default year 1979\n"))
}
if (is.na(year) || year < 1979 || year > 2004) stop("Invalid year provided. Must be between 1979 and 2020.")

overwrite <- TRUE
pop_weight <- TRUE

# Shapefile setup
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Load US counties
cat(crayon::blue("Loading and preparing county shapefile...\n"))
usa_counties <- st_read(usa_county_path, quiet = TRUE)
print_memory("After loading USA counties shapefile")

current_data_path <- get_era5_agg_yearly(year)
# check if path exists
if (!file.exists(current_data_path)) {
  print("file not found")}
existing_data <- read.csv(current_data_path)

processed_counties <- unique(existing_data$poly_id_int)
usa_counties$GEOID <- as.integer(usa_counties$GEOID)
all_counties <- usa_counties$GEOID
missing_counties <- setdiff(all_counties, processed_counties)

usa_counties <- usa_counties[usa_counties$GEOID %in% missing_counties, ]


# ---- debugging code, remove soon




# ------ end of debugging code






# Overlay weights
cat(crayon::blue("Calculating overlay weights...\n"))
if (pop_weight) {
  county_weights <- overlay_weights_adj(usa_counties, "GEOID", secondary_weights = pop_world_2015_era5)
} else {
  county_weights <- stagg::overlay_weights(usa_counties, "GEOID")
}
print_memory("After calculating overlay weights")

# ------ 3. MODEL LOOP (for GDNat) OR Single-run (ERA5) ------

if (is_era5) {
  model_list <- c("ERA5")
} else {
  model_list <- c("MIROC6", "ACCESS-ESM1-5", "CanESM5", "MRI-ESM2-0", "NorESM2-LM")
}

for (model_name in model_list) {

  cat(crayon::blue(sprintf("\n==================== Processing: %s (year %d) ====================\n", model_name, year)))

  if (is_era5) {
    dir_tiff <- ERA5_RAW_FOLDER
    filename_pattern <- "era5_data_%d.grib"
    file_format_prefix <- "era5_usa_agg_fillna"
    output_dir <- ERA5_AGG_FOLDER
    daily <- FALSE
  } else {
    filename_pattern <- "gdnat_%d.tif"
    dir_tiff <- file.path(GDNAT_TIFF_DIR, model_name)
    file_format_prefix <- paste0("gdnat_usa_agg_fillna", model_name, "_")
    output_dir <- file.path(GDNAT_DIR, "aggregated", "usa_pop_county", model_name)
    daily <- TRUE
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }

  output_filepath_all_years <- file.path(output_dir, paste0(file_format_prefix, "all_years.csv"))
  current_filename <- sprintf(filename_pattern, year)
  current_filepath <- file.path(dir_tiff, current_filename)
  output_filename <- sprintf("%s%d.csv", file_format_prefix, year)
  output_filepath <- file.path(output_dir, output_filename)

  # Check if input exists
  if (!file.exists(current_filepath)) stop(sprintf("Input file not found: %s", current_filepath))
  if (!overwrite && file.exists(output_filepath)) {
    message(sprintf("Output already exists: %s", output_filepath))
    next
  }

  print_memory("Before reading raster data")
  cat(sprintf("Reading raster data for %d...\n", year))
  r <- terra::rast(current_filepath)
  print_memory("After reading raster data")

  if (terra::xmax(r) > 190) {
    print("Correcting longitudes...")
    r <- terra::rotate(r)
  }

  cat("Cropping raster to USA extent...\n")
  r_crop <- terra::crop(r, terra::vect(usa_counties))
  r_crop_celsius <- r_crop - 273.15

  cat("Running stagg aggregation...\n")
  tic("Stagg aggregation")
  print_memory("Before stagg aggregation")

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

  toc()
  print_memory("After stagg aggregation")

  temp_out$year <- year
  temp_out <- temp_out %>% drop_na()
  existing_data <- existing_data %>% drop_na()

  # Add number of days per (year, month)
  temp_out$num_days <- days_in_month(ymd(paste(temp_out$year, temp_out$month, "01", sep = "-")))

  # Compute average from sum using correct number of days
  for (deg in 1:4) {
    sum_col <- paste0("order_", deg)
    avg_col <- paste0("order_", deg, "_avg")
    if (sum_col %in% colnames(temp_out)) {
      temp_out[[avg_col]] <- temp_out[[sum_col]] / temp_out$num_days
    }
  }

  temp_out <- as.data.frame(temp_out)
  temp_out$num_days <- NULL
  df_combined <- rbind(existing_data, temp_out)

  write.csv(df_combined, output_filepath, row.names = FALSE)
  cat(crayon::green(sprintf("Saved output: %s\n", output_filepath)))
}
