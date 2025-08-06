# ==============================================================================
# ERA5 Climate Data Spatial-Temporal Aggregation Script
# ==============================================================================
# Purpose: Aggregate hourly ERA5 temperature data to monthly county-level
#          averages with polynomial transformations for US counties (1979-2004)
#
# Prerequisites: Load r-spatial module before running ('module load r-spatial')
# ==============================================================================
options(
  warning.length = 8170, # Increase max length if needed
  warning.expression = quote({
    warning_message <- geterrmessage()
    cat("[WARNING] ", warning_message, "\n", file = stderr())
    flush.console()
  })
)

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

# ------ Adjusting stagg overlay weights -------- #

#' Find the spatial overlap between a raster and a set of polygons
#'
#' The `overlay_weights()` function generates a table of weights mapping
#' each grid cell to its respective polygon(s) for use in the `staggregate_*()`
#' family of functions.
#'
#' @param polygons a simple features polygon or multipolygon object
#' @param polygon_id_col the name of a column in the `polygons` object with a
#'   unique identifier for each polygon
#' @param grid a raster layer with the same spatial resolution as the data
#'   (must use geographic coordinates)
#' @param secondary_weights an optional table of secondary weights, output from
#'   the `secondary_weights()` function
#'
#' @return a data.table of area weights and possibly secondary weights for each
#'   cell within each polygon
#'
#' @examples
#' overlay_output_with_secondary_weights <- overlay_weights(
#'   polygons = tigris::counties("nj"), # Polygons outlining the 21 counties of New Jersey
#'   polygon_id_col = "COUNTYFP", # The name of the column with the unique
#'                                # county identifiers
#'   grid = era5_grid, # The grid to use when extracting area weights (era5_grid is the
#'                     # default)
#'   secondary_weights = cropland_world_2015_era5 # Output from
#'                                                # secondary_weights
#'                                                # (cropland_world_2015_era5 is
#'                                                # available to the# user)
#'   )
#'
#' head(overlay_output_with_secondary_weights)
#'
#'
#'
#' overlay_output_without_secondary_weights <- overlay_weights(
#'   polygons = tigris::counties("nj"), # Polygons outlining the 21 counties of New Jersey
#'   polygon_id_col = "COUNTYFP" # The name of the column with the unique county
#'                               # identifiers
#'   )
#'
#' head(overlay_output_without_secondary_weights)
#'
#'
#' @export
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

# ------ 1. LIBRARY MANAGEMENT ------

# Load required packages (must be pre-installed)
library(sf) # Spatial data handling
library(dplyr) # Data manipulation
library(ggplot2) # Plotting
library(terra) # Raster operations
library(crayon) # Colored console messages
library(tictoc) # Code timing
library(remotes) # For GitHub packages
library(stagg) # Spatial-temporal aggregation

cat(crayon::green("All required packages loaded.\n"))

library(pryr) # for mem_used()

print_memory <- function(label = "") {
  gc()
  used <- pryr::mem_used()
  if (used > 1e9) {
    mem_str <- sprintf("%.2f GB", used / 1e9)
  } else if (used > 1e6) {
    mem_str <- sprintf("%.2f MB", used / 1e6)
  } else {
    mem_str <- sprintf("%.2f kB", used / 1e3)
  }
  cat(sprintf("[%s] Memory used: %s\n", label, mem_str))
}


# ------ 2. CONFIGURATION ------

print_memory("Start of script")

# File paths
# Set data source manually here
DATA_SOURCE <- "era5"  # or "gdnat"
is_era5 <- DATA_SOURCE == "era5"

dir <- "/global/scratch/users/yougsanghvi"
if (is_era5) {
  dir_tiff <- ERA5_RAW_FOLDER
  filename_pattern <- "era5_data_%d.grib"
  file_format_prefix <- "era5_usa_agg_missings_"
  output_dir <- ERA5_AGG_FOLDER
  daily <- FALSE
} else {
  # modify this to represent new per model structure 
  dir_tiff <- file.path(dir, "gdnat_hourly_by_year")
  filename_pattern <- "gdnat_data_%d.nc"
  file_format_prefix <- "gdnat_usa_agg__missings_"
  output_dir <- file.path(dir, "aggregated_results_gdnat_usa")
  daily <- TRUE
}

# Define paths for usa county shapefile.
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Define an output directory for the aggregated results
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

output_filepath_all_years <- file.path(output_dir, paste0(file_format_prefix, "all_years.csv"))

# Processing parameters
# Get year from command line argument or use default
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  year <- as.numeric(args[1])
  cat(crayon::blue(sprintf(
    "Processing year %d from command line argument\n",
    year
  )))
} else {
  year <- 1989 # Default for standalone execution
  cat(crayon::yellow(
    "No command line argument provided, using default year 1979\n"
  ))
}

# Validate year
if (is.na(year) || year < 1979 || year > 2004) {
  stop("Invalid year provided. Must be between 1979 and 2004.")
}

overwrite <- FALSE # Set to FALSE to skip existing files
pop_weight <- TRUE # TRUE: population-weighted, FALSE: area-weighted only
daily <- FALSE # TRUE if data is already daily aggregated

print_memory("After configuration")

# ------ 3. SPATIAL DATA PREPARATION ------

cat(crayon::blue("Loading and preparing county shapefile...\n"))

# Load US county shapefile
usa_counties <- st_read(usa_county_path, quiet = TRUE)

# Ensure consistent CRS (WGS84)
usa_counties <- sf::st_transform(usa_counties, 4326)

# Fix invalid geometries
usa_counties <- st_make_valid(usa_counties)
print(ext(usa_counties))
print(sf::st_crs(usa_counties))

print_memory("After loading USA counties shapefile")

# ------ 4. RASTER PROCESSING AND AGGREGATION ------

cat(crayon::magenta(sprintf("Processing year %d...\n", year)))
cat("\t constructing file paths")
# Construct file paths
current_filename <- sprintf(filename_pattern, year)
current_filepath <- file.path(dir_tiff, current_filename)
output_filename <- sprintf("%s%d.csv", file_format_prefix, year)
output_filepath <- file.path(output_dir, output_filename)

cat("\t checking input file paths")
# Check if input file exists
if (!file.exists(current_filepath)) {
  stop(sprintf("Input file not found: %s", current_filepath))
}

cat("\t checking output file paths \n")
# Check if output file exists and if overwrite is FALSE
if (!overwrite && file.exists(output_filepath)) {
  message(sprintf("Output file already exists: %s", output_filepath))
  message("Set overwrite=TRUE to regenerate or delete the existing file.")
  quit(status = 0)
}

print_memory("before reading raster data")

# Load and process raster data
cat(sprintf("Reading raster data for %d...\n", year))
r <- terra::rast(current_filepath)

print(ext(r))
print(sf::st_crs(r))

print_memory("After reading raster data")

# Rotate longitude if needed (0-360 to -180-180)
if (terra::xmax(r) > 190) {
  print("correcting lat long coordinates")
  r <- terra::rotate(r)
}

results_list <- list()

# reading existing data and only filtering to missing counties 
current_data_path <- get_era5_agg_yearly(year)
# check if path exists
if (!file.exists(current_data_path)) {
  print("file not found")}
existing_data <- read.csv(current_data_path)

processed_counties <- unique(existing_data$GEOID)
all_counties <- usa_counties$GEOID
missing_counties <- setdiff(all_counties, processed_counties)

usa_counties <- usa_counties[usa_counties$GEOID %in% missing_counties, ]

for (i in seq_len(nrow(usa_counties))) {
  # if (i > 1) {
  #  break
  # }

  tic("Processing county: ", i) # Start timing for the current county
  county <- usa_counties[i, ]
  county_id <- county$GEOID

  print(sprintf("county-level shapefile details for ", county_id))
  print(ext(county))
  print(sf::st_crs(county))

  cat(sprintf(
    "=== Processing county %s (%d of %d) ===\n",
    county_id,
    i,
    nrow(usa_counties)
  ))

  cat("calculating overlay weights for county ", county_id, "\n")
  # Calculate overlay weights
  overlay_weights <- tryCatch(
    {
      if (pop_weight) {
        overlay_weights_adj(
          county,
          "GEOID",
          secondary_weights = pop_world_2015_era5
        )
      } else {
        stagg::overlay_weights(county, "GEOID")
      }
    },
    error = function(e) {
      warning(sprintf(
        "Overlay weight failed for county %s: %s",
        county_id,
        e$message
      ))
      return(NULL)
    }
  )

  class(overlay_weights)
  print(overlay_weights)

  if (is.null(overlay_weights) || nrow(overlay_weights) == 0) {
    warning(sprintf("No valid overlay weights for %s, skipping.", county_id))
    next
  }

  cat("dimensions of raster before cropping:\n")
  print(ext(r))
  print(sf::st_crs(r))
  cat("dimensions of county before cropping:\n")
  print(ext(county))
  print(sf::st_crs(county))
  
  cat("cropping raster to county ", county_id, "\n")
  tic("time taken to crop raster")
  # Crop raster to county
  r_crop <- tryCatch(
    {
      cat("inside tryCatch for cropping raster\n")
      # terra::crop(r, terra::vect(county))
      terra::crop(r, terra::vect(county))
    },
    error = function(e) {
      warning(sprintf("Raster crop failed for %s: %s", county_id, e$message))
      return(NULL)
    }
  )

  if (is.null(r_crop)) {
    warning(sprintf("Skipping county %s due to failed raster crop.", county_id))
    next
  }
  toc() # End timing for the raster crop

  print(ext(r_crop))
  print(sf::st_crs(r_crop))
  terra::spatSample(r_crop, size = 5, method = "regular", values = TRUE)

  cat("running staggregate_polynomial for county ", county_id, "\n")
  # Run staggregate_polynomial with daily conditional
  county_result <- tryCatch(
    {
      if (!daily) {
        stagg::staggregate_polynomial(
          data = r_crop,
          overlay_weights = overlay_weights,
          daily_agg = "average",
          time_agg = "month",
          start_date = paste0(year, "-01-01 00:00:00"),
          time_interval = "1 hour",
          degree = 4
        )
      } else {
        stagg::staggregate_polynomial(
          data = r_crop,
          overlay_weights = overlay_weights,
          daily_agg = "none",
          time_agg = "month",
          start_date = paste0(year, "-01-01 00:00:00"),
          time_interval = "24 hour",
          degree = 4
        )
      }
    },
    error = function(e) {
      warning(sprintf(
        "Stagg aggregation failed for %s: %s",
        county_id,
        e$message
      ))
      return(NULL)
    }
  )

  if (!is.null(county_result) && nrow(county_result) > 0) {
    cat("county result is good!\n")
    print(county_result)
    county_result$GEOID <- county_id
    county_result$year <- year
    results_list[[length(results_list) + 1]] <- county_result
  } else {
    cat("county result is not good")
    print(county_result)
  }

  print(county_result)
  print_memory(sprintf("After county %s", county_id))
  flush.console()

  toc() # End timing for the current county
}

# Combine results and save
if (length(results_list) > 0) {
  temp_out <- dplyr::bind_rows(results_list)
  write.csv(temp_out, output_filepath, row.names = FALSE)
  cat(crayon::green(sprintf(
    "Saved per-county aggregated data for year %d\n",
    year
  )))
} else {
  stop("No county results generated.")
}
