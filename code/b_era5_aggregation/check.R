# ------ 2. CONFIGURATION ------

print_memory("Start of script")

# Set data source manually here
DATA_SOURCE <- "era5"  # or "gdnat"
is_era5 <- DATA_SOURCE == "era5"

# Base directory
dir <- "/global/scratch/users/yougsanghvi"

# Subdirectories and file name patterns per dataset
if (is_era5) {
  dir_tiff <- file.path(dir, "era5_hourly_by_year")
  filename_pattern <- "era5_data_%d.grib"
  file_format_prefix <- "era5_usa_agg_"
  output_dir <- file.path(dir, "aggregated_results_era5_usa")
  daily <- FALSE
} else {
  dir_tiff <- file.path(dir, "gdnat_hourly_by_year")
  filename_pattern <- "gdnat_data_%d.nc"
  file_format_prefix <- "gdnat_usa_agg_"
  output_dir <- file.path(dir, "aggregated_results_gdnat_usa")
  daily <- TRUE
}

# Shapefile path
usa_county_dir <- file.path(dir, "shapefiles")
usa_county_filename <- "tl_2016_us_county_mortality.shp"
usa_county_path <- file.path(usa_county_dir, usa_county_filename)

# Output setup
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

output_filepath_all_years <- file.path(output_dir, paste0(file_format_prefix, "all_years.csv"))
