library("terra")
#library('RNetCDF')
#library('ncmeta')


dir <- "/global/scratch/users/yougsanghvi"
gdnat_tiff_filename <- "gdnat_1979.tif"
tiff_path <- file.path(dir, gdnat_tiff_filename)

r <- terra::rast(tiff_path)
