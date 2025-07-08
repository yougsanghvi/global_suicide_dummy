# ERA5 Hourly to Daily Mean Conversion Script
# --------------------------------------------
# This script summarizes ERA5 hourly temperature data into daily means for a range of years.
#
# Requirements:
#   - xarray
#   - cfgrib engine for xarray (pip install cfgrib)
#   - Input .grib files for each year in the specified directory
#
# Output:
#   - For each year, a NetCDF (.nc) file with daily mean temperature is saved to the same directory.
#
# Author: Youg Sanghvi
# Date: July 7, 2025

import xarray as xr  # For working with multi-dimensional arrays and NetCDF/grib files
import os            # For file and directory operations

# Set base directory for input/output files
# !! Change this path if your data is stored elsewhere
cd = "/global/scratch/users/yougsanghvi"

# Define the range of years to process
# !! Adjust the range as needed
years = range(2015, 2021)

# Loop over each year and process the data
for year in years:
    input_path = f"{cd}/era5_data_{year}.grib"           # Input grib file for the year
    output_path = f"{cd}/era5_daily_mean_{year}.nc"      # Output NetCDF file for daily means

    # Skip processing if output already exists
    if os.path.exists(output_path):
        print(f"Skipping {year} (already summarized)")
        continue

    print(f"Processing year {year}...")

    # Open the hourly data file using xarray and cfgrib engine
    ds = xr.open_dataset(
        input_path,
        engine="cfgrib",
        backend_kwargs={"indexpath": ""},
        chunks={"time": 24}  # Chunk by day for efficiency
    )

    # Resample to daily means
    daily_ds = ds.resample(time="1D").mean()
    # Save the daily mean data to NetCDF
    daily_ds.to_netcdf(output_path)

    print(f"Saved daily mean to {output_path}")


