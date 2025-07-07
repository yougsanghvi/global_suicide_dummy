# GDNat Climate Data Cleaning and Preparation
# ------------------------------------------
# This script loads, processes, and saves GDNat climate data as NetCDF and GeoTIFF files.
# It is intended for data preparation and cleaning only (no plotting).
#
# Requirements:
#   - xarray, numpy, pandas, dask, rioxarray, glob
#   - cfgrib and netcdf4 engines for xarray (pip install cfgrib netcdf4)
#   - Input Zarr files for GDNat in the specified directories
#
# Output:
#   - Combined and split NetCDF/GeoTIFF files
#
# Author: Youg Sanghvi
# Date: July 7, 2025

import xarray as xr
import os
import numpy as np
from dask.diagnostics import ProgressBar
import rioxarray

# --- Set file paths ---
data_dir_cullen = os.path.join("/global", "scratch", "users", "cmolitor")
data_dir_gdnat_orig = os.path.join(data_dir_cullen, "global_suicide")
gdnat_1 = os.path.join(
    data_dir_gdnat_orig,
    "climate_data",
    "gdnat_ACCESS-CM2_tas_1979-1999_v2025-02-11.zarr",
)
gdnat_2 = os.path.join(
    data_dir_gdnat_orig,
    "climate_data",
    "gdnat_ACCESS-CM2_tas_2000-2020_v2025-02-11.zarr",
)
data_dir_gdnat = "/global/scratch/users/yougsanghvi"
combined_gdnat_file_path = os.path.join(data_dir_gdnat, "gdnat_1979-2020.nc")
tiff_folder_path = os.path.join(data_dir_gdnat, "gdnat_tiff_files_by_yr")
os.makedirs(tiff_folder_path, exist_ok=True)

# --- Load GDNat data from Zarr files ---
gdnat_data_1 = xr.open_zarr(gdnat_1)
gdnat_data_2 = xr.open_zarr(gdnat_2)

# --- Concatenate GDNat datasets and save as NetCDF ---
gdnat_all_gridded = xr.concat([gdnat_data_1, gdnat_data_2], dim="time")
for var in gdnat_all_gridded.data_vars:
    if "compressor" in gdnat_all_gridded[var].encoding:
        del gdnat_all_gridded[var].encoding["compressor"]
with ProgressBar():
    gdnat_all_gridded.to_netcdf(combined_gdnat_file_path, mode="w")

def save_dataset_per_year(
    ds,
    output_dir,
    stagg=False,
    start_year=None,
    end_year=None,
    overwrite=False,
    file_format="netcdf",
):
    """
    Saves data from an xarray Dataset into yearly files.
    Assumes only one variable in the dataset (e.g. 'tas').
    Saves as NetCDF or multi-band GeoTIFF (bands=time) depending on file_format.
    """
    years = np.unique(ds["time.year"].values)
    min_year, max_year = years.min(), years.max()
    if start_year is None:
        start_year = min_year
    if end_year is None:
        end_year = max_year
    if start_year > max_year or end_year < min_year:
        print(
            f"Requested years {start_year}-{end_year} are outside dataset range {min_year}-{max_year}. Nothing to process."
        )
        return
    start_year = max(start_year, min_year)
    end_year = min(end_year, max_year)
    years_to_process = [y for y in years if start_year <= y <= end_year]
    print(f"Processing years: {years_to_process}")
    for year in years_to_process:
        filename = (
            f"gdnat_{year}.nc" if file_format == "netcdf" else f"gdnat_{year}.tif"
        )
        filepath = os.path.join(output_dir, filename)
        if os.path.exists(filepath):
            print(f"File {filepath} already exists")
            if overwrite:
                print("Overwriting existing file")
            else:
                print("Skipping this file")
                continue
        print(f"Processing year {year}...")
        ds_year = ds.sel(time=str(year))
        if stagg:
            print("Removing 'model' variable for STaGG use...")
            ds_year = ds_year.squeeze(dim="model", drop=True)
            ds_year = ds_year.transpose("time", "lat", "lon")
        if file_format == "netcdf":
            encoding = {
                var: {"zlib": False, "chunksizes": None} for var in ds_year.data_vars
            }
            with ProgressBar():
                ds_year.to_netcdf(
                    filepath, encoding=encoding, engine="netcdf4", compute=True
                )
            print(f"Saved NetCDF: {filepath}")
        elif file_format == "tiff":
            var_name = list(ds_year.data_vars)[0]
            da = ds_year[var_name]
            print("CRS before setting:", da.rio.crs)
            if not da.rio.crs:
                da = da.rio.write_crs("EPSG:4326")
            print("CRS after setting:", da.rio.crs)
            da = da.rio.set_spatial_dims(x_dim="lon", y_dim="lat")
            print("Dimensions:", da.dims)
            da = da.rename({"time": "band", "lat": "y", "lon": "x"})
            print("Dims after rename:", da.dims)
            with ProgressBar():
                da.rio.to_raster(filepath)
            print(f"Saved multi-band GeoTIFF: {filepath}")
        else:
            print(f"Unsupported file_format: {file_format}. Skipping year {year}.")

# --- Split GDNat data by year and save as TIFF files ---
save_dataset_per_year(gdnat_data_1, tiff_folder_path, stagg=True, file_format="tiff")
save_dataset_per_year(gdnat_data_2, tiff_folder_path, stagg=True, file_format="tiff")
