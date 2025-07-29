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
import sys

# Importing file paths from utils 
sys.path.append("/global/home/users/yougsanghvi/global_suicide_dummy/code")
from y_utils import config

print("imported all required libraries")

# --- Load GDNat data from Zarr files ---
print("loading dataset")
gdnat_data = xr.open_zarr(config.GDNAT_DATA_FP)

# --- Split GDNat datasets and save as tiff ---
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
    Saves data from an xarray Dataset into yearly files, splitting by model if present.
    Assumes only one variable in the dataset (e.g. 'tas').
    Saves as NetCDF or multi-band GeoTIFF (bands=time) depending on file_format.

    If 'model' dimension exists, creates subfolders per model inside output_dir.
    """
    # Determine years available
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

    # Check if 'model' dimension exists
    has_model = "model" in ds.dims or "model" in ds.coords

    if has_model:
        models = ds["model"].values
        print(f"Found models: {models}")
        for model in models:
            model_folder = os.path.join(output_dir, str(model))
            os.makedirs(model_folder, exist_ok=True)
            print(f"Processing model: {model} in folder: {model_folder}")
            ds_model = ds.sel(model=model)
            
            for year in years_to_process:
                filename = (
                    f"gdnat_{year}.nc" if file_format == "netcdf" else f"gdnat_{year}.tif"
                )
                filepath = os.path.join(model_folder, filename)
                if os.path.exists(filepath):
                    print(f"File {filepath} already exists")
                    if overwrite:
                        print("Overwriting existing file")
                    else:
                        print("Skipping this file")
                        continue
                print(f"Processing year {year} for model {model}...")
                ds_year = ds_model.sel(time=str(year))
                if stagg:
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
                    if not da.rio.crs:
                        da = da.rio.write_crs("EPSG:4326")
                    da = da.rio.set_spatial_dims(x_dim="lon", y_dim="lat")
                    da = da.rename({"time": "band", "lat": "y", "lon": "x"})
                    with ProgressBar():
                        da.rio.to_raster(filepath)
                    print(f"Saved multi-band GeoTIFF: {filepath}")
                else:
                    print(f"Unsupported file_format: {file_format}. Skipping year {year}.")
    else:
        # No model dimension, fallback to original behavior
        print("No 'model' dimension found, processing without model split.")
        os.makedirs(output_dir, exist_ok=True)
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
            if stagg and "model" in ds_year.dims:
                ds_year = ds_year.squeeze(dim="model", drop=True)
            if stagg:
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
                if not da.rio.crs:
                    da = da.rio.write_crs("EPSG:4326")
                da = da.rio.set_spatial_dims(x_dim="lon", y_dim="lat")
                da = da.rename({"time": "band", "lat": "y", "lon": "x"})
                with ProgressBar():
                    da.rio.to_raster(filepath)
                print(f"Saved multi-band GeoTIFF: {filepath}")
            else:
                print(f"Unsupported file_format: {file_format}. Skipping year {year}.")

# --- Split GDNat data by year and save as TIFF files ---
save_dataset_per_year(gdnat_data, config.GDNAT_TIFF_OUTPUT_DIR, stagg=True, file_format="tiff")
