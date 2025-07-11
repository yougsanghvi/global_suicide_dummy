import xarray as xr
import pandas as pd
import numpy as np
import os
import sys
from cftime import DatetimeGregorian

year = int(sys.argv[1])  # e.g. 2000 from SLURM

# Output path
out_dir = "/global/scratch/users/yougsanghvi/era5_gdnat_panel_diff/"
os.makedirs(out_dir, exist_ok=True)
out_path = os.path.join(out_dir, f"era5_gdnat_diff_{year}.nc")

# Skip if output file exists
if os.path.exists(out_path):
    print(f"Output file already exists for year {year}, skipping processing.")
    sys.exit(0)

print("script started for year:", year)

# Input Paths
era5_path = f"/global/scratch/users/yougsanghvi/era5_daily_by_year/era5_daily_mean_{year}.nc"
gdnat_1 = "/global/scratch/users/yougsanghvi/global_suicide/gdnat_ACCESS-CM2_tas_1979-1999_v2025-02-11.zarr"
gdnat_2 = "/global/scratch/users/yougsanghvi/global_suicide/gdnat_ACCESS-CM2_tas_2000-2020_v2025-02-11.zarr"
gdnat_path = gdnat_1 if year <= 1999 else gdnat_2

# Load GDNat Zarr
gdnat_ds = xr.open_zarr(gdnat_path)

# Select the year slice by new time coordinate
gdnat_year = gdnat_ds.sel(time=slice(f"{year}-01-01", f"{year}-12-31"))
gdnat_year = gdnat_year.squeeze(dim='model', drop=True)

# Convert GDNat time values to DatetimeGregorian so they match ERA5
gdnat_year['time'] = [DatetimeGregorian(t.year, t.month, t.day) for t in gdnat_year['time'].values]

# Load ERA5 dataset and temperature variable
era5_ds = xr.open_dataset(era5_path, use_cftime=True)

era5_temp = era5_ds['t2m']

# Drop leap day to match GDNat's 365-day calendar
era5_temp = era5_temp.sel(time=~((era5_temp.time.dt.month == 2) & (era5_temp.time.dt.day == 29)))

# Rename dims in GDNat to match ERA5 if needed:
if 'lat' in gdnat_year.dims:
    gdnat_year = gdnat_year.rename({'lat': 'latitude', 'lon': 'longitude'})

# Align the two datasets on time, latitude, longitude (inner join)
era5_temp_aligned, gdnat_aligned = xr.align(era5_temp, gdnat_year['tas'], join='inner')

# Print alignment info
print(f"ERA5 shape before align: {era5_temp.shape} dims: {era5_temp.dims}")
print(f"GDNat shape before align: {gdnat_year['tas'].shape} dims: {gdnat_year['tas'].dims}")
print(f"ERA5 shape after align: {era5_temp_aligned.shape}")
print(f"GDNat shape after align: {gdnat_aligned.shape}")

# Calculate difference
diff_temp = era5_temp_aligned - gdnat_aligned

# Save output
diff_temp.to_netcdf(os.path.join(out_path))

print(f"Saved diff for {year}")
