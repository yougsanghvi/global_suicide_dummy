import os
import sys
import xarray as xr
import rioxarray
import numpy as np

# -----------------------
# Command-line input
# -----------------------
year = int(sys.argv[1])

# -----------------------
# Paths
# -----------------------
era5_daily_dir = "/global/scratch/users/yougsanghvi/era5_daily_by_year/"
gdnat_dir = "/global/scratch/users/yougsanghvi/gdnat_tiff_files_by_yr/"
output_dir = "/global/scratch/users/yougsanghvi/panel_diff_by_year/"
os.makedirs(output_dir, exist_ok=True)

# -----------------------
# Load ERA5 daily dataset
# -----------------------
era5_path = os.path.join(era5_daily_dir, f"era5_daily_mean_{year}.nc")
era5_ds = xr.open_dataset(era5_path)
era5_var = 't2m'  # Change if your ERA5 temperature variable is named differently

# -----------------------
# Load GDNat yearly raster
# -----------------------
gdnat_path = os.path.join(gdnat_dir, f"gdnat_{year}.tif")
gdnat_ds = rioxarray.open_rasterio(gdnat_path).squeeze()  # remove band dim if present

# -----------------------
# Fix GDNat longitude from [0, 360) → [-180, 180)
# -----------------------
def shift_longitude_intuitive(ds, lon_name='x'):
    lon = ds[lon_name].values  # get longitudes as numpy array
    lon_corrected = lon.copy()
    lon_corrected[lon_corrected > 180] -= 360  # shift longitudes >180 to west
    ds = ds.assign_coords({lon_name: lon_corrected})
    ds = ds.sortby(lon_name)
    return ds

gdnat_ds = shift_longitude_intuitive(gdnat_ds, lon_name='x')

# Set CRS if missing
if not gdnat_ds.rio.crs:
    gdnat_ds = gdnat_ds.rio.write_crs("EPSG:4326")

# -----------------------
# Expand GDNat to daily to match ERA5
# -----------------------
times = era5_ds.time.values  # daily timestamps
gdnat_daily = gdnat_ds.expand_dims(time=times)  # same raster, repeated across days

# -----------------------
# Compute difference
# -----------------------
era5_temp = era5_ds[era5_var]

gdnat_daily = gdnat_daily.rename({'y': 'latitude', 'x': 'longitude'})

diff_temp = era5_temp - gdnat_daily

# -----------------------
# Save all to NetCDF
# -----------------------
out_ds = xr.Dataset({
    'era5_temp': era5_temp,
    'gdnat_temp': gdnat_daily,
    'diff_temp': diff_temp
})

out_path = os.path.join(output_dir, f"daily_panel_diff_{year}.nc")
out_ds.to_netcdf(out_path)

print(f"✅ Saved daily panel difference dataset for year {year} to:\n{out_path}")
