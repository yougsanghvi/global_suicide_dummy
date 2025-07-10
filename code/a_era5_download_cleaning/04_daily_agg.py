import xarray as xr
import os
import sys

# Arguments and paths
year = int(sys.argv[1])

cd = "/global/scratch/users/yougsanghvi/era5_hourly_by_year/"
output_dir = "/global/scratch/users/yougsanghvi/era5_daily_by_year/"
os.makedirs(output_dir, exist_ok=True)

input_path = f"{cd}/era5_data_{year}.grib"
output_path = f"{output_dir}/era5_daily_mean_{year}.nc"

if os.path.exists(output_path):
    print(f"Skipping {year} (already summarized)")
    sys.exit(0)

print(f"Processing year {year}...")

ds = xr.open_dataset(
    input_path,
    engine="cfgrib",
    backend_kwargs={"indexpath": ""},
    chunks={"time": 24},
)

daily_ds = ds.resample(time="1D").mean()
daily_ds.to_netcdf(output_path)

print(f"Saved daily mean to {output_path}")
