import xarray as xr
import numpy as np
import sys
import os

# --- CONFIG --- #
# Provide year as command line argument or hardcode it here
if len(sys.argv) < 2:
    year = 2000
else:
    year = int(sys.argv[1])

# Path to your saved diff file
diff_path = f"/global/scratch/users/yougsanghvi/era5_gdnat_panel_diff/era5_gdnat_diff_{year}.nc"

# --- CHECK EXISTENCE --- #
if not os.path.exists(diff_path):
    print(f"\n❌ File not found: {diff_path}")
    sys.exit(1)

print(f"\n📂 Loading difference file for {year}: {diff_path}")
ds = xr.open_dataset(diff_path)

# --- BASIC METADATA --- #
print("\n🧾 Dataset info:")
print(ds)

# --- VARIABLE SUMMARY --- #
# Assuming the diff variable inherits the name 't2m'
var_name = list(ds.data_vars)[0]
diff = ds[var_name]

print(f"\n📊 Summary statistics for '{var_name}':")
print(f"  Shape: {diff.shape}")
print(f"  Dimensions: {diff.dims}")
print(f"  Min: {float(diff.min().values):.3f}")
print(f"  Max: {float(diff.max().values):.3f}")
print(f"  Mean: {float(diff.mean().values):.3f}")
print(f"  Std Dev: {float(diff.std().values):.3f}")
print(f"  Time Range: {diff['time'].values[0]} to {diff['time'].values[-1]}")
