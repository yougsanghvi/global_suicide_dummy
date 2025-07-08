# GDNat and ERA5 Climate Data Exploration and Comparison
# -----------------------------------------------------
# This notebook/script loads, processes, and compares GDNat and ERA5 climate datasets.
# It includes functions for splitting datasets by year, computing temperature differences,
# and visualizing results for specific locations and time periods.
#
# Requirements:
#   - xarray, numpy, pandas, matplotlib, cartopy, cftime, dask, rioxarray, glob
#   - cfgrib and netcdf4 engines for xarray (pip install cfgrib netcdf4)
#   - Input Zarr and NetCDF/GeoTIFF files for GDNat and ERA5 in the specified directories
#
# Output:
#   - Combined and split NetCDF/GeoTIFF files, summary statistics, and visualizations
#
# Author: Youg Sanghvi
# Date: July 7, 2025

# ---
# 1. Loading and previewing GDNat Data

# ---

# --- Import necessary libraries ---
import xarray as xr  # For working with multi-dimensional climate data
import os  # For file and directory operations
import matplotlib.pyplot as plt  # For plotting
import cartopy.crs as ccrs  # For map projections
import cartopy.feature as cfeature  # For map features
import numpy as np  # For numerical operations
import pandas as pd  # For tabular data and date handling
import glob  # For file pattern matching
import cftime  # For handling non-standard calendar dates
from datetime import datetime  # For date parsing
from dask.diagnostics import ProgressBar  # For progress bars with dask
import rioxarray  # For raster (GeoTIFF) I/O


# --- Set file paths ---
# Path to Cullen's data directory (original source)
data_dir_cullen = os.path.join("/global", "scratch", "users", "cmolitor")
# Path to GDNat data directory (original source)
data_dir_gdnat_orig = os.path.join(data_dir_cullen, "global_suicide")
# Zarr file paths for GDNat data (1979-1999 and 2000-2020)
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
# Working directories for output and ERA5 data
data_dir_era5 = "/global/scratch/users/yougsanghvi"
data_dir_gdnat = "/global/scratch/users/yougsanghvi"

# File path to save combined GDNat NetCDF
combined_gdnat_file_path = os.path.join(data_dir_gdnat, "gdnat_1979-2020.nc")
# Folder path to save yearly TIFF files
tiff_folder_path = os.path.join(data_dir_gdnat, "gdnat_tiff_files_by_yr")

# Folder to save output figures
outputs_folder = os.path.join(data_dir_gdnat, "outputs")
os.makedirs(outputs_folder, exist_ok=True)


# --- Load GDNat data from Zarr files ---
gdnat_data_1 = xr.open_zarr(gdnat_1)
gdnat_data_2 = xr.open_zarr(gdnat_2)


# --- Concatenate GDNat datasets and save as NetCDF ---
gdnat_all_gridded = xr.concat([gdnat_data_1, gdnat_data_2], dim="time")
# Remove compressor encoding for NetCDF compatibility
for var in gdnat_all_gridded.data_vars:
    if "compressor" in gdnat_all_gridded[var].encoding:
        del gdnat_all_gridded[var].encoding["compressor"]
# Save concatenated dataset to NetCDF with progress bar
with ProgressBar():
    gdnat_all_gridded.to_netcdf(combined_gdnat_file_path, mode="w")

# function to split gdnat data by year and save as a tiff or netcdf file


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

    Parameters:
    - ds: xarray Dataset with 'time' dimension and one variable
    - output_dir: directory to save files
    - stagg: bool, if True removes 'model' dimension and reorders dims for STaGG usage
    - start_year, end_year: int or None, limits on years to process
    - overwrite: bool, overwrite existing files
    - file_format: "netcdf" or "tiff"
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
            ds_year = ds_year.transpose("time", "lat", "lon")  # reorder dims

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
            # Assume one variable, e.g. 'tas'
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


def compute_temperature_difference(location, time_range, era5_data, gdnat_data):
    """
    Computes the difference in surface temperature between ERA5 and GDNat datasets
    for a given location and time range.

    Parameters:
        location (tuple): Latitude and Longitude of the point (lat, lon)
        time_range (tuple): Start and End dates (start_date, end_date) in 'YYYY-MM-DD' format
        era5_data (xarray.Dataset): ERA5 dataset with 't2m' variable
        gdnat_data (xarray.Dataset): GDNat dataset with 'tas' variable

    Returns:
        xarray.DataArray: Difference in temperatures (ERA5 - GDNat) in Celsius
    """
    lat, lon = location
    start_date, end_date = time_range

    # Select nearest grid point in both datasets and time slice
    era5_point = (
        era5_data["t2m"]
        .sel(latitude=lat, longitude=lon, method="nearest")
        .sel(time=slice(start_date, end_date))
        - 273.15
    )

    gdnat_point = (
        gdnat_data["tas"]
        .sel(lat=lat, lon=lon, method="nearest")
        .sel(time=slice(start_date, end_date))
        - 273.15
    )

    # Convert both time axes to datetime64[ns] for consistency
    era5_time = np.array(era5_point["time"].values, dtype="datetime64[ns]")
    gdnat_time = np.array(gdnat_point["time"].values, dtype="datetime64[ns]")

    era5_point = era5_point.assign_coords(time=era5_time)
    gdnat_point = gdnat_point.assign_coords(time=gdnat_time)

    # Align both datasets along time (robust to mismatches)
    era5_aligned, gdnat_aligned = xr.align(era5_point, gdnat_point, join="inner")

    # Compute the difference
    difference = era5_aligned - gdnat_aligned

    return difference

# 3. Plotting


# Clean data to plot a map with an estimate for anthropogenic global warming

# Set your time range for comparison
start_date = "2000-01-01"
end_date = "2011-12-31"

# Load ERA5 daily data (merge across years if needed)

era5_files = [
    f"{data_dir_era5}/era5_daily_mean_{year}.nc" for year in range(2000, 2012)
]

print("loading ERA5 files")
era5_ds = xr.open_mfdataset(
    era5_files, combine="by_coords", decode_timedelta=True, chunks={"time": 365}
)

print("finished ERA5 loading files")

# Select time range
print("slicing both files")
era5_sel = era5_ds.sel(time=slice(start_date, end_date))
gdnat_sel = gdnat_data_2.sel(time=slice(start_date, end_date))
print("finished slicing files")

# Ensure temperature units are comparable (ERA5 is in Kelvin, GDNat usually too)
print("converting to celsius")
era5_temp = era5_sel["t2m"] - 273.15  # Convert to °C
gdnat_temp = gdnat_sel["tas"].squeeze() - 273.15  # Remove 'model' dim
print("finished converting to celsius")

# Time-average both
print("computing means")
era5_mean = era5_temp.mean(dim="time").compute()
gdnat_mean = gdnat_temp.mean(dim="time").compute()
print("finished computing means")

# ensure alignment for merging
gdnat_mean = gdnat_mean.rename({"lat": "latitude", "lon": "longitude"})

print("ERA5 shape:", era5_mean.shape)
print("GDNat shape:", gdnat_mean.shape)
print("ERA5 dims:", era5_mean.dims)
print("GDNat dims:", gdnat_mean.dims)

# Delta (Panel F)
delta = era5_mean - gdnat_mean


# Plotting a map with an estimate for anthropogenic global warming

fig, axs = plt.subplots(
    1,
    3,
    figsize=(18, 5),
    subplot_kw={"projection": ccrs.PlateCarree()},
    constrained_layout=True,
)

# Panel D: ERA5
im1 = axs[0].pcolormesh(
    era5_mean["longitude"],
    era5_mean["latitude"],
    era5_mean,
    cmap="coolwarm",
    vmin=-40,
    vmax=40,
    transform=ccrs.PlateCarree(),
)
axs[0].set_title("ERA5 (D)")
axs[0].coastlines()
axs[0].add_feature(cfeature.BORDERS, linewidth=0.3)
plt.colorbar(im1, ax=axs[0], orientation="horizontal", label="Temperature (°C)")

# Panel E: GDNat
im2 = axs[1].pcolormesh(
    gdnat_mean["longitude"],
    gdnat_mean["latitude"],
    gdnat_mean,
    cmap="coolwarm",
    vmin=-40,
    vmax=40,
    transform=ccrs.PlateCarree(),
)
axs[1].set_title("GDNat (E)")
axs[1].coastlines()
axs[1].add_feature(cfeature.BORDERS, linewidth=0.3)
plt.colorbar(im2, ax=axs[1], orientation="horizontal", label="Temperature (°C)")

# Panel F: Delta
im3 = axs[2].pcolormesh(
    delta["longitude"],
    delta["latitude"],
    delta,
    cmap="RdBu_r",
    vmin=-4,
    vmax=4,
    transform=ccrs.PlateCarree(),
)
axs[2].set_title("GDNat Delta (F)")
axs[2].coastlines()
axs[2].add_feature(cfeature.BORDERS, linewidth=0.3)
plt.colorbar(
    im3, ax=axs[2], orientation="horizontal", label="Temperature Difference (°C)"
)

plt.suptitle("ACCESS CM-2: Temperature Comparison (2000–2011)", fontsize=16)

# Save the figure to outputs folder
fig_path = os.path.join(
    outputs_folder, "ACCESS_CM2_Temperature_Comparison_2000_2011.png"
)
plt.savefig(fig_path, dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"Saved figure: {fig_path}")


# check if file exists before analyzing heatwave data

# Define the full path to the file you want to check
file_path = os.path.join(data_dir_era5, "era5_daily_mean_2019.nc")

# Check if it exists
if os.path.exists(file_path):
    print("File exists ✅")
else:
    print("File not found ❌")


# 3B. Plotting Time Series


def plot_heatwave_comparison(location, heatwave_period, plot_range, title, heatwave):
    """
    Plot observed (ERA5) and counterfactual (GDNat) temperatures with a secondary y-axis for the anthropogenic component.

    Parameters:
        location (tuple): (latitude, longitude)
        heatwave_period (tuple): (start_date, end_date) of the heatwave for highlighting
        plot_range (tuple): (start_date, end_date) for the x-axis time range
        title (str): Title of the plot
        heatwave (bool): Whether to highlight the heatwave period
    """

    print("Plotting your GDNat and ERA5 plot...")

    lat, lon = location
    start_date, end_date = plot_range

    # Determine which GDNat file to use based on the year
    year = pd.to_datetime(start_date).year
    if year <= 1999:
        gdnat_data = gdnat_data_1
    else:
        gdnat_data = gdnat_data_2

    # Load ERA5 data for the relevant year
    era5_file = os.path.join(data_dir_era5, f"era5_daily_mean_{year}.nc")
    era5_data = xr.open_dataset(era5_file)

    # Select nearest grid points and slice by time
    era5_point = (
        era5_data["t2m"]
        .sel(latitude=lat, longitude=lon, method="nearest")
        .sel(time=slice(start_date, end_date))
        - 273.15
    )
    gdnat_point = (
        gdnat_data["tas"]
        .sel(lat=lat, lon=lon, method="nearest")
        .sel(time=slice(start_date, end_date))
        .squeeze()
        - 273.15
    )

    anthropogenic_component = compute_temperature_difference(
        location, (start_date, end_date), era5_data, gdnat_data
    )

    # test the anthropogenic means:

    """
    print("ERA5 sample:", era5_point.values[:5])
    print("GDNat sample:", gdnat_point.values[:5])
    print("Anthropogenic Component sample (ERA5 - GDNat):", anthropogenic_component.values[:5])

    print("Anthropogenic Component Mean (°C):", anthropogenic_component.mean().compute().item())
    print("Anthropogenic Component Std Dev (°C):", anthropogenic_component.std().compute().item())
    print("Anthropogenic Component min/max (°C):", anthropogenic_component.min().compute().item(), anthropogenic_component.max().compute().item())
    """

    # Create figure and axes
    fig, ax1 = plt.subplots(figsize=(12, 6))

    # Plot ERA5 and GDNat on primary y-axis
    ax1.plot(
        era5_point["time"],
        era5_point,
        label=f"ERA5 Actual Temperature ({year})",
        color="red",
    )
    ax1.plot(
        gdnat_point["time"],
        gdnat_point,
        label=f"GDNat Counterfactual ({year})",
        color="blue",
    )

    ax1.set_xlabel("Date")
    ax1.set_ylabel("Temperature (°C)", color="black")
    ax1.tick_params(axis="y", labelcolor="black")

    # Highlight heatwave period if requested
    if heatwave:
        heat_start, heat_end = heatwave_period
        ax1.axvspan(
            np.datetime64(heat_start),
            np.datetime64(heat_end),
            color="gray",
            alpha=0.3,
            label="Heatwave Period",
        )

    # Secondary y-axis for anthropogenic component
    ax2 = ax1.twinx()
    ax2.plot(
        anthropogenic_component["time"],
        anthropogenic_component,
        label="Anthropogenic Component (ERA5 - GDNat)",
        color="black",
        linestyle="--",
    )
    ax2.set_ylabel("Anthropogenic Component (°C)", color="black")
    ax2.tick_params(axis="y", labelcolor="black")

    """
    # Get average temperature difference DataFrame for the heatwave period
    df_avg_diff = average_temperature_over_range(location, plot_range[0], plot_range[1])

    # Calculate mean difference over the period
    avg_diff = (df_avg_diff["avg_temperature_era5"] - df_avg_diff["avg_temperature_gdnat"]).mean()

    # Plot horizontal line on secondary y-axis for average difference
    ax2.axhline(
        avg_diff,
        color="red",
        linestyle=":",
        linewidth=2,
        label=f"Avg Diff ERA5-GDNat ({plot_range[0]} to {plot_range[1]})"
    )

    # Update legend for secondary axis to include this new line
    lines, labels = ax2.get_legend_handles_labels()
    ax2.legend(lines, labels, loc="upper right")
    """

    # Combine legends from both axes
    lines_1, labels_1 = ax1.get_legend_handles_labels()
    lines_2, labels_2 = ax2.get_legend_handles_labels()
    ax1.legend(lines_1 + lines_2, labels_1 + labels_2, loc="upper left")

    ax1.set_title(title)
    ax1.grid(True, which="both", linestyle="--", color="gray", alpha=0.7)
    ax1.tick_params(axis="x", labelsize=8)
    ax1.tick_params(axis="y", labelsize=8)
    ax2.tick_params(axis="y", labelsize=8)
    ax2.set_ylim(anthropogenic_component.min().compute().item(), 20)

    plt.tight_layout()
    plt.show()


plot_heatwave_comparison(
    location=(28.61, 77.21),
    heatwave_period=("2019-05-15", "2019-06-15"),
    heatwave=True,
    plot_range=("2019-04-01", "2019-07-31"),
    title="New Delhi Heatwave (Mid-May to Mid-June, 2019)",
)


plot_heatwave_comparison(
    location=(28.61, 77.21),
    heatwave_period=("2019-05-15", "2019-06-15"),
    plot_range=("2019-01-01", "2019-12-31"),
    title="New Delhi Heatwave (Mid-May to Mid-June, 2019)",
    heatwave=True,
)


# comparision to colder part of India -- Kashmir is chosen here
plot_heatwave_comparison(
    location=(34.08, 74.79),  # Srinagar, India
    heatwave_period=("2019-05-15", "2019-08-15"),
    plot_range=("2019-04-01", "2019-07-31"),
    title="Srinagar, India in 2019 (Non-Heatwave Affected)",
    heatwave=False,
)


# full year version
plot_heatwave_comparison(
    location=(34.08, 74.79),  # Srinagar, India
    heatwave_period=("2019-01-01", "2019-12-01"),
    plot_range=("2019-01-01", "2019-12-01"),
    title="Srinagar, India in 2019 (Non-Heatwave Affected)",
    heatwave=False,
)


plot_heatwave_comparison(
    location=(41.88, 272.37),  # Chicago lat, ERA5 longitude
    heatwave_period=("1995-07-12", "1995-07-16"),  # Heatwave dates
    plot_range=("1995-07-01", "1995-07-31"),  # Wider plot range for context
    title="Chicago Heatwave (July 1995)",
    heatwave=True,
)


plot_heatwave_comparison(
    location=(41.88, 272.37),  # Chicago lat, ERA5 longitude
    heatwave_period=("1995-07-12", "1995-07-16"),  # Heatwave dates
    plot_range=("1995-01-01", "1995-12-31"),
    title="Chicago Heatwave (July 1995)",
    heatwave=True,
)


plot_heatwave_comparison(
    location=(41.88, 272.37),  # Chicago lat, ERA5 longitude
    heatwave_period=("2020-06-20", "2020-09-30"),  # Heatwave dates
    plot_range=("2020-04-20", "2020-10-30"),
    title="Chicago Summer (2020)",
    heatwave=True,
)


plot_heatwave_comparison(
    location=(41.88, 272.37),  # Chicago lat, ERA5 longitude
    heatwave_period=("2020-06-20", "2020-09-30"),  # Heatwave dates
    plot_range=("2020-01-01", "2020-12-31"),
    title="Chicago Summer (2020)",
    heatwave=True,
)


plot_heatwave_comparison(
    location=(41.88, 272.37),  # Chicago lat, ERA5 longitude
    heatwave_period=("2015-02-01", "2015-02-28"),  # Heatwave dates
    plot_range=("2015-01-01", "2015-12-30"),
    title="Chicago Coldest February since 1875 (2015)",
    heatwave=True,
)


plot_heatwave_comparison(
    location=(41.88, 272.37),  # Chicago lat, ERA5 longitude
    heatwave_period=("2015-02-01", "2015-02-28"),  # Heatwave dates
    plot_range=("2015-01-01", "2015-12-30"),
    title="Chicago Coldest February since 1875 (2015)",
    heatwave=True,
)
