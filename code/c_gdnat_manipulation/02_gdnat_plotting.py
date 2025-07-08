# GDNat and ERA5 Climate Data Plotting and Analysis
# ------------------------------------------------
# This script loads GDNat and ERA5 climate datasets and provides functions for plotting
# temperature maps, time series, and anthropogenic warming estimates.
# It is intended for analysis and visualization only (no data cleaning or file creation).
#
# Requirements:
#   - xarray, numpy, pandas, matplotlib, cartopy, cftime, dask
#   - Input NetCDF/GeoTIFF files for GDNat and ERA5 in the specified directories
#
# Output:
#   - Figures saved to the outputs folder
#
# Author: Youg Sanghvi
# Date: July 7, 2025

import xarray as xr
import os
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
import pandas as pd
from datetime import datetime
from dask.diagnostics import ProgressBar

# --- Set file paths ---
data_dir_era5 = "/global/scratch/users/yougsanghvi"
data_dir_gdnat = "/global/scratch/users/yougsanghvi"
outputs_folder = os.path.join(data_dir_gdnat, "outputs")
os.makedirs(outputs_folder, exist_ok=True)

def compute_temperature_difference(location, time_range, era5_data, gdnat_data):
    """
    Computes the difference in surface temperature between ERA5 and GDNat datasets
    for a given location and time range.
    """
    lat, lon = location
    start_date, end_date = time_range
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
    era5_time = np.array(era5_point["time"].values, dtype="datetime64[ns]")
    gdnat_time = np.array(gdnat_point["time"].values, dtype="datetime64[ns]")
    era5_point = era5_point.assign_coords(time=era5_time)
    gdnat_point = gdnat_point.assign_coords(time=gdnat_time)
    era5_aligned, gdnat_aligned = xr.align(era5_point, gdnat_point, join="inner")
    difference = era5_aligned - gdnat_aligned
    return difference

# --- Example: Plotting a map with anthropogenic global warming estimate ---
def plot_temperature_comparison():
    start_date = "2000-01-01"
    end_date = "2011-12-31"
    era5_files = [
        f"{data_dir_era5}/era5_daily_mean_{year}.nc" for year in range(2000, 2012)
    ]
    print("loading ERA5 files")
    era5_ds = xr.open_mfdataset(
        era5_files, combine="by_coords", decode_timedelta=True, chunks={"time": 365}
    )
    print("finished ERA5 loading files")
    print("slicing both files")
    era5_sel = era5_ds.sel(time=slice(start_date, end_date))
    gdnat_file = os.path.join(data_dir_gdnat, "gdnat_1979-2020.nc")
    gdnat_ds = xr.open_dataset(gdnat_file)
    gdnat_sel = gdnat_ds.sel(time=slice(start_date, end_date))
    print("finished slicing files")
    print("converting to celsius")
    era5_temp = era5_sel["t2m"] - 273.15
    gdnat_temp = gdnat_sel["tas"].squeeze() - 273.15
    print("finished converting to celsius")
    print("computing means")
    era5_mean = era5_temp.mean(dim="time").compute()
    gdnat_mean = gdnat_temp.mean(dim="time").compute()
    print("finished computing means")
    gdnat_mean = gdnat_mean.rename({"lat": "latitude", "lon": "longitude"})
    delta = era5_mean - gdnat_mean
    fig, axs = plt.subplots(
        1,
        3,
        figsize=(18, 5),
        subplot_kw={"projection": ccrs.PlateCarree()},
        constrained_layout=True,
    )
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
    fig_path = os.path.join(
        outputs_folder, "ACCESS_CM2_Temperature_Comparison_2000_2011.png"
    )
    plt.savefig(fig_path, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved figure: {fig_path}")

# --- Plotting Time Series for Heatwaves and Locations ---
def plot_heatwave_comparison(location, heatwave_period, plot_range, title, heatwave):
    print("Plotting your GDNat and ERA5 plot...")
    lat, lon = location
    start_date, end_date = plot_range
    year = pd.to_datetime(start_date).year
    gdnat_file = os.path.join(data_dir_gdnat, "gdnat_1979-2020.nc")
    gdnat_ds = xr.open_dataset(gdnat_file)
    if year <= 1999:
        gdnat_data = gdnat_ds.sel(time=slice(f"{year}-01-01", f"{year}-12-31"))
    else:
        gdnat_data = gdnat_ds.sel(time=slice(f"{year}-01-01", f"{year}-12-31"))
    era5_file = os.path.join(data_dir_era5, f"era5_daily_mean_{year}.nc")
    era5_data = xr.open_dataset(era5_file)
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
    fig, ax1 = plt.subplots(figsize=(12, 6))
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
    if heatwave:
        heat_start, heat_end = heatwave_period
        ax1.axvspan(
            np.datetime64(heat_start),
            np.datetime64(heat_end),
            color="gray",
            alpha=0.3,
            label="Heatwave Period",
        )
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
    fig_path = os.path.join(
        outputs_folder, f"{title.replace(' ', '_').replace('(', '').replace(')', '')}.png"
    )
    plt.savefig(fig_path, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved figure: {fig_path}")

# Example usage (uncomment to run):
# plot_temperature_comparison()
# plot_heatwave_comparison(
#     location=(28.61, 77.21),
#     heatwave_period=("2019-05-15", "2019-06-15"),
#     plot_range=("2019-04-01", "2019-07-31"),
#     title="New Delhi Heatwave (Mid-May to Mid-June, 2019)",
#     heatwave=True,
# )
