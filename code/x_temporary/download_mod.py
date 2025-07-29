"""
ERA5 Hourly 2m Temperature Data Download Script with Parallelism and Per-Year Logging

Description:
-------------
Downloads ERA5 hourly 2-meter temperature data from the Copernicus Climate Data Store (CDS)
for each year in a specified range, saving one GRIB file per year.

Key Features:
- Parallel downloads using Python's ThreadPoolExecutor for faster retrieval.
- Skips years already downloaded to avoid redundant requests.
- Writes detailed per-year logs capturing download progress and errors.
- Minimal console output for high-level progress monitoring.

Usage:
------
- Requires 'cdsapi' Python package and configured CDS API key.
- Configure data directory and year range as needed.
- Run interactively or in HPC batch jobs.
- Check logs/ folder for detailed per-year download information.

Author: Youg Sanghvi
Date: July 9, 2025
"""


import cdsapi
import os
from concurrent.futures import ThreadPoolExecutor
import logging
from contextlib import redirect_stdout, redirect_stderr

print("Running ERA5 download script with logging...")

# Configuration
dataset = "reanalysis-era5-single-levels"
data_dir = "/global/scratch/users/yougsanghvi/"
log_path = "/global/home/users/yougsanghvi/global_suicide_dummy/logs/"
data_folder = os.path.join(data_dir, "era5_hourly_by_year")

overwrite = True


def download_era5_year(year):
    fn = f"era5_data_{year}.grib"
    fp = os.path.join(data_folder, fn)
    log_fp = os.path.join(log_path, f"era5_download_{year}.log")

    if os.path.exists(fp) and overwrite == False:
        print(f"[SKIP] {fn} already exists.")
        return

    # Redirect stdout and stderr to the log file while downloading
    try:
        client = cdsapi.Client()
        request = {
            "product_type": "reanalysis",
            "variable": "2m_temperature",
            "year": str(year),
            "month": [f"{m:02d}" for m in range(1, 13)],
            "day": [f"{d:02d}" for d in range(1, 32)],
            "time": [f"{h:02d}:00" for h in range(24)],
            "data_format": "grib",
        }

        with open(log_fp, "a") as log_file:
            with redirect_stdout(log_file), redirect_stderr(log_file):
                client.retrieve(dataset, request, fp)

        logger.info(f"Completed download for year {year}")

    except Exception as e:
        logger.error(f"Failed to download year {year}: {e}")
        print(f"[ERROR] Failed to download {fn}: {e}")


# Run downloads in parallel
download_era5_year(1992)

print("All download tasks complete. Check logs/ folder for detailed output.")
