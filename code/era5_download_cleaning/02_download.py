
# ERA5 Hourly Data Download Script
# ---------------------------------
# This script downloads hourly 2m temperature data from the ERA5 reanalysis dataset
# for each year in the specified range (for eg. 1979-2020) using the CDS API.
#
# Requirements:
#   - Install the CDS API: pip install cdsapi
#   - Set up your CDS API key in ~/.cdsapirc (see https://cds.climate.copernicus.eu/api-how-to)
#   - Ensure the output directory exists and you have write permissions.
#
# Output:
#   - For each year, a .grib file containing hourly 2m temperature data is saved to the specified folder.
#
# Author: Youg Sanghvi
# Date: July 7, 2025

print("running download script...")


import cdsapi  # Climate Data Store API client
import os      # For file path operations


# Dataset and output configuration
dataset = "reanalysis-era5-single-levels"  # ERA5 single-level reanalysis dataset
data_dir = "/global/scratch/users/yougsanghvi/"  # Base directory for data storage
data_folder = os.path.join(data_dir, "era5_hourly_by_year")  # Output folder for yearly files

# Year range for data download
start_year = 1979
end_year = 2020


# Loop over each year and download the data
for year in range(start_year, end_year + 1):
    print(f"downloading data for year: {year}")

    # Construct the request dictionary for the CDS API
    request = {
        "product_type": ["reanalysis"],            # Type of product
        "variable": ["2m_temperature"],            # Variable to download
        "year": [str(year)],                        # Year as string
        "month": [                                  # All months
            "01", "02", "03", "04", "05", "06",
            "07", "08", "09", "10", "11", "12",
        ],
        "day": [                                    # All days in month
            "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
            "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
            "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
        ],
        "time": [                                   # All hours in a day
            "00:00", "01:00", "02:00", "03:00", "04:00", "05:00",
            "06:00", "07:00", "08:00", "09:00", "10:00", "11:00",
            "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
            "18:00", "19:00", "20:00", "21:00", "22:00", "23:00",
        ],
        "data_format": "grib",                     # Output format
        "download_format": "unarchived",            # Download as unarchived file
    }

    # Output file name and path
    fn = f"era5_data_{year}.grib"
    fp = os.path.join(data_folder, fn)

    # Initialize CDS API client
    client = cdsapi.Client()

    print("downloading: ", fp)
    # Download the data and save to file
    client.retrieve(dataset, request, fp)

