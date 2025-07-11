## libraries and File Paths
import os
import pandas as pd
import numpy as np
import statsmodels.api as sm
import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.cm import coolwarm
from matplotlib.colors import Normalize
from matplotlib.colors import TwoSlopeNorm
import matplotlib.colors as colors

# setting base directory 
dir = "/global/scratch/users/yougsanghvi"

# Input file paths:
# 1. setting suicide panel file path
suicide_proj_folder = "data"
suicide_panel_folderp = os.path.join("merged", "USA")
suicide_panel_filen = "USA_adm2_1968_2004_monthly.dta"
suicide_panel_filep = os.path.join(dir, suicide_proj_folder, suicide_panel_folderp, suicide_panel_filen)

# 2. Define paths for usa county shapefile
usa_county_dir = os.path.join(dir, "shapefiles")
usa_county_filename = "tl_2016_us_county_mortality.shp"
usa_county_path = os.path.join(usa_county_dir, usa_county_filename)

# 3. Specific folder for attribution results
attribution_output_folderpath = os.path.join(
    dir,
    "gdnat_era5_compare_output"
)

# 4. Define the modeling input filename
attribution_output_filename = "merged_data_panel_extended.csv"

# Construct the full file path for the prev modeling data
attribution_output_filepath = os.path.join(
    attribution_output_folderpath,
    attribution_output_filename
)

# --- Load data ---
attribution_output_data = pd.read_csv(attribution_output_filepath)
suicide_panel_data = pd.read_stata(suicide_panel_filep)
usa_counties = gpd.read_file(usa_county_path)
# Output file paths 

# path for intermediate outputs 
output_path_intermediate = os.path.join(dir, "gdnat_intermediate_outputs")
## Data Processing
# Filter and select columns
filtered_suicide = suicide_panel_data[
    (suicide_panel_data["agegroup"] == 0) & (suicide_panel_data["gender"] == 0)
][[
    'fipsst', 'fipscty', 'fips', 'year', 'month', 'num_of_suicide',
    'suiciderate', 'population', 'adm1_id', 'adm2_id',
    'statename', 'countyname'
]]

# Tagging counties with incomplete data 

fips_1979_1988 = set(
    filtered_suicide[
        (filtered_suicide['year'] >= 1979) & 
        (filtered_suicide['year'] <= 1988) &
        (~filtered_suicide['suiciderate'].isna())
    ]['fips'].unique()
)

fips_1989plus = set(
    filtered_suicide[
        (filtered_suicide['year'] >= 1989) &
        (~filtered_suicide['suiciderate'].isna())
    ]['fips'].unique()
)

# Identify categories
fips_all_years = fips_1979_1988 & fips_1989plus
fips_only_early = fips_1979_1988 - fips_1989plus

# Create mapping
def tag_missing(fips_str):
    if pd.isna(fips_str):
        return np.nan
    fips_int = int(fips_str)
    if fips_int in fips_all_years:
        return 0
    elif fips_int in fips_only_early:
        return 1
    else:
        return np.nan

# Apply tag to dataset
filtered_suicide['missing'] = filtered_suicide['fips'].apply(tag_missing)
complete_data_suicide = filtered_suicide[filtered_suicide['missing'] == 0]

# merging the dataset

# Ensure ID columns are integers
complete_data_suicide['fips'] = complete_data_suicide['fips'].astype(int)
attribution_output_data['poly_id'] = attribution_output_data['poly_id'].astype(int)

# Perform inner merge on year, month, and ID
merged_data = pd.merge(
    complete_data_suicide,
    attribution_output_data,
    how='inner',
    left_on=['year', 'month', 'fips'],
    right_on=['year', 'month', 'poly_id']
)



# Difference between climate and counterfactual models
merged_data['diff_suicide'] = merged_data['y_hat_era5'] - merged_data['y_hat_gdnat']

# Counterfactual suicide rate (removing climate effect)
merged_data['suiciderate_cf'] = merged_data['suiciderate'] - merged_data['diff_suicide']

# Add % suicide attributable to climate change directly to main df
merged_data['pct_suiciderate_cc'] = (merged_data['diff_suicide'] / merged_data['suiciderate']) * 100

merged_data.to_csv(os.path.join(output_path_intermediate,"suicide_data_withcf.csv"), index=False)

# This file only includes the ~500 counties with suicide data for all the years