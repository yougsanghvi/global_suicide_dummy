# %% [markdown]
# ## 1. Libraries and File Paths

# %%
import pandas as pd
import os
import xarray as xr
import geopandas as gpd
import matplotlib.pyplot as plt
import rasterio
from rasterio.plot import show
import rioxarray as rxr
from dask.diagnostics import ProgressBar

# %%
# Setting file paths
base_dir = "/global/scratch/users/yougsanghvi"

# Paths for stagg aggregated files for a specific year
stagg_folder_path = os.path.join(base_dir, "aggregated_results_gdnat_usa")
stagg_filename = "gdnat_usa_agg_1979.csv"
stagg_data_path = os.path.join(stagg_folder_path, stagg_filename)

# Paths for stagg aggregated files for all years
stagg_filename_allyrs = "gdnat_usa_agg_all_years.csv"
stagg_data_path_allyrs = os.path.join(stagg_folder_path, stagg_filename_allyrs)

# Paths for raw Gdnat data
raw_foldername = "gdnat_tiff_files_by_yr"
raw_filename = "gdnat_1979.tif"
raw_filepath = os.path.join(base_dir, raw_foldername, raw_filename)

# Define paths for usa county shapefile
usa_county_dir = os.path.join(base_dir, "shapefiles")
usa_county_filename = "tl_2016_us_county_mortality.shp"
usa_county_path = os.path.join(usa_county_dir, usa_county_filename)

# Paths for raw gdnat data
data_dir_cullen = os.path.join(
    "/global",
    "scratch",
    "users",
    "cmolitor",
)

data_dir_gdnat = os.path.join(data_dir_cullen, "global_suicide")
gdnat_1_path = os.path.join(
    data_dir_gdnat,
    "climate_data",
    "gdnat_ACCESS-CM2_tas_1979-1999_v2025-02-11.zarr",
)

gdnat_2_path = os.path.join(
    data_dir_gdnat,
    "climate_data",
    "gdnat_ACCESS-CM2_tas_2000-2020_v2025-02-11.zarr",
)

# path to Maren's era5 degree days file

era5_dd_folderpath = os.path.join("data", "climatedata", "USA")
era5_dd_filename = "temp_degreedays_1986_1999_USA_ERA5_pop_weights.csv"
era5_dd_filepath = os.path.join(base_dir, era5_dd_folderpath, era5_dd_filename)

# %% [markdown]
# ## 2. Loading in Files

# %%
# Loading in county shapefile
usa_counties = gpd.read_file(usa_county_path)

# %%
# load file for all years combined
stagg_usa_data_allyrs = pd.read_csv(stagg_data_path_allyrs)

# Convert 'order_1' from monthly sum to approximate daily average
stagg_usa_data_allyrs["order_1_daily_avg"] = stagg_usa_data_allyrs["order_1"] / 30

# %%
# load file for specific year speficied in path before
stagg_usa_data = pd.read_csv(stagg_data_path)

# Convert 'order_1' from monthly sum to approximate daily average
stagg_usa_data['order_1_daily_avg'] = stagg_usa_data['order_1'] / 30

# summary statistics of the daily average
print(stagg_usa_data["order_1_daily_avg"].describe())

# %%
# load raw file for specific year

rds = rxr.open_rasterio(raw_filepath, masked=True)

# %%
# Load raw file for all years
gdnat_1 = xr.open_dataset(gdnat_1_path, chunks={"time": 365})
gdnat_2 = xr.open_dataset(gdnat_1_path, chunks={"time": 365})

# %%
# Load era5 DD files
era5_dd_data = pd.read_csv(era5_dd_filepath)

# %% [markdown]
# ## 3. Verifying file correctness

# %%
null_check = era5_dd_data.isnull().values.any()
era5_nas = era5_dd_data[era5_dd_data.isnull().any(axis=1)]
null_counts = era5_dd_data.isnull().sum()

era5_nas_poly_ids = era5_dd_data[era5_dd_data.isnull().any(axis=1)]['poly_id'].unique().tolist()

# %%
# Check for NA values in the entire DataFrame and sum them up per column
print("Number of NA values per column:")
print(stagg_usa_data.isnull().sum())

# Check for NA values in the entire DataFrame and get a boolean DataFrame
bool_na = stagg_usa_data.isnull()

# Check if there are any NA values in the entire DataFrame (returns a single boolean)
print("\nAre there any NA values in the DataFrame?")
print(stagg_usa_data.isnull().any().any())

# Get the total count of NA values in the entire DataFrame
print("\nTotal number of NA values in the DataFrame:")
print(stagg_usa_data.isnull().sum().sum())

nas_only_df = stagg_usa_data[stagg_usa_data.isnull().any(axis=1)]

# Display unique values of 'poly_id' in the filtered DataFrame
if 'poly_id' in nas_only_df.columns:
    unique_poly_ids = nas_only_df['poly_id'].unique()
    print("Unique values of 'poly_id' in the filtered DataFrame (nas_only.csv):")
    print(unique_poly_ids)
else:
    print("The 'poly_id' column does not exist in the filtered DataFrame.")

# %%
polys_tocheck = era5_dd_data[era5_dd_data['poly_id'].isin(unique_poly_ids)]

# %%
polys_tocheck
print(polys_tocheck['threshold_9_to_10'].isna().sum() / len(polys_tocheck) * 100)

# %% [markdown]
# ## 4. Plotting

# %%
# function copied from part d

def plot_climate_data(
    data,
    data_type, # 'county' or 'grid'
    usa_counties, # GeoDataFrame of US county boundaries
    column_name, # Name of the column (county) or variable (grid) containing the data values
    start_year=None, # Start year for temporal aggregation (inclusive).
    end_year=None,   # End year for temporal aggregation (inclusive).
    title=None, # Whole title input
    value_min=None, # Minimum value for the color scale (None means auto-determined by Matplotlib)
    value_max=None, # Maximum value for the color scale (None means auto-determined by Matplotlib)
    color_map="coolwarm", # Colormap for plotting
    lon_bounds=(-130, -65), # Longitude bounds for the plot extent
    lat_bounds=(23, 50),     # Latitude bounds for the plot extent
    key = "poly_id"
):
    """
    Generates a geospatial plot of climate data, supporting both county-level
    and grid-level input formats. The function performs temporal aggregation
    (annual or multi-year average) based on specified year ranges and overlays
    county boundaries.

    Args:
        data (pd.DataFrame or xr.DataArray):
            Input climate data.
            - If `data_type` is 'county': A pandas DataFrame containing monthly data
              with 'poly_id', 'year', 'month', and `column_name`.
            - If `data_type` is 'grid': An xarray DataArray containing monthly data
              with 'x'/'lon', 'y'/'lat' coordinates, and 'time'/'band' dimensions.
        data_type (str):
            Specifies the format of the input `data`: 'county' or 'grid'.
        usa_counties (gpd.GeoDataFrame):
            GeoDataFrame containing US county boundaries. Must include a 'GEOID'
            column, which will be cast to integer type for merging.
        column_name (str):
            The name of the column in the county DataFrame (e.g., 'order_1_daily_avg')
            or the variable name within the xarray DataArray (e.g., 'tas') that holds
            the climate values for plotting.
        start_year (int, optional):
            The beginning year for temporal aggregation (inclusive). If provided
            and `end_year` is None, `end_year` defaults to `start_year` (single-year average).
            If both `start_year` and `end_year` are None, aggregation occurs over
            all available years in the dataset. Defaults to None.
        end_year (int, optional):
            The ending year for temporal aggregation (inclusive). If provided
            without `start_year`, it is ignored, and aggregation occurs over
            all available years. Defaults to None.
        title (str, optional):
            The complete title string for the plot. If None, a default title is
            generated based on `data_type`, `column_name`, and the aggregated years.
            Defaults to None.
        value_min (float, optional):
            Minimum value for the color scale. If None, Matplotlib automatically
            determines the minimum based on the data's spread. Defaults to None.
        value_max (float, optional):
            Maximum value for the color scale. If None, Matplotlib automatically
            determines the maximum based on the data's spread. Defaults to None.
        color_map (str, optional):
            Matplotlib colormap to apply to the plot. Defaults to "coolwarm".
        lon_bounds (tuple, optional):
            A tuple (min_longitude, max_longitude) defining the horizontal
            extent of the plot. Defaults to (-130, -65) for mainland USA.
        lat_bounds (tuple, optional):
            A tuple (min_latitude, max_latitude) defining the vertical
            extent of the plot. Defaults to (23, 50) for mainland USA.

    Returns:
        None: Displays the generated plot.
    """

    processed_data = None
    plot_title_years_str = "" # String representation of the years for the plot title

    # Determine the effective start and end years for aggregation, and format title string
    if start_year is not None:
        if end_year is None:
            end_year = start_year # Single-year aggregation if only start_year is provided
        plot_title_years_str = f"{start_year}" if start_year == end_year else f"{start_year}-{end_year}"
    else:
        # If no specific years are provided, average over the entire dataset period
        plot_title_years_str = "All Years"


    # Ensure 'GEOID' column in the county GeoDataFrame is of integer type
    if 'GEOID' in usa_counties.columns:
        usa_counties["GEOID"] = usa_counties["GEOID"].astype(int)

    if data_type == 'county':
        # --- County Data Processing ---
        # Calculate the annual average temperature per polygon ('poly_id') and year
        # from the raw monthly data.
        annual_avg_per_county_year = (
            data.groupby([key, "year"])[column_name]
            .mean()
            .reset_index()
        )

        filtered_annual_data = annual_avg_per_county_year.copy()

        # Apply temporal filtering if start_year is specified
        if start_year is not None:
            filtered_annual_data = filtered_annual_data[
                filtered_annual_data["year"].between(start_year, end_year)
            ]

        # Compute the final average over the filtered annual data per polygon.
        # This yields either a single year's annual average or an average over the
        # specified multi-year range.
        processed_data_avg = filtered_annual_data.groupby(key)[column_name].mean().reset_index()

        # Merge the aggregated data with the US county GeoDataFrame using GEOID.
        processed_data = usa_counties.merge(
            processed_data_avg, how="left", left_on="GEOID", right_on=key
        )
        # Set the plot title. If a custom title is not provided, generate a default.
        if title is None:
            plot_title = f"Average Temperature ({column_name}) - {plot_title_years_str} (Post-Aggregation)"
        else:
            plot_title = title

    elif data_type == 'grid':
        # --- Grid Data Processing ---
        grid_data_var = None

        # Determine if 'data' is a Dataset (collection of variables) or a DataArray (the variable itself)
        if isinstance(data, xr.Dataset):
            # If 'data' is a Dataset, extract the specific DataArray by 'column_name'
            if column_name not in data.data_vars:
                raise ValueError(f"'{column_name}' not found in the xarray.Dataset variables.")
            grid_data_var = data[column_name]
        elif isinstance(data, xr.DataArray):
            # If 'data' is already a DataArray, it is the variable to plot.
            # 'column_name' is then used as a label for units conversion and title.
            grid_data_var = data
            # Optionally, ensure the DataArray has a name for consistency if 'column_name' is intended as its name
            if grid_data_var.name is None:
                 grid_data_var.name = column_name # Assign column_name as the DataArray's name if unset
        else:
            raise TypeError("Expected xarray.Dataset or xarray.DataArray for grid data.")


        # Shift longitudes from 0-360 to -180-180 if needed.
        # This handles common global grid conventions. Sort by longitude after adjustment.
        if 'x' in grid_data_var.coords and grid_data_var.x.max() > 180:
            grid_data_var = grid_data_var.assign_coords({"x": ((grid_data_var.x + 180) % 360) - 180}).sortby("x")
        elif 'lon' in grid_data_var.coords and grid_data_var.lon.max() > 180:
            grid_data_var = grid_data_var.assign_coords({"lon": ((grid_data_var.lon + 180) % 360) - 180}).sortby("lon")

        averaged_grid = None

        # Perform temporal aggregation for grid data.
        # Prioritize 'time' dimension for explicit date-based filtering.
        if 'time' in grid_data_var.dims:
            if start_year is not None:
                # Select data within the specified year range and compute mean over time.
                start_date_str = f"{start_year}-01-01"
                end_date_str = f"{end_year}-12-31" # Inclusive end of year
                grid_time_filtered = grid_data_var.sel(time=slice(start_date_str, end_date_str))
                averaged_grid = grid_time_filtered.mean(dim="time", skipna=True)
            else:
                # If no specific years are provided, average over the entire 'time' dimension.
                averaged_grid = grid_data_var.mean(dim="time", skipna=True)
        elif 'band' in grid_data_var.dims:
            # If 'band' is the primary time-like dimension, average across bands.
            # Note: direct year filtering isn't applied here unless 'band' values
            # explicitly map to years, which is not assumed by default for 'band'.
            averaged_grid = grid_data_var.mean(dim="band", skipna=True)
            if start_year is not None: # Reflect provided years in title if available
                plot_title_years_str = f"{start_year}" if start_year == end_year else f"{start_year}-{end_year}"
            else:
                plot_title_years_str = "All Bands"
        else:
            # If no suitable time-like dimension is found, plot the original DataArray.
            print("Warning: Neither 'time' nor 'band' dimension found for averaging in grid data. Plotting original data.")
            averaged_grid = grid_data_var
            plot_title_years_str = "Original Data"

        # Convert temperature from Kelvin to Celsius if the column name suggests it's in Kelvin.
        if column_name == 'tas':
             processed_data = averaged_grid - 273.15
        else:
             processed_data = averaged_grid

        # Ensure the CRS is explicitly set for the xarray DataArray to allow proper
        # reprojection and alignment with county boundaries during plotting.
        if not processed_data.rio.crs:
            processed_data = processed_data.rio.write_crs("EPSG:4326", inplace=True) # Assume WGS84 for lat/lon data

        # Set the plot title. If a custom title is not provided, generate a default.
        if title is None:
            plot_title = f"Average Temperature ({column_name}) - {plot_title_years_str} Grid"
        else:
            plot_title = title

    else:
        # Handle invalid data_type input.
        print(f"Error: Invalid 'data_type': '{data_type}'. Please use 'county' or 'grid'.")
        return

    # --- Common Plotting Logic ---
    fig, ax = plt.subplots(1, 1, figsize=(12, 8))

    # Construct a dictionary of plot arguments, conditionally including `vmin` and `vmax`.
    # Matplotlib will auto-scale if these are not provided.
    plot_kwargs = {
        'ax': ax,
        'cmap': color_map,
        # The 'legend' argument is only valid for certain plot types (e.g., GeoPandas plots with 'column').
        # xarray's .plot() handles colorbars automatically, and passing 'legend' can cause errors
        # with its underlying matplotlib QuadMesh object.
    }

    if value_min is not None:
        plot_kwargs['vmin'] = value_min
    if value_max is not None:
        plot_kwargs['vmax'] = value_max

    if data_type == 'county':
        # For county plots, the 'legend' argument controls the colorbar.
        plot_kwargs['legend'] = True
        # Plot county boundaries as a base layer.
        usa_counties.boundary.plot(ax=ax, color="gray", linewidth=0.2)
        # Plot the merged county data.
        processed_data.plot(
            column=column_name,
            **plot_kwargs
        )
    elif data_type == 'grid':
        # Use ProgressBar context for visualizing computation progress of large datasets.
        with ProgressBar():
            # Plot the grid data.
            processed_data.plot(
                **plot_kwargs # legend is not passed here
            )
        # Overlay county boundaries, ensuring they are reprojected to match the grid's CRS.
        usa_counties.to_crs(processed_data.rio.crs).boundary.plot(
            ax=ax, color="grey", linewidth=0.3
        )

    # Set the geographical extents of the plot to focus on mainland USA.
    ax.set_xlim(lon_bounds[0], lon_bounds[1])
    ax.set_ylim(lat_bounds[0], lat_bounds[1])

    # Apply the determined plot title and remove axis labels/ticks for a cleaner map.
    ax.set_title(plot_title)
    ax.axis("off")
    plt.show()


# %%
# Post aggregation for 1979

plot_climate_data(
    data=stagg_usa_data,  
    data_type='county',
    usa_counties=usa_counties,  
    column_name='order_1_daily_avg',
    start_year=1979,
    end_year=1979,
    title="Annual Average Post-Aggregation - 1979",
    value_min=-5,
    value_max=30,
    color_map="coolwarm",
    lon_bounds=(-130, -65),
    lat_bounds=(23, 50)
)

# %%
# Pre aggregation for 1979

plot_climate_data(
    data=rds,  
    data_type='grid',
    usa_counties=usa_counties,  
    column_name='tas',
    start_year=1979,
    end_year=1979,
    title="Annual Average Pre-Aggregation - 1979",
    value_min=-5,
    value_max=30,
    color_map="coolwarm",
    lon_bounds=(-130, -65),
    lat_bounds=(23, 50)
)

# %%
plot_climate_data(
    data=era5_dd_data,  # Your original monthly county data DataFrame
    data_type='county',
    usa_counties=usa_counties,  # Your GeoDataFrame of US county boundaries
    column_name='threshold_10_to_29',
    title="Degree Days",
    color_map="coolwarm",
    lon_bounds=(-130, -65),
    lat_bounds=(23, 50)
)

# %%
# Post aggregation for all years

plot_climate_data(
    data=stagg_usa_data,  
    data_type='county',
    usa_counties=usa_counties,  
    column_name='order_1_daily_avg',
    title="Annual Average Post-Aggregation - 1979-2004",
    value_min=-5,
    value_max=30,
    color_map="coolwarm",
    lon_bounds=(-130, -65),
    lat_bounds=(23, 50)
)

# %%
# Pre aggregation for all years

plot_climate_data(
    data=rds,  
    data_type='grid',
    usa_counties=usa_counties,  
    column_name='tas',
    title="Annual Average Pre-Aggregation - 1979-2004",
    value_min=-5,
    value_max=30,
    color_map="coolwarm",
    lon_bounds=(-130, -65),
    lat_bounds=(23, 50)
)

# %%
# Sample time series plot
# this is converted to a function in part f

county_name = "New York"
# Find specific county polygon ID directly by name
chosen_county = usa_counties.loc[
    usa_counties["NAME"] == county_name, "GEOID"
].values[0]

print(chosen_county)

# Filter stagg data for Alameda county
chosen_county__data = stagg_usa_data[stagg_usa_data["poly_id"] == chosen_county]

print(chosen_county__data)

# Sort by year and month
chosen_county__data = chosen_county__data.sort_values(["year", "month"])

# Create a datetime column for plotting
chosen_county__data["date"] = pd.to_datetime(
    dict(year=chosen_county__data["year"], month=chosen_county__data["month"], day=1)
)

# Plot
fig, ax = plt.subplots(figsize=(12, 6))
ax.plot(
    chosen_county__data["date"], chosen_county__data["order_1_daily_avg"], marker="o", linestyle="-"
)

ax.set_title(f"Monthly Average Temperature - {county_name}")
ax.set_xlabel("Date")
ax.set_ylabel("Temperature (°C)")
ax.grid(True)

plt.show()



