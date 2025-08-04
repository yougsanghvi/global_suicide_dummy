import pandas as pd

import pandas as pd

def filter_and_avg_by_year(df, county_id_col, value_col, time_col=None, start_time=None, end_time=None, agg_method='mean', pop_col=None):
    """
    Filters and aggregates a value column from a dataframe over time.

    Parameters:
    - df (pd.DataFrame): Input dataframe.
    - county_id_col (str): Column identifying the county.
    - value_col (str): Column containing the values to be aggregated.
    - time_col (str, optional): Column indicating time (e.g., 'year', 'period').
    - start_time, end_time (int or str, optional): Time window for filtering.
    - agg_method (str): 'mean' or 'sum'.
    - pop_col (str, optional): If specified, perform population-weighted aggregation using this column.

    Returns:
    - pd.DataFrame: Aggregated dataframe with average or sum by time.
    """
    print(f"Running filter_and_avg for value_col = '{value_col}' (agg_method = {agg_method}, pop_col = {pop_col})")

    df = df.copy()

    # Filter by time range if specified
    if time_col:
        try:
            df[time_col] = pd.to_datetime(df[time_col], errors='coerce')
            start = pd.to_datetime(start_time) if start_time is not None else None
            end = pd.to_datetime(end_time) if end_time is not None else None
        except Exception:
            try:
                df[time_col] = pd.to_numeric(df[time_col], errors='coerce')
                start = float(start_time) if start_time is not None else None
                end = float(end_time) if end_time is not None else None
            except Exception as e:
                print(f"❌ Error parsing time columns or values: {e}")
                return None

        if start is not None:
            print(f"Filtering data between {start_time} and {end_time} on column '{time_col}'...")
            df = df[df[time_col] >= start]
        if end is not None:
            df = df[df[time_col] <= end]

        if df.empty:
            print("❌ No data found in specified time range after filtering.")
            return None
    else:
        print("No time range provided, aggregating over entire dataset period...")

    # If population column is specified, use it for weighted aggregation
    if pop_col:
        before_drop = len(df)
        df = df.dropna(subset=[value_col, pop_col])
        after_drop = len(df)
        print(f"Dropped {before_drop - after_drop} rows due to NA in {value_col} or {pop_col}")

        df["weighted_value"] = df[value_col] * df[pop_col]
        grouped = df.groupby(time_col).agg(
            weighted_sum=("weighted_value", "sum"),
            weight=(pop_col, "sum")
        )

        if agg_method == 'mean':
            result = (grouped["weighted_sum"] / grouped["weight"]).reset_index(name=value_col)
        elif agg_method == 'sum':
            print(f"❌ Invalid aggregation method '{agg_method}' with population weighting.")
        else:
            print(f"❌ Invalid aggregation method '{agg_method}' with population weighting.")
            return None
    else:
        if agg_method == 'mean':
            print("Aggregating using MEAN...")
            # doing this group by might break the maps which require sum over time 
            result = df.groupby(time_col)[value_col].mean().reset_index()
        elif agg_method == 'sum':
            print("Aggregating using SUM...")
            result = df.groupby(time_col)[value_col].sum().reset_index()
        else:
            print(f"❌ Invalid aggregation method '{agg_method}'. Use 'mean' or 'sum'.")
            return None

    return result


def filter_and_avg_by_county(df, county_id_col, value_col, time_col=None, start_time=None, end_time=None, agg_method='mean'):
    """
    Filter dataframe by time range (if provided) and return aggregated value_col grouped by county_id_col.

    Parameters:
    - df: pandas DataFrame
    - county_id_col: column name for county ID in df
    - value_col: column name for the value to aggregate
    - time_col: optional column name for time (str or datetime)
    - start_time: optional filter start time (string or numeric)
    - end_time: optional filter end time (string or numeric)
    - agg_method: 'mean' (default) or 'sum'

    Returns:
    - grouped aggregated DataFrame or None if filtering results in no data
    """

    import pandas as pd

    if time_col and start_time is not None and end_time is not None:
        print(f"Filtering data between {start_time} and {end_time} on column '{time_col}'...")
        try:
            df[time_col] = pd.to_datetime(df[time_col], errors='coerce')
            start = pd.to_datetime(start_time)
            end = pd.to_datetime(end_time)
        except Exception:
            try:
                df[time_col] = pd.to_numeric(df[time_col], errors='coerce')
                start = float(start_time)
                end = float(end_time)
            except Exception as e:
                print(f"❌ Error parsing time columns or values: {e}")
                return None
        df_filtered = df[(df[time_col] >= start) & (df[time_col] <= end)]
        if df_filtered.empty:
            print("❌ No data found in specified time range after filtering.")
            return None
    else:
        print("No time range provided, aggregating over entire dataset period...")
        df_filtered = df

    # Choose aggregation method
    if agg_method == 'sum':
        print("Aggregating using SUM...")
        df_agg = df_filtered.groupby(county_id_col)[value_col].sum().reset_index()
    elif agg_method == 'mean':
        print("Aggregating using MEAN...")
        df_agg = df_filtered.groupby(county_id_col)[value_col].mean().reset_index()
    else:
        print(f"❌ Invalid aggregation method '{agg_method}'. Use 'mean' or 'sum'.")
        return None

    df_agg.rename(columns={value_col: value_col}, inplace=True)
    return df_agg


def plot_county_data(
    shapefile_path,
    shapefile_id_col,
    county_data_path,
    county_id_col,
    value_col,
    title,
    time_col=None,
    start_time=None,
    end_time=None,
    vmin=None,
    vmax=None,
    crop_bounds=None,
    separate_files_by_year=False,
    agg_method='mean',
    show = True, 
    ax = None,
    crop_scale_pct = None,
):
    import geopandas as gpd
    import pandas as pd
    import matplotlib.pyplot as plt
    import sys

    print("[1/6] Loading shapefile...")
    sys.stdout.flush()
    try:
        gdf = gpd.read_file(shapefile_path)
    except Exception as e:
        print(f"❌ ERROR loading shapefile: {e}")
        return

    print("[2/6] Loading county-level data...")
    if separate_files_by_year:
        if not (yearly_raw_folder and yearly_raw_template and years):
            print("❌ Must provide yearly_raw_folder, yearly_raw_template, and years when separate_files_by_year=True")
            return
        df_list = []
        for yr in years:
            path = os.path.join(yearly_raw_folder, yearly_raw_template.format(year=yr))
            print(f"  Loading file for year {yr}: {path}")
            try:
                df_year = pd.read_csv(path)
                df_year['year'] = yr
                df_list.append(df_year)
            except FileNotFoundError:
                print(f"❌ File not found for year {yr}: {path}. Skipping this year.")
            except pd.errors.EmptyDataError:
                print(f"❌ File for year {yr} is empty: {path}. Skipping this year.")
            except Exception as e:
                print(f"❌ Error loading file for year {yr}: {e}. Skipping this year.")
        if not df_list:
            print("❌ No data loaded for any year, aborting.")
            return
        try:
            df = pd.concat(df_list, ignore_index=True)
        except Exception as e:
            print(f"❌ Error concatenating yearly dataframes: {e}")
            return
    else:
        try:
            df = pd.read_csv(county_data_path)
        except Exception as e:
            print(f"❌ Error loading county-level data: {e}")
            return


    print("[3/6] Validating column names...")
    shapefile_cols = gdf.columns.tolist()
    data_cols = df.columns.tolist()

    missing = False
    if shapefile_id_col not in shapefile_cols:
        print(f"❌ '{shapefile_id_col}' not found in shapefile columns.")
        print("Available shapefile columns:", shapefile_cols)
        missing = True
    if county_id_col not in data_cols:
        print(f"❌ '{county_id_col}' not found in county data columns.")
        print("Available county data columns:", data_cols)
        missing = True
    if value_col not in data_cols:
        print(f"❌ '{value_col}' not found in county data columns.")
        print("Available county data columns:", data_cols)
        missing = True

    if missing:
        return

    print("[4/6] Filtering and averaging data...")
    avg_df = filter_and_avg(df, county_id_col, value_col, time_col, start_time, end_time, agg_method)
    if avg_df is None:
        return

    print("[5/6] Merging county data with shapefile...")
    gdf[shapefile_id_col] = gdf[shapefile_id_col].astype(int)
    avg_df[county_id_col] = avg_df[county_id_col].astype(int)
    merged = gdf.merge(avg_df, left_on=shapefile_id_col, right_on=county_id_col, how='left')

    if crop_scale_pct is not None:
        col_values = merged[value_col].dropna()
        lower = col_values.quantile(crop_scale_pct / 100)
        upper = col_values.quantile(1 - crop_scale_pct / 100)
        limit = max(abs(lower), abs(upper))
        vmin = -limit
        vmax = limit
    else:
        if vmin is None and vmax is None:
            col_values = merged[value_col].dropna()
            vmax = col_values.abs().max()
            vmin = -vmax

    print("[6/6] Plotting map...")

    if ax is None:
        fig, ax = plt.subplots(figsize=(12, 8))

    merged.plot(
        column=value_col,
        cmap="coolwarm",
        linewidth=0.2,
        edgecolor="black",
        legend=True,
        vmin=vmin,
        vmax=vmax,
        ax=ax
    )
    ax.set_title(title)
    if crop_bounds:
        ax.set_xlim(crop_bounds[0], crop_bounds[1])
        ax.set_ylim(crop_bounds[2], crop_bounds[3])

    print("Done.")

    if show and ax is None:
        plt.tight_layout()
        plt.show()

def plot_county_diff(
    shapefile_path,
    shapefile_id_col,

    title,
    
    # For dataset 1
    county_data_path_1=None,
    county_id_col_1=None,
    value_col1=None,
    separate_files_by_year_1=False,
    yearly_raw_folder_1=None,
    yearly_raw_template_1=None,
    
    # For dataset 2
    county_data_path_2=None,
    county_id_col_2=None,
    value_col2=None,
    separate_files_by_year_2=False,
    yearly_raw_folder_2=None,
    yearly_raw_template_2=None,
    
    years = None,

    vmin=None,
    vmax=None,
    crop_bounds=None,
    time_col = None,
):

    import geopandas as gpd
    import pandas as pd
    import matplotlib.pyplot as plt
    import sys
    import os

    print("[1/8] Loading shapefile...")
    sys.stdout.flush()
    try:
        gdf = gpd.read_file(shapefile_path)
    except Exception as e:
        print(f"❌ ERROR loading shapefile: {e}")
        return

    print("[2/8] Loading first county-level dataset...")
    
    if separate_files_by_year_1:
        if not (yearly_raw_folder_1 and yearly_raw_template_1 and years):
            print("❌ Must provide yearly_raw_folder_1, yearly_raw_template_1, and years when separate_files_by_year_1=True")
            return
        df_list_1 = []
        for yr in years:
            path_1 = os.path.join(yearly_raw_folder_1, yearly_raw_template_1.format(year=yr))
            print(f"  Loading first dataset file for year {yr}: {path_1}")
            try:
                df_year_1 = pd.read_csv(path_1)
                df_year_1['year'] = yr
                df_list_1.append(df_year_1)
            except FileNotFoundError:
                print(f"❌ First dataset file not found for year {yr}: {path_1}. Skipping this year.")
            except pd.errors.EmptyDataError:
                print(f"❌ First dataset file for year {yr} is empty: {path_1}. Skipping this year.")
            except Exception as e:
                print(f"❌ Error loading first dataset file for year {yr}: {e}. Skipping this year.")
        if not df_list_1:
            print("❌ No data loaded for any year in first dataset, aborting.")
            return
        try:
            df_1 = pd.concat(df_list_1, ignore_index=True)
        except Exception as e:
            print(f"❌ Error concatenating first dataset yearly dataframes: {e}")
            return
    else:
        try:
            df_1 = pd.read_csv(county_data_path_1)
        except Exception as e:
            print(f"❌ Error loading first county-level data: {e}")
            return

    print("[3/8] Loading second county-level dataset...")
    if separate_files_by_year_2:
        if not (yearly_raw_folder_2 and yearly_raw_template_2 and years):
            print("❌ Must provide yearly_raw_folder_2, yearly_raw_template_2, and years when separate_files_by_year_2=True")
            return
        df_list_2 = []
        for yr in years:
            path_2 = os.path.join(yearly_raw_folder_2, yearly_raw_template_2.format(year=yr))
            print(f"  Loading second dataset file for year {yr}: {path_2}")
            try:
                df_year_2 = pd.read_csv(path_2)
                df_year_2['year'] = yr
                df_list_2.append(df_year_2)
            except FileNotFoundError:
                print(f"❌ Second dataset file not found for year {yr}: {path_2}. Skipping this year.")
            except pd.errors.EmptyDataError:
                print(f"❌ Second dataset file for year {yr} is empty: {path_2}. Skipping this year.")
            except Exception as e:
                print(f"❌ Error loading second dataset file for year {yr}: {e}. Skipping this year.")
        if not df_list_2:
            print("❌ No data loaded for any year in second dataset, aborting.")
            return
        try:
            df_2 = pd.concat(df_list_2, ignore_index=True)
        except Exception as e:
            print(f"❌ Error concatenating second dataset yearly dataframes: {e}")
            return
    else:
        try:
            df_2 = pd.read_csv(county_data_path_2)
        except Exception as e:
            print(f"❌ Error loading second county-level data: {e}")
            return


    print("[4/8] Validating column names...")
    shapefile_cols = gdf.columns.tolist()
    df1_cols = df_1.columns.tolist()
    df2_cols = df_2.columns.tolist()

    missing = False
    if shapefile_id_col not in shapefile_cols:
        print(f"❌ '{shapefile_id_col}' not found in shapefile columns.")
        print("Available shapefile columns:", shapefile_cols)
        missing = True
    if county_id_col_1 not in df1_cols:
        print(f"❌ '{county_id_col_1}' not found in first county data columns.")
        print("Available columns:", df1_cols)
        missing = True
    if value_col1 not in df1_cols:
        print(f"❌ '{value_col1}' not found in first county data columns.")
        print("Available columns:", df1_cols)
        missing = True
    if county_id_col_2 not in df2_cols:
        print(f"❌ '{county_id_col_2}' not found in second county data columns.")
        print("Available columns:", df2_cols)
        missing = True
    if value_col2 not in df2_cols:
        print(f"❌ '{value_col2}' not found in second county data columns.")
        print("Available columns:", df2_cols)
        missing = True
    if time_col and (time_col not in df1_cols or time_col not in df2_cols):
        print(f"❌ '{time_col}' not found in both county data columns.")
        print("First data columns:", df1_cols)
        print("Second data columns:", df2_cols)
        missing = True

    if missing:
        return

    start_time = years[0]  # will be 2000
    end_time = years[-1] 

    print("[5/8] Processing first dataset...")
    df1_avg = filter_and_avg(df_1, county_id_col_1, value_col1, time_col, start_time, end_time)
    if df1_avg is None:
        return

    print("[6/8] Processing second dataset...")
    df2_avg = filter_and_avg(df_2, county_id_col_2, value_col2, time_col, start_time, end_time)
    if df2_avg is None:
        return

    print("[7/8] Calculating difference and merging with shapefile....")

    df1_avg_renamed = df1_avg.rename(columns={value_col1: 'value_1', county_id_col_1: 'county_id'})
    df2_avg_renamed = df2_avg.rename(columns={value_col2: 'value_2', county_id_col_2: 'county_id'})

    df1_avg_renamed['county_id'] = df1_avg_renamed['county_id'].astype(int)
    df2_avg_renamed['county_id'] = df2_avg_renamed['county_id'].astype(int)

    # Merge on county_id
    merged_df = pd.merge(df1_avg_renamed, df2_avg_renamed, on='county_id', how='outer')

    # Compute difference column safely (fill missing values if needed)
    merged_df['diff'] = merged_df['value_1'] - merged_df['value_2']

    # Now merge with GeoDataFrame
    gdf[shapefile_id_col] = gdf[shapefile_id_col].astype(int)
    merged_df['county_id'] = merged_df['county_id'].astype(int)

    gdf_merged = gdf.merge(merged_df[['county_id', 'diff']], left_on=shapefile_id_col, right_on='county_id', how='left')


    print("[8/8] Plotting difference map...")
    fig, ax = plt.subplots(figsize=(12, 8))
    gdf_merged.plot(
        column='diff',
        cmap="coolwarm",
        linewidth=0.2,
        edgecolor="black",
        legend=True,
        vmin=vmin,
        vmax=vmax,
        ax=ax
    )
    ax.set_title(title)
    if crop_bounds:
        ax.set_xlim(crop_bounds[0], crop_bounds[1])
        ax.set_ylim(crop_bounds[2], crop_bounds[3])

    print("Done.")
    plt.tight_layout()
    plt.show()