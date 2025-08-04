import pandas as pd
import sys
import os

# === Setup ===
sys.path.append("/global/home/users/yougsanghvi/global_suicide_dummy/code/y_utils/")
import functions
import config

models = ["MIROC6", "ACCESS-ESM1-5", "CanESM5", "MRI-ESM2-0", "NorESM2-LM", "ACCESS-CM2"]
merge_how = "inner"

# User flags: set True to overwrite (recompute) the column, False to skip if present
overwrite_pct_suicide_attr = False
overwrite_nsuicide_attr = False
overwrite_cfsuiciderate_attr = False

def compute_additional_columns(df):
    if overwrite_pct_suicide_attr or ("pct_suicide_attr" not in df.columns):
        print("Computing pct_suicide_attr")
        df["pct_suicide_attr"] = (df["y_hat_diff"] / df["suiciderate"]) * 100
        df["pct_suicide_attr"].replace([float("inf"), -float("inf")], pd.NA, inplace=True)

    if overwrite_nsuicide_attr or ("nsuicide_attr" not in df.columns):
        print("Computing nsuicide_attr")
        df["nsuicide_attr"] = df["y_hat_diff"] * (df["population"] / 100000)

    if overwrite_cfsuiciderate_attr or ("cfsuiciderate_attr" not in df.columns):
        print("Computing cfsuiciderate_attr")
        df["cfsuiciderate_attr"] = df["suiciderate"] - df["y_hat_diff"]

    print("Updated computed columns")
    return df

# === Loop over models ===
for model in models:
    print(f"\n===== Processing model: {model} =====")

    # Set paths
    model_dir = os.path.join(config.PROJECT_FOLDER, "results", model)

    attribution_output_fp = os.path.join(model_dir, f"attribution_output_{model}.csv")
    suicide_data_fp = config.SUICIDE_PANEL_FP
    output_file = os.path.join(model_dir, f"attribution_output_filtered_{model}.csv")

    # === Skip if output already exists ===
    if os.path.exists(output_file):
        print(f"Output file already exists for {model}: {output_file}. Skipping.")
        merged_df = pd.read_csv(output_file)
        merged_df = compute_additional_columns(merged_df)
        merged_df.to_csv(output_file, index=False)
        continue

    print("Loading datasets...")

    attribution_df = pd.read_csv(attribution_output_fp)
    suicide_df = pd.read_stata(suicide_data_fp)

    # Subset columns
    attr_cols = ['year', 'month', 'poly_id_int', 'diff_poly1', 'y_hat_diff', 'order_1_avg_gdnat', 'order_1_avg_era5']
    attribution_df = attribution_df[attr_cols]

    suicide_cols = [
        'fipsst', 'fipscty', 'fips', 'year', 'month', 'period',
        'num_of_suicide', 'suiciderate', 'population',
        'nsuicide_b1988', 'nsuicide_a1989', 'sol_rate_adj', 'sol_n_suicide',
        'sol_tmean', 'sol_prec', 'pop', 'sol_rate', 'adm1_id', 'adm2_id',
        'statename', 'countyname', 'tavg_poly1_aw', 'prec_poly1_aw'
    ]

    suicide_df = suicide_df[(suicide_df['gender'] == 0) & (suicide_df['agegroup'] == 0)][suicide_cols]
    suicide_df["adm2_id"] = suicide_df["adm2_id"].astype(int)
    suicide_df = suicide_df.rename(columns={"adm2_id": "poly_id_int"})

    # === Merge ===
    merge_keys = ["poly_id_int", "year", "month"]
    print("Merging on:", merge_keys)
    merged_df = pd.merge(attribution_df, suicide_df, on=merge_keys, how=merge_how)

    # === Check for duplicates ===
    dupes = merged_df.duplicated(subset=merge_keys, keep=False)
    if dupes.any():
        print("ERROR: Duplicate rows found for same (year, month, poly_id_int).")
        print(merged_df.loc[dupes, merge_keys + list(merged_df.columns.difference(merge_keys))])
        sys.exit(1)
    else:
        print("Merge successful: no duplicates.")

    # === Compute additional columns ===
    merged_df = compute_additional_columns(merged_df)

    # === Save output ===
    print(f"Saving filtered output to: {output_file}")
    merged_df.to_csv(output_file, index=False)

    print("✅ Done for model:", model)
    print(merged_df.head())

for model in models:
    print(f"\n===== Filtering full sample for model: {model} =====")

    # Set paths
    model_dir = os.path.join(config.PROJECT_FOLDER, "results", model)
    output_file = os.path.join(model_dir, f"attribution_output_filtered_{model}.csv")

    merged_df = pd.read_csv(output_file)

    # Determine full sample years in merged data
    all_years = merged_df["year"].unique()
    all_years.sort()
    n_years = len(all_years)

    # Count years per county
    years_per_county = merged_df.groupby("poly_id_int")["year"].nunique()

    # Keep only counties with data for all years
    counties_fullsample = years_per_county[years_per_county == n_years].index

    # Filter merged_df
    merged_df_fullsample = merged_df[merged_df["poly_id_int"].isin(counties_fullsample)].copy()

    # Save this filtered full-sample dataset separately
    fullsample_output_file = os.path.join(model_dir, f"attribution_output_fullsample_{model}.csv")
    print(f"Saving full-sample counties data to: {fullsample_output_file}")
    merged_df_fullsample.to_csv(fullsample_output_file, index=False)

