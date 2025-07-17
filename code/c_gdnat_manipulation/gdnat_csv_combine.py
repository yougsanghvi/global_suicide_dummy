import os
import pandas as pd
from tqdm import tqdm

# ---- CONFIG ----
folder_path = "/global/scratch/users/yougsanghvi/aggregated_results_gdnat_usa"
start_year = 1979  # <-- set your start year
end_year = 2004    # <-- set your end year

# ---- LOAD & COMBINE ----
all_dfs = []
for year in tqdm(range(start_year, end_year + 1), desc="Combining yearly files"):
    filename = f"gdnat_usa_agg_{year}.csv"
    filepath = os.path.join(folder_path, filename)

    if os.path.exists(filepath):
        df = pd.read_csv(filepath)
        all_dfs.append(df)
    else:
        print(f"Warning: File for year {year} not found: {filepath}")

# ---- COMBINE ----
if all_dfs:
    combined_df = pd.concat(all_dfs, ignore_index=True)
    print(f"\n✅ Combined {len(all_dfs)} files. Total rows: {len(combined_df):,}")
else:
    print("\n❌ No files loaded.")

# ---- OPTIONAL: Save to file ----
output_path = os.path.join(folder_path, f"gdnat_usa_agg_{start_year}_{end_year}.csv")
combined_df.to_csv(output_path, index=False)
print(f"📁 Saved combined file to: {output_path}")

print(combined_df.head())
