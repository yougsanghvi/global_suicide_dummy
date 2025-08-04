import pandas as pd

csv_fp = "/global/scratch/projects/co_carleton/carleton_colab/projects/global_suicide/merged_data_panel_extended_v3.csv"

# Read the CSV
df = pd.read_csv(csv_fp)

# Check if 'temp_diff' exists
if 'temp_diff' not in df.columns:
    required_cols = ['order_1_avg_era5', 'order_1_avg_gdnat']
    missing_cols = [col for col in required_cols if col not in df.columns]
    if missing_cols:
        raise ValueError(f"Missing required columns for temp_diff calculation: {missing_cols}")
    
    df['temp_diff'] = df['order_1_avg_era5'] - df['order_1_avg_gdnat']
    print("Added 'temp_diff' column.")
else:
    print("'temp_diff' column already exists. No changes made.")

# Save updated CSV
df.to_csv(csv_fp, index=False)
print(f"Saved updated CSV to {csv_fp}")
