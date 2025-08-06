import pandas as pd
import calendar
import os
import sys

# Importing file paths from utils 
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))  # where script lives
print(f"[DEBUG] SCRIPT_DIR: {SCRIPT_DIR}")

CODE_DIR = os.path.abspath(os.path.join(SCRIPT_DIR, ".."))  # one level up
print(f"[DEBUG] CODE_DIR: {CODE_DIR}")
print(f"[DEBUG] Contents of CODE_DIR: {os.listdir(CODE_DIR)}")

sys.path.append(CODE_DIR)

from y_utils import config

# ========= USER INPUTS ========= #
start_year = 1979
end_year = 2005
data = 'era5'  # 'gdnat' or 'era5'
model_list = ["MIROC6", "ACCESS-ESM1-5", "CanESM5", "MRI-ESM2-0", "NorESM2-LM", "ACCESS-CM2"] # Only used for GDNat
# =============================== #

def get_days_in_month(year, month):
    return calendar.monthrange(year, month)[1]

def safe_days_in_month(row):
    try:
        if pd.isna(row['year']) or pd.isna(row['month']):
            return pd.NA
        return get_days_in_month(int(row['year']), int(row['month']))
    except Exception:
        return pd.NA

def process_file(year, model_name=None):
    if data == 'gdnat':
        if model_name is None:
            raise ValueError("GDNat requires a model name.")
        file_path = config.get_gdnat_agg_yearly(year, model=model_name)
    elif data == 'era5':
        file_path = config.get_era5_agg_yearly(year)
    else:
        raise ValueError(f"Invalid data source: {data}")

    if not os.path.exists(file_path):
        print(f"[WARNING] File not found: {file_path}")
        return

    print(f"[INFO] Processing year: {year} ({model_name if model_name else 'ERA5'})")
    df_orig = pd.read_csv(file_path)
    orig_nrows = len(df_orig)

    required_cols = ['year', 'month', 'order_1', 'order_2', 'order_3', 'order_4']
    if not all(col in df_orig.columns for col in required_cols):
        print(f"[ERROR] Missing required columns in {file_path}")
        return

    df = df_orig.copy()
    df['days_in_month'] = df.apply(safe_days_in_month, axis=1)

    na_months = df['days_in_month'].isna().sum()
    if na_months > 0:
        print(f"[WARNING] {na_months} rows missing/invalid month/year in {file_path}")

    for i in range(1, 5):
        col = f'order_{i}'
        avg_col = f'{col}_avg'
        if avg_col not in df.columns:
            df[avg_col] = df[col] / df['days_in_month']
        else:
            print(f"[INFO] Skipping {avg_col}, already exists.")

    df.drop(columns='days_in_month', inplace=True)

    if len(df) != orig_nrows:
        print(f"[ERROR] Row count mismatch in {file_path}. Skipping save.")
        return

    df.to_csv(file_path, index=False)
    print(f"[INFO] Successfully updated file: {file_path}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python divide_orders_by_days.py <base_year> <offset>")
        sys.exit(1)

    try:
        base_year = int(sys.argv[1])
        offset = int(sys.argv[2])
        year = base_year + offset

        if start_year <= year <= end_year:
            if data == 'gdnat':
                for model_name in model_list:
                    process_file(year, model_name=model_name)
            elif data == 'era5':
                process_file(year)
        else:
            print(f"[ERROR] Computed year {year} is out of range ({start_year}-{end_year})")
    except ValueError as e:
        print(f"[ERROR] ValueError: {e}")
