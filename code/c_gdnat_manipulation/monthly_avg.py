import pandas as pd
import calendar
import os
import sys

# Importing file paths from utils 
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))  # where monthly_avg.py lives
print(f"[DEBUG] SCRIPT_DIR: {SCRIPT_DIR}")

CODE_DIR = os.path.abspath(os.path.join(SCRIPT_DIR, ".."))  # one level up to 'code'
print(f"[DEBUG] CODE_DIR: {CODE_DIR}")
print(f"[DEBUG] Contents of CODE_DIR: {os.listdir(CODE_DIR)}")

sys.path.append(CODE_DIR)

from y_utils import config

# ========= USER INPUTS ========= #
start_year = 1979
end_year = 2020
# =============================== #

def get_days_in_month(year, month):
    """Return number of days in a given month."""
    return calendar.monthrange(year, month)[1]

def safe_days_in_month(row):
    """Safely return number of days or NA if month/year is missing or invalid."""
    try:
        if pd.isna(row['year']) or pd.isna(row['month']):
            return pd.NA
        return get_days_in_month(int(row['year']), int(row['month']))
    except Exception:
        return pd.NA

def process_file(year):
    file_path = config.get_gdnat_agg_yearly(year)

    if not os.path.exists(file_path):
        print(f"[WARNING] File not found: {file_path}")
        return

    print(f"[INFO] Processing year: {year}")
    df_orig = pd.read_csv(file_path)
    orig_nrows = len(df_orig)

    required_cols = ['year', 'month', 'order_1', 'order_2', 'order_3', 'order_4']
    if not all(col in df_orig.columns for col in required_cols):
        print(f"[ERROR] Missing required columns in {file_path}")
        return

    df = df_orig.copy()

    # Calculate days in month safely
    df['days_in_month'] = df.apply(safe_days_in_month, axis=1)

    na_months = df['days_in_month'].isna().sum()
    if na_months > 0:
        print(f"[WARNING] {na_months} rows have missing or invalid month/year in {file_path}. order_x_avg will be NA for these rows.")

    # Compute and store avg columns
    for i in range(1, 5):
        col = f'order_{i}'
        avg_col = f'{col}_avg'
        df[avg_col] = df[col] / df['days_in_month']

    df.drop(columns='days_in_month', inplace=True)

    # Check row count before saving
    if len(df) != orig_nrows:
        print(f"[ERROR] Row count mismatch for {file_path}: original={orig_nrows}, modified={len(df)}. Not saving file.")
        return

    # Save file (overwrite)
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
            process_file(year)
        else:
            print(f"[ERROR] Computed year {year} is out of range ({start_year}-{end_year})")
    except ValueError:
        print(f"[ERROR] Invalid input: {sys.argv[1]} {sys.argv[2]}")
