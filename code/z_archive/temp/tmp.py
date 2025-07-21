import pandas as pd
import numpy as np
import os
from datetime import datetime

# Define output directory and file name
output_dir = "/global/scratch/projects/co_carleton"
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
filename = f"random_data_{timestamp}.csv"
output_path = os.path.join(output_dir, filename)

# Generate random DataFrame
np.random.seed(42)  # for reproducibility
n_rows = 100
data = {
    "id": range(1, n_rows + 1),
    "random_int": np.random.randint(0, 1000, size=n_rows),
    "random_float": np.random.rand(n_rows),
    "category": np.random.choice(["A", "B", "C"], size=n_rows),
    "timestamp": pd.date_range("2022-01-01", periods=n_rows, freq="D")
}
df = pd.DataFrame(data)

# Write CSV
df.to_csv(output_path, index=False)
print(f"Random CSV file written to:\n{output_path}")
