#!/bin/bash
#SBATCH --job-name=avgmonthly_gdnat
#SBATCH --account=co_carleton
#SBATCH --partition=savio4_htc
#SBATCH --array=0-41  # 1979 to 2020 inclusive → 42 years → 0 to 41
#SBATCH --cpus-per-task=4
#SBATCH --time=01:30:00
#SBATCH --output=logs/divide_orders_%A_%a.out
#SBATCH --error=logs/divide_orders_%A_%a.err

# ==== CONFIG ====
module purge  # Clean environment
cd ~/global_suicide_dummy  # Change to your project directory

BASE_YEAR=1979
SCRIPT_PATH="code/c_gdnat_manipulation/monthly_avg.py"  

source ./climate-env/bin/activate  # Activate virtual environment
echo "Using Python: $(which python)"

python $SCRIPT_PATH $BASE_YEAR $SLURM_ARRAY_TASK_ID

