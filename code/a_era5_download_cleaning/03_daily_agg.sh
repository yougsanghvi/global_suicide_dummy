#!/bin/bash
#SBATCH --job-name=era5_daily
#SBATCH --account=co_carleton
#SBATCH --partition=savio4_htc
#SBATCH --time=6:00:00
#SBATCH --cpus-per-task=5
#SBATCH --array=0-20
#SBATCH --output=slurm_logs/%A_%a.out
#SBATCH --error=slurm_logs/%A_%a.err
#SBATCH --mail-user=yougsanghvi@berkeley.edu

module purge  # Clean environment
cd ~/global_suicide_dummy  # Change to your project directory

PYTHON_SCRIPT="./code/a_era5_download_cleaning/04_daily_agg.py" 

source ./climate-env/bin/activate  # Activate virtual environment
echo "Using Python: $(which python)"

START_YEAR=2004
YEAR=$((START_YEAR + SLURM_ARRAY_TASK_ID))
python $PYTHON_SCRIPT $YEAR

