#!/bin/bash
#SBATCH --job-name=era5-gdnat-diff
#SBATCH --account=co_carleton
#SBATCH --partition=savio4_htc
#SBATCH --time=2:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --array=0-4
#SBATCH --output=slurm_logs/%A_%a.out
#SBATCH --error=slurm_logs/%A_%a.err
#SBATCH --mail-user=yougsanghvi@berkeley.edu
#SBATCH --mail-type=ALL

module purge  # Clean environment

cd ~/global_suicide_dummy  # Change to your project directory

PYTHON_SCRIPT="./code/a_era5_download_cleaning/06_era5_gdnat_diff.py" 
LOG_FILE="./slurm_logs/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.log"

source ./climate-env/bin/activate  # Activate virtual environment
echo "Using Python: $(which python)"

# Map array index (0-4) to actual year (2000-2004)
year=$((2000 + SLURM_ARRAY_TASK_ID))

# Run the Python script with the mapped year
python $PYTHON_SCRIPT $year >& $LOG_FILE
