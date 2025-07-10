#!/bin/bash
#SBATCH --job-name=dta-download
#SBATCH --account=co_carleton
#SBATCH --partition=savio4_htc
#SBATCH --time=60:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --output slurm_logs/%j.out
#SBATCH --error slurm_logs/%j.err
#SBATCH --mail-user=yougsanghvi@berkeley.edu
#SBATCH --mail-type=ALL

module purge  # Unload all loaded modules for a clean environment

cd ~/global_suicide_dummy  # Change to project directory

PYTHON_SCRIPT="./code/a_era5_download_cleaning/02_download.py"  # Python script to run
LOG_FILE="./slurm_logs/$SLURM_JOB_ID.log"               # Log file for script output

source ./climate-env/bin/activate  # Activate Python virtual environment
echo $(which python)               # Print Python interpreter path

python $PYTHON_SCRIPT >& $LOG_FILE  # Run Python script and redirect output to log

