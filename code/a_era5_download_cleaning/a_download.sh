#!/bin/bash
#SBATCH --job-name=dta-download
#SBATCH --account=co_carleton
#SBATCH --partition=savio4_htc
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --output slurm_logs/%j.out
#SBATCH --error slurm_logs/%j.err
#SBATCH --mail-user=yougsanghvi@berkeley.edu
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=24

module purge
cd ~/global_suicide_dummy
PYTHON_SCRIPT="./code/a_era5_download_cleaning/a_download.py"
LOG_FILE="./slurm_logs/$SLURM_JOB_ID.log"
source ./climate-env/bin/activate
echo $(which python)
python $PYTHON_SCRIPT >& $LOG_FILE