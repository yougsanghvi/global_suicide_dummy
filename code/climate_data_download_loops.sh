#!/bin/bash
#SBATCH --job-name=dta-download
#SBATCH --account=fc_carleton2025
#SBATCH --partition=savio3
#SBATCH --time=40:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --output slurm_logs/%j.out
#SBATCH --error slurm_logs/%j.err
#SBATCH --mail-user=yougsanghvi@berkeley.edu
#SBATCH --mail-type=ALL

module purge

cd $SLURM_SUBMIT_DIR
PYTHON_SCRIPT="./code/climate_data_download_loops.py"
LOG_FILE="./slurm_logs/$SLURM_JOB_ID.log"

source ./climate-env/bin/activate
echo $(which python)

python $PYTHON_SCRIPT >& $LOG_FILE