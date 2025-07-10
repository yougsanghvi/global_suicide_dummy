# code incomplete and in progress!

#!/bin/bash
#SBATCH --job-name=era5_daily
#SBATCH --output=logs/era5_daily_%A_%a.out
#SBATCH --error=logs/era5_daily_%A_%a.err
#SBATCH --time=02:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=2
#SBATCH --array=2000-2004

module load python/3.8  # or appropriate module

YEAR=${SLURM_ARRAY_TASK_ID}

python process_era5_year.py $YEAR
