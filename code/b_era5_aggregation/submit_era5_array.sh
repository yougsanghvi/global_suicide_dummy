#!/bin/bash
#SBATCH --job-name=era5_agg_array
#SBATCH --account=co_carleton
#SBATCH --partition=savio4_htc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=50
#SBATCH --time=10:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yougsanghvi@berkeley.edu
#SBATCH --output=slurm_logs/era5_agg_array_%A_%a.out
#SBATCH --error=slurm_logs/era5_agg_array_%A_%a.err
#SBATCH --array=0-1%1

# This script processes years in parallel using SLURM job arrays
# Array 0-5 represents 26 years (1979-2004)
# The %10 limits to 10 concurrent jobs

# Load required modules
module load r-spatial
module load r

# Create logs directory if it doesn't exist
mkdir -p slurm_logs

# Calculate actual year from array index
BASE_YEAR=1989
YEAR=$((BASE_YEAR + SLURM_ARRAY_TASK_ID))

echo "Job started at: $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Array Task ID: $SLURM_ARRAY_TASK_ID"
echo "Processing year: $YEAR"
echo "Node: $SLURM_NODELIST"

# Run R script for specific year
echo "Starting ERA5 aggregation for year $YEAR..."
Rscript code/b_era5_aggregation/z_stagg_aggregation_forslurm_yrlyandpoly.R $YEAR

echo "The script for year $YEAR terminated at: $(date). Please check the error logs and output folder to verify successful completion."
# End of script
