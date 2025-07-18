#!/bin/bash
#SBATCH --job-name=era5_agg_array
#SBATCH --account=co_carleton
#SBATCH --partition=savio4_htc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=15
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yougsanghvi@berkeley.edu
#SBATCH --output=slurm_logs/era5_agg_array_%A_%a.out
#SBATCH --error=slurm_logs/era5_agg_array_%A_%a.err
#SBATCH --array=0-25

# This script processes years in parallel using SLURM job arrays
# Array 0-25 represents 26 years (1979-2004)
# The %5 limits to 5 concurrent jobs

# Load required modules
module load r-spatial
module load r

# Set up R environment
export R_LIBS_USER=$HOME/R/library

# Create logs directory if it doesn't exist
mkdir -p slurm_logs

# Calculate actual year from array index
BASE_YEAR=1979
YEAR=$((BASE_YEAR + SLURM_ARRAY_TASK_ID))

echo "Job started at: $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Array Task ID: $SLURM_ARRAY_TASK_ID"
echo "Processing year: $YEAR"
echo "Node: $SLURM_NODELIST"

# Run R script for specific year
echo "Starting ERA5 aggregation for year $YEAR..."
Rscript z_stagg_aggregation_forslurm.R $YEAR

echo "Completed processing year $YEAR at: $(date)"
