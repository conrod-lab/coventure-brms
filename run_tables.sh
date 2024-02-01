#!/bin/bash
#SBATCH --job-name=brms_array
#SBATCH --cpus-per-task=8 \
#SBATCH --mem=65G \
#SBATCH --output=/home/spinney/scratch/coventure/output/brms_%A_%a.out \
#SBATCH --error=/home/spinney/scratch/coventure/error/brms_%A_%a.err \
#SBATCH --array=1-1   # Set the range to match the number of models (e.g., 1 to 5)


module load r/4.2.2

# Define an array of R script names
# R_SCRIPTS=(
#   "Coventure_Anxiety_tables.R" 
#   "Coventure_DEPADO_dashed_tables.R" 
#   "Coventure_DEPADO_undashed_tables.R"  
#   "Coventure_Depression_tables.R")

R_SCRIPTS=(
  "Coventure_SDQ.R")

# Get the index (array task ID) to choose the R script
SCRIPT_INDEX=${SLURM_ARRAY_TASK_ID}

# Check if the index is valid
if [ $SCRIPT_INDEX -ge 1 ] && [ $SCRIPT_INDEX -le ${#R_SCRIPTS[@]} ]; then
  # Choose the R script based on the index
  SELECTED_SCRIPT=${R_SCRIPTS[$((SCRIPT_INDEX-1))]}
  echo "Running $SELECTED_SCRIPT"
  
  # Run the selected R script using Singularity
  Rscript /home/spinney/scratch/coventure/src/$SELECTED_SCRIPT
else
  echo "Invalid script index: $SCRIPT_INDEX"
fi
