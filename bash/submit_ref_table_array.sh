#!/bin/bash
#SBATCH --time=4-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=rf
#SBATCH --output=logs/rf-%A.log
#SBATCH --mem=5GB
#SBATCH --cpus-per-task=1
#SBATCH --array=1-20
#SBATCH --partition=regular

module load R
CHUNK_ID=${SLURM_ARRAY_TASK_ID}
N_SIM_CHUNK=500
Rscript DivABC/scripts/bisse/gen_ref_table.R