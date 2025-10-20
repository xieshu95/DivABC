#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=ref
#SBATCH --output=logs/ref-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
id=${1}



ml R
Rscript DivABC/scripts/run_ref_table_peregrine.R ${id}