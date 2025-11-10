#!/bin/bash
#SBATCH --time=5:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=ref
#SBATCH --output=logs/ref-%j.log
#SBATCH --mem=3GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
id=${1}



ml R
Rscript DivABC/scripts/run_ref_table_peregrine_geosse.R ${id}