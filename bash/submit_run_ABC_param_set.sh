#!/bin/bash
#SBATCH --time=9-23:05:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=run_ABC
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
param_space_name=$1
param_set=$2
idparsopt=$3

ml R
Rscript TraisieABC/scripts/run_ABC_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt}
