#!/bin/bash
#SBATCH --time=6-23:50:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=MCMC_DA
#SBATCH --output=logs/MCMC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
param_space_name=$1
param_set=$2
idparsopt_lac=$3
idparsopt_mu=$4
idparsopt_gam=$5
idparsopt_laa=$6

ml R
Rscript DivABC/scripts/run_MCMC_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt_lac} \
                                               ${idparsopt_mu} \
                                               ${idparsopt_gam} \
                                               ${idparsopt_laa}
