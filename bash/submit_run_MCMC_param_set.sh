#!/bin/bash
#SBATCH --time=5-23:05:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=mcte
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
Rscript TraisieABC/scripts/run_MCMC_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt_lac} \
                                               ${idparsopt_mu} \
                                               ${idparsopt_gam} \
                                               ${idparsopt_laa}
