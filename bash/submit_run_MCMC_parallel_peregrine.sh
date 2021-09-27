#!/bin/bash
#SBATCH --time=00:45:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=run_MCMC_parallel
#SBATCH --output=logs/MCMC_parallel-%j.log
#SBATCH --mem=2GB
#SBATCH --partition=short

# Arguments to follow the Rscript are as follows:
param_space_name=$1
idparsopt_lac=$2
idparsopt_mu=$3
idparsopt_gam=$4
idparsopt_laa=$5

ml R
Rscript TraisieABC/scripts/run_MCMC_parallel_peregrine.R ${param_space_name} \
                                               ${idparsopt_lac} \
                                               ${idparsopt_mu} \
                                               ${idparsopt_gam} \
                                               ${idparsopt_laa}