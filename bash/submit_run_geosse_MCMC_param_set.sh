#!/bin/bash
#SBATCH --time=23:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=mcG
#SBATCH --output=logs/MCMC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=gelifes

# Arguments to follow the Rscript are as follows:
param_space_name=$1
param_set=$2
idparsopt_lam1=$3
idparsopt_lam2=$4
idparsopt_lam3=$5
idparsopt_mu1=$6
idparsopt_mu2=$7
idparsopt_q12=$8
idparsopt_q21=$9

ml R
Rscript DivABC/scripts/run_MCMC_geosse_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt_lam1} \
                                               ${idparsopt_lam2} \
                                               ${idparsopt_lam3} \
                                               ${idparsopt_mu1} \
                                               ${idparsopt_mu2} \
                                               ${idparsopt_q12} \
                                               ${idparsopt_q21}
