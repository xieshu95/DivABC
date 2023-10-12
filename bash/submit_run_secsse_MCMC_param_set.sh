#!/bin/bash
#SBATCH --time=2-23:05:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=mcT
#SBATCH --output=logs/MCMC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
scenario=$1
param_set=$2
idparsopt_lam1=$3
idparsopt_lam2=$4
idparsopt_mu1=$5
idparsopt_mu2=$6
idparsopt_q12=$7
idparsopt_q21=$8

ml R
Rscript TraisieABC/scripts/run_MCMC_secsse_peregrine.R ${scenario} \
                                               ${param_set} \
                                               ${idparsopt_lam1} \
                                               ${idparsopt_lam2} \
                                               ${idparsopt_mu1} \
                                               ${idparsopt_mu2} \
                                               ${idparsopt_q12} \
                                               ${idparsopt_q21}
