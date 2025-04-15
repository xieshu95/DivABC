#!/bin/bash
#SBATCH --time=2-23:05:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=mumcmc
#SBATCH --output=logs/MCMC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
param_space_name=$1
param_set=$2
idparsopt_lam1=$3
idparsopt_lam2=$4
idparsopt_lam3=$5
idparsopt_mu1=$6
idparsopt_mu2=$7
idparsopt_mu3=$8
idparsopt_q12=$9
idparsopt_q13=$10
idparsopt_q21=$11
idparsopt_q23=$12
idparsopt_q31=$13
idparsopt_q32=$14


ml R
Rscript DivABC/scripts/run_MCMC_musse_peregrine.R ${param_space_name} \
                                                  ${param_set} \
                                                  ${idparsopt_lac1} \
                                                  ${idparsopt_lac2} \
                                                  ${idparsopt_lac3} \
                                                  ${idparsopt_mu1} \
                                                  ${idparsopt_mu2} \
                                                  ${idparsopt_mu3} \
                                                  ${idparsopt_q12} \
                                                  ${idparsopt_q13} \
                                                  ${idparsopt_q21} \
                                                  ${idparsopt_q23} \
                                                  ${idparsopt_q31} \
                                                  ${idparsopt_q32}
