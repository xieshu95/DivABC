#!/bin/bash
#SBATCH --time=7-23:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=ge0
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=gelifes

# Arguments to follow the Rscript are as follows:
param_space_name=${1}
param_set=${2}
idparsopt_lam1=${3}
idparsopt_lam2=${4}
idparsopt_lam3=${5}
idparsopt_mu1=${6}
idparsopt_mu2=${7}
idparsopt_q1=${8}
idparsopt_q2=${9}
sim_model=${10}
ss_set=${11}


ml R
Rscript DivABC/scripts/run_ABC_bisse_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt_lam1} \
                                               ${idparsopt_lam2} \
                                               ${idparsopt_lam3} \
                                               ${idparsopt_mu1} \
                                               ${idparsopt_mu2} \
                                               ${idparsopt_q1} \
                                               ${idparsopt_q2} \
                                               ${sim_model} \
                                               ${ss_set}
