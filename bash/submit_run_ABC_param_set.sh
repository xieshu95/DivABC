#!/bin/bash
#SBATCH --time=5-23:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=TE-1
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
param_space_name=${1}
param_set=${2}
idparsopt_lac=${3}
idparsopt_mu=${4}
idparsopt_gam=${5}
idparsopt_laa=${6}
idparsopt_K1=${7}
idparsopt_lac2=${8}
idparsopt_mu2=${9}
idparsopt_gam2=${10}
idparsopt_laa2=${11}
idparsopt_trans=${12}
idparsopt_trans2=${13}
idparsopt_K2=${14}
sim_model=${15}
ss_set=${16}


ml R
Rscript DivABC/scripts/run_ABC_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt_lac} \
                                               ${idparsopt_mu} \
                                               ${idparsopt_gam} \
                                               ${idparsopt_laa} \
                                               ${idparsopt_K1} \
                                               ${idparsopt_lac2} \
                                               ${idparsopt_mu2} \
                                               ${idparsopt_gam2} \
                                               ${idparsopt_laa2} \
                                               ${idparsopt_trans} \
                                               ${idparsopt_trans2} \
                                               ${idparsopt_K2} \
                                               ${sim_model} \
                                               ${ss_set}
