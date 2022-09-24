#!/bin/bash
#SBATCH --time=5-23:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=ss
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=gelifes

# Arguments to follow the Rscript are as follows:
param_space_name=${1}
param_set=${2}
idparsopt_lac=${3}
idparsopt_mu=${4}
idparsopt_gam=${5}
idparsopt_laa=${6}
idparsopt_lac2=${7}
idparsopt_mu2=${8}
idparsopt_gam2=${9}
idparsopt_laa2=${10}
idparsopt_trans=${11}
idparsopt_trans2=${12}
sim_model=${13}
ss_set=${14}


ml R
Rscript TraisieABC/scripts/run_ABC_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt_lac} \
                                               ${idparsopt_mu} \
                                               ${idparsopt_gam} \
                                               ${idparsopt_laa} \
                                               ${idparsopt_lac2} \
                                               ${idparsopt_mu2} \
                                               ${idparsopt_gam2} \
                                               ${idparsopt_laa2} \
                                               ${idparsopt_trans} \
                                               ${idparsopt_trans2} \
                                               ${sim_model} \
                                               ${ss_set}
