#!/bin/bash
#SBATCH --time=6-23:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=s15
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=gelifes

# Arguments to follow the Rscript are as follows:
param_space_name=${1}
param_set=${2}
idparsopt_lac1=${3}
idparsopt_lac2=${4}
idparsopt_mu1=${5}
idparsopt_mu2=${6}
idparsopt_trans12=${7}
idparsopt_trans21=${8}
sim_model=${9}
ss_set=${10}


ml R
Rscript DivABC/scripts/run_ABC_bisse_peregrine.R ${param_space_name} \
                                               ${param_set} \
                                               ${idparsopt_lac1} \
                                               ${idparsopt_lac2} \
                                               ${idparsopt_mu1} \
                                               ${idparsopt_mu2} \
                                               ${idparsopt_trans12} \
                                               ${idparsopt_trans21} \
                                               ${sim_model} \
                                               ${ss_set}
