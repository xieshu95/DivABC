#!/bin/bash
#SBATCH --time=7-23:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=mu3
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
param_space_name=${1}
param_set=${2}
idparsopt_lac1=${3}
idparsopt_lac2=${4}
idparsopt_lac3=${5}
idparsopt_mu1=${6}
idparsopt_mu2=${7}
idparsopt_mu3=${8}
idparsopt_trans=${9}
sim_model=${10}
ss_set=${11}


ml R
Rscript DivABC/scripts/run_ABC_musse_peregrine.R ${param_space_name} \
${param_set} \
${idparsopt_lac1} \
${idparsopt_lac2} \
${idparsopt_lac3} \
${idparsopt_mu1} \
${idparsopt_mu2} \
${idparsopt_mu3} \
${idparsopt_trans} \
${sim_model} \
${ss_set}
