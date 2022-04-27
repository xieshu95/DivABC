#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=1GB
#SBATCH --partition=short


# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/TraisieABC@kernel')"

param_space_name=${1}
idparsopt_lac=${2}
idparsopt_mu=${3}
idparsopt_gam=${4}
idparsopt_laa=${5}
idparsopt_lac2=${6}
idparsopt_mu2=${7}
idparsopt_gam2=${8}
idparsopt_laa2=${9}
idparsopt_trans=${10}
idparsopt_trans2=${11}
sim_model=${12}

for_length=`wc -l TraisieABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 301; param_set <= 400; param_set++ ))
do
sbatch TraisieABC/bash/submit_run_ABC_param_set.sh ${param_space_name} \
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
                                                   ${sim_model}
done