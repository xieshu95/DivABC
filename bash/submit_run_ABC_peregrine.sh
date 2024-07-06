#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=1GB
#SBATCH --partition=regular


# git clone  https://github.com/xieshu95/TraisieABC.git
# sbatch TraisieABC/bash/submit_run_ABC_peregrine.sh DAISIE_ABC 1 0 0 0 0 0 0 0 0 0 DAISIE
# sbatch TraisieABC/bash/submit_run_ABC_peregrine.sh Traisie_ABC 1 0 0 0 1 0 0 0 0 0 TraiSIE

# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/TraisieABC@daisie')"

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
ss_set=${13}

for_length=`wc -l TraisieABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
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
                                                   ${sim_model} \
                                                   ${ss_set}
done
