#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# git clone https://github.com/xieshu95/TraisieABC
# sbatch TraisieABC/bash/submit_run_ABC_peregrine.sh secsse_ABC 1 1 1 1 1 1 secsse



# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/TraisieABC@test_conti')"

param_space_name=${1}
idparsopt_lac1=${2}
idparsopt_lac2=${3}
idparsopt_mu1=${4}
idparsopt_mu2=${5}
idparsopt_trans12=${6}
idparsopt_trans21=${7}
sim_model=${8}
ss_set=${9}

for_length=`wc -l TraisieABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
do
sbatch TraisieABC/bash/submit_run_secsse_ABC_param_set.sh ${param_space_name} \
                                                   ${param_set} \
                                                   ${idparsopt_lac1} \
                                                   ${idparsopt_lac2} \
                                                   ${idparsopt_mu1} \
                                                   ${idparsopt_mu2} \
                                                   ${idparsopt_trans12} \
                                                   ${idparsopt_trans21} \
                                                   ${sim_model} \
                                                   ${ss_set}
done
