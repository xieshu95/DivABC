#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# git clone https://github.com/xieshu95/DivABC
# sbatch DivABC/bash/submit_run_musse_ABC_peregrine.sh musse_ABC_test 1 1 1 1 1 1 1 1 1 1 1 1 musse 0
# sbatch DivABC/bash/submit_run_musse_ABC_param_set.sh musse_ABC_test 1 1 1 1 1 1 1 1 1 1 1 1 1 musse 0



# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/DivABC@daisie')"

param_space_name=${1}
idparsopt_lac1=${2}
idparsopt_lac2=${3}
idparsopt_lac3=${4}
idparsopt_mu1=${5}
idparsopt_mu2=${6}
idparsopt_mu3=${7}
idparsopt_trans=${8}
sim_model=${9}
ss_set=${10}

for_length=`wc -l DivABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
  do
sbatch DivABC/bash/submit_run_musse_ABC_param_set.sh ${param_space_name} \
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
done
