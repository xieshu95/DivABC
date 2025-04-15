#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_MCMC
#SBATCH --output=logs/start_MCMC.log
#SBATCH --mem=1GB
#SBATCH --partition=regular


# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/DivABC@daisie')"

param_space_name=$1
idparsopt_lam1=$2
idparsopt_lam2=$3
idparsopt_lam3=$4
idparsopt_mu1=$5
idparsopt_mu2=$6
idparsopt_mu3=$7
idparsopt_q12=$8
idparsopt_q13=$9
idparsopt_q21=$10
idparsopt_q23=$11
idparsopt_q31=$12
idparsopt_q32=$13

for_length=`wc -l DivABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
  do
sbatch DivABC/bash/submit_run_bisse_MCMC_param_set.sh ${param_space_name} \
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
done
