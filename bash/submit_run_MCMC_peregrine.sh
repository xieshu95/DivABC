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
Rscript -e "remotes::install_github('xieshu95/TraisieABC@daisie')"

param_space_name=$1
idparsopt_lac=$2
idparsopt_mu=$3
idparsopt_gam=$4
idparsopt_laa=$5

for_length=`wc -l TraisieABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
do
sbatch TraisieABC/bash/submit_run_MCMC_param_set.sh ${param_space_name} \
                                                   ${param_set} \
                                                   ${idparsopt_lac} \
                                                   ${idparsopt_mu} \
                                                   ${idparsopt_gam} \
                                                   ${idparsopt_laa}
done
