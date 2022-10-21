#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_MCMC
#SBATCH --output=logs/start_MCMC.log
#SBATCH --mem=1GB
#SBATCH --partition=short


# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/TraisieABC@secsse')"

param_space_name=$1
idparsopt_lam1=$2
idparsopt_lam2=$3
idparsopt_mu1=$4
idparsopt_mu2=$5
idparsopt_q12=$6
idparsopt_q21=$7

for_length=`wc -l TraisieABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
do
sbatch TraisieABC/bash/submit_run_secsse_MCMC_param_set.sh ${param_space_name} \
                                                   ${param_set} \
                                                   ${idparsopt_lam1} \
                                                   ${idparsopt_lam2} \
                                                   ${idparsopt_mu1} \
                                                   ${idparsopt_mu2} \
                                                   ${idparsopt_q12} \
                                                   ${idparsopt_q21}
done