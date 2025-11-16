#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=1GB
#SBATCH --partition=regular


# git clone -b daisie  https://github.com/xieshu95/DivABC.git
# sbatch DivABC/bash/submit_run_ABC_peregrine.sh DAISIE_ABC_DI 1 1 1 1 0 0 0 0 0 0 0 0 DAISIE_DI 1
# sbatch DivABC/bash/submit_run_ABC_param_set.sh DAISIE_ABC_DI 15 1 1 1 1 0 0 0 0 0 0 0 0 DAISIE_DI 3
# sbatch DivABC/bash/submit_run_ABC_peregrine.sh DAISIE_ABC_DD 1 1 1 1 1 0 0 0 0 0 0 0 DAISIE_DD 0
# sbatch DivABC/bash/submit_run_ABC_param_set.sh DAISIE_ABC_DD 1 1 1 1 1 1 0 0 0 0 0 0 0 DAISIE_DD 3
# sbatch DivABC/bash/submit_run_MCMC_param_set.sh DAISIE_MCMC_DI 1 1 1 1 1 0 DI
# sbatch DivABC/bash/submit_run_MCMC_peregrine.sh DAISIE_MCMC_DI 1 1 1 1 0 DI
# sbatch DivABC/bash/submit_run_MCMC_param_set.sh DAISIE_MCMC_DD 1 1 1 1 1 1 DD
# sbatch DivABC/bash/submit_run_MCMC_peregrine.sh DAISIE_MCMC_DD 1 1 1 1 1 DD

# sbatch DivABC/bash/submit_run_ABC_peregrine.sh Traisie_ABC 1 0 0 0 1 0 0 0 0 0 TraiSIE
# sbatch DivABC/bash/submit_run_bisse_ABC_peregrine.sh bisse_ABC_test 1 1 1 1 1 1 bisse 0
# sbatch DivABC/bash/submit_run_bisse_ABC_param_set.sh bisse_ABC_test 1 1 1 1 1 1 1 bisse 15
# sbatch DivABC/bash/submit_run_bisse_MCMC_param_set.sh bisse_MCMC_test 1 1 1 1 1 1 1
# sbatch DivABC/bash/submit_run_bisse_MCMC_peregrine.sh bisse_MCMC_test 1 1 1 1 1 1
# # for param_set in 5 6 16

# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/DivABC@daisie')"

param_space_name=${1}
idparsopt_lac=${2}
idparsopt_mu=${3}
idparsopt_gam=${4}
idparsopt_laa=${5}
idparsopt_K1=${6}
idparsopt_lac2=${7}
idparsopt_mu2=${8}
idparsopt_gam2=${9}
idparsopt_laa2=${10}
idparsopt_trans=${11}
idparsopt_trans2=${12}
idparsopt_K2=${13}
sim_model=${14}
ss_set=${15}

for_length=`wc -l DivABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
do
sbatch DivABC/bash/submit_run_ABC_param_set.sh ${param_space_name} \
                                                   ${param_set} \
                                                   ${idparsopt_lac} \
                                                   ${idparsopt_mu} \
                                                   ${idparsopt_gam} \
                                                   ${idparsopt_laa} \
                                                   ${idparsopt_K1} \
                                                   ${idparsopt_lac2} \
                                                   ${idparsopt_mu2} \
                                                   ${idparsopt_gam2} \
                                                   ${idparsopt_laa2} \
                                                   ${idparsopt_trans} \
                                                   ${idparsopt_trans2} \
                                                   ${idparsopt_K2} \
                                                   ${sim_model} \
                                                   ${ss_set}
done
