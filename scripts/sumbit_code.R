## code submit on cluster:
# module load R
# R
# git clone  https://github.com/xieshu95/TraisieABC.git
# sbatch TraisieABC/bash/submit_run_ABC_peregrine.sh DAISIE_ABC TRUE TRUE TRUE TRUE
# sbatch TraisieABC/bash/submit_run_MCMC_peregrine.sh DAISIE_MCMC TRUE TRUE TRUE TRUE