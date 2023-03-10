#!/bin/bash
#SBATCH --time=2-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=MLE_MCMC
#SBATCH --output=logs/MLE_MCMC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=gelifes

module load R
Rscript TraisieABC/scripts/loglik_test/MLE_init_with_MCMC.R