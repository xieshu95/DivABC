#!/bin/bash
#SBATCH --time=2-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=Loglik
#SBATCH --output=logs/Loglik-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

module load R
Rscript DivABC/scripts/loglik_test/run_loglik_cluster.R