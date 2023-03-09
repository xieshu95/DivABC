#!/bin/bash
#SBATCH --time=1-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=MLE5
#SBATCH --output=logs/MLE-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=gelifes

module load R
Rscript TraisieABC/scripts/secsse/calc_MLE_cluster5.R
