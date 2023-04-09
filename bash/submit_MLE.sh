#!/bin/bash
#SBATCH --time=3-23:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=MLE
#SBATCH --output=logs/MLE-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

module load R
Rscript TraisieABC/scripts/DAISIE_analysis/calc_MLE.R