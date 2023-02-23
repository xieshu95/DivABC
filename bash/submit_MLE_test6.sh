#!/bin/bash
#SBATCH --time=3-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=test
#SBATCH --output=logs/MLE_test-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

module load R
Rscript TraisieABC/scripts/secsse/test_secsse_MLE6.R