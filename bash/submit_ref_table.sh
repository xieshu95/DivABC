#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=1GB
#SBATCH --partition=regular


# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/DivABC@abcrf')"


for (( id = 1; id <= 3; id++ ))
  do
Rscript DivABC/scripts/run_ABC_bisse_peregrine.R ${id}
done
