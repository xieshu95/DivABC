#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABCrf
#SBATCH --output=logs/start_ABCrf.log
#SBATCH --mem=1GB
#SBATCH --partition=regular


# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/DivABC@abcrf')"

for (( id = 1; id <= 25; id++ ))
do
sbatch DivABC/bash/submit_ref_table.sh ${id}
done
