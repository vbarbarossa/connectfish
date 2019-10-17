#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=20:00:00
#SBATCH --output=sh/extract_iucn2hybas12.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

module load 2019
module load R/3.5.1-foss-2018b

Rscript R/preprocess/extract_iucn2hybas12.R
