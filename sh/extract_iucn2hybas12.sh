#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=20:00:00
#SBATCH --output=sh/extract_iucn2hybas12.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

Rscript R/preprocess/extract_iucn2hybas12.R
