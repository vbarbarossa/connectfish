#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=1-20:00:00
#SBATCH --output=sh/calculate_CI.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

Rscript R/calculate_CI.R
