#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=20:00:00
#SBATCH --output=sh/calculate_CI_min10k.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

Rscript R/calculate_CI_min10k.R
