#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=3-00:00:00
#SBATCH --output=sh/maps.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

Rscript R/Figures/maps.R
