#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=1-00:00:00
#SBATCH --output=sh/calculate_CI_MEK_%a.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

module load 2019
module load R/3.5.1-foss-2018b

Rscript R/calculate_CI_MEK.R
