#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=2:00:00
#SBATCH --output=sh/maps_watershed_focus.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

module load 2019
module load R/3.5.1-foss-2018b

Rscript R/Figures/watershed_focus.R
