#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=2-00:00:00
#SBATCH --output=sh/calculate_CI_cont_FWC_%a.out
#SBATCH --mail-type=END
#SBATCH --mail-user=v.barbarossa@cml.leidenuniv.nl

module load 2019
module load R/3.5.1-foss-2018b

Rscript R/calculate_CI_cont_FWC.R
