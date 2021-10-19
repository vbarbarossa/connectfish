#!/bin/bash
#SBATCH -N 1
#SBATCH -n 24
#SBATCH -t 1-00:00:00
#SBATCH -p "cpu-medium"
#SBATCH --output=sh/calculate_CI_cont_FWC_%a.out
#SBATCH --mail-type=END
#SBATCH --mail-user=v.barbarossa@cml.leidenuniv.nl

module load Miniconda3/4.7.10 
source activate ~/envs/r4

Rscript R/calculate_CI_cont_FWC_2050.R
