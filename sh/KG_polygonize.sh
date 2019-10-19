#!/bin/bash
#SBATCH -p normal
#SBATCH -N 1
#SBATCH --time=20:00:00
#SBATCH --output=sh/KG_polygonize.out
#SBATCH --mail-type=END
#SBATCH --mail-user=vbarbarossa@science.ru.nl

module load 2019
module load R/3.5.1-foss-2018b

gdal_polygonize.py data/Beck_KG_V1_present_0p0083.tif proc/Beck_KG_V1_present_0p0083.gpkg
