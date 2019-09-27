# connectfish

## Settings
- **MASTER.R** this is used to setup files and directories connections and other model parameters

## Preprocessing
1. **extract_iucn2hybas12.R** reference the IUCN ranges to the sub-basin units of HydroBASINS - level 12 of aggregation
2. **extract_customRanges2hybas12.R** reference additional ranges of species not included in the IUCN database to the sub-basin units of HydroBASINS - level 12 of aggregation
3. **reference_tedescoBasins2hybas12.R** (optional) reference the basins shapefile used by Tedesco et al. 2017 (https://www.nature.com/articles/sdata2017141)
4. **georeference_dams.R**

## Main
- **calcuate_ci.R** calculates species-specific Connectivity Index (CI) for diadromous and non-diadromous species
- **calculate_ci_min10k.R** calculate_ci for basins smaller than 10k km2
- **functions.R** set of generic functions

## Figures
