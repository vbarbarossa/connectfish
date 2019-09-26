source('R/functions.R')

### list of directories ### <<<<<<<<<--------- TO MODIFY ACCORDINGLY

dir_figs <- dir_('figs/')

dir_tabs <- dir_('tabs/')

#processed data
dir_proc <- dir_('proc/')

# packages needed
library(sf); library(foreach); library(rfishbase); library(data.table); library(ggplot2); library(viridis)
# not fully loaded: foreign, rnaturalearth,

# location of hydrobasins shapefiles
# data is freely accessible @ https://hydrosheds.org/downloads
# the entire dataset is needed (divided in custom continents 'af','ar','as','au','eu','gr','na','sa','si')
dir_hybas12 <- '/vol/milkunarc/vbarbarossa/data/HydroBASINS/global_lev12'

# IUCN data
# data is freely accessible @ https://www.iucnredlist.org/resources/spatial-data-download
dir_iucn_fish <- '/vol/milkunarc/vbarbarossa/data/IUCN/FW_FISH_20181113'
file_iucn_fish1 <- '/vol/milkunarc/vbarbarossa/data/IUCN/FW_FISH_20181113/FW_FISH_PART_1.shp'
file_iucn_fish2 <- '/vol/milkunarc/vbarbarossa/data/IUCN/FW_FISH_20181113/FW_FISH_PART_2.shp'
file_iucn_habitat_type <- 'data/iucn_habitat_type.csv' # from fishsuit

# dams data
# data is freely accessible @
file_grand_dams <- 'data/GRanD_Version_1_3/GRanD_dams_v1_3.shp'
file_good2_dams <- 'data/GOOD2_unsnapped/GOOD2_unsnapped.shp'
file_frhed_dams <- 'data/17_0116_future_dams_update_final_v2.csv'




