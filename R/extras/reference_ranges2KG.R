source('R/MASTER.R')

# reference KG to HB units-------------------------------------------------------------------------
library(raster)
# read KG poly
# kg <- read_sf('proc/KG_reclass.gpkg')
kg <- raster('data/Koeppen-Geiger-Classification-Reclassfied_5min_moderesampling.tif')

# read centroids of HB units
# determine centroids with sf
if(file.exists('proc/hybas12_points_nolakes.gpkg')){
  points <- read_sf('proc/hybas12_points_nolakes.gpkg')
}else{
  points <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- read_sf(paste0(dir_hybas12,'/hybas_',cont,'_lev12_v1c.shp'))
    return(st_centroid(poly))
  }
  write_sf(points,'proc/hybas12_points_nolakes.gpkg',driver='GPKG')
}

points$kg <- extract(kg,points)

# lst <- st_contains(kg,points,sparse = T)
# 
# kg_hybas12 <- lapply(seq_along(lst),function(i){
#   hb <- points$HYBAS_ID[lst[[i]]]
#   if(length(hb) > 0){
#     return(
#       data.frame(HYBAS_ID = hb,
#                  kg = kg$name[i]) #<<<<<<<<<<< need to check
#     )
#   }
# }
# ) %>% do.call('rbind',.) %>% distinct()

# save?

# reference KG to species--------------------------------------------------------------------------

# read species data referenced to HB
sp_data <- bind_rows(
  # read hybas12 on IUCN
  vroom('proc/hybas12_fish.csv',delim=','),
  # read hybas12 on customRanges
  vroom(paste0('proc/hybas12_fish_custom_ranges_occth0.csv'),delim=',')
) %>%
  # and join them with the kg_hybas12
  inner_join(.,points %>% as_tibble() %>% dplyr::select(HYBAS_ID,kg) %>% filter(!is.na(kg)),by="HYBAS_ID")

# per species
sp_kg <- sp_data %>%
  group_by(binomial) %>%
  summarize(
    KG = names(sort(table(kg),decreasing = T))[1]
  )


