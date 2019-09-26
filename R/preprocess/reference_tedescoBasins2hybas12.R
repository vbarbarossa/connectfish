#Valerio Barbarossa, 26 Sep 2019
# script that references the hydrobasins level 12 units to Tedesco
source('R/MASTER.R')

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

# species count per basin
ted_occ <- read.csv(paste0(dir_ted_bas,'Occurrence_Table.csv'),sep=';') %>%
  as_tibble() %>%
  select(BasinName = X1.Basin.Name, species = X6.Fishbase.Valid.Species.Name) %>%
  distinct() %>%
  group_by(BasinName) %>%
  summarize(no_species = n())

# read Tedesco et al. basins shapefile
ted <- read_sf(paste0(dir_ted_bas,'Basin042017_3119.shp')) %>%
  left_join(.,ted_occ) %>%
  select(BasinName,Country,Ecoregion,Endorheic,no_species,geometry)

# intersect with Hybas12
lst <- st_contains(ted[1:100,],points,sparse = T)

tab <- lapply(seq_along(lst),function(i){
  hb <- points$HYBAS_ID[lst[[i]]]
  if(length(hb) > 0){
    
    return(
      data.frame(HYBAS_ID = hb,
                 BasinName = ted$BasinName[i],
                 Country = ted$Country[i],
                 Ecoregion = ted$Ecoregion[i],
                 Endorheic = ted$Endorheic[i],
                 no_species = ted$no_species[i]
      )
    )}
}
) %>% do.call('rbind',.) %>% distinct()

write.csv(tab,paste0('proc/hybas12_tedesco.csv'),row.names = F)




