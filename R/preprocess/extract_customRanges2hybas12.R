#Valerio Barbarossa, 26 Sep 2019
# script that references the custom ranges to the hydobasins units level 12

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

# load habitat data
# habitat <- read.csv(file_custom_ranges_habitat_type)

# determine species to exclude because already in iucn database------------------------
# load IUCN data
iucn <- rbind(read_sf(file_iucn_fish1),read_sf(file_iucn_fish2))
# load synonyms table
synonyms_table <- vroom(list.files(dir_synonyms_table,full.names = T),delim=',') %>%
  filter(name_iucn %in% iucn$binomial)
# exclude names in iucn data and names in fishbase that are synonyms for iucn
names_to_exclude <- unique(c(iucn$binomial,synonyms_table$name_src))


# loop through multiple thresholds
for(th in c(1,2,10,20,30,50,100)){
  min_no_occ <- th
  
  # load species shapefile and filter
  sp <- read_sf(file_custom_ranges) %>%
    filter(no_occ >= min_no_occ) %>% #filter out based on no_occ threshold
    # filter(!name %in% unique(as.character(habitat$name[habitat$OnlyLake == -1]))) %>% # filter out exclusively lentic species <<< should be done later on to have a full comparison with Tedesco data
    filter(!name %in% names_to_exclude) # filter out species already covered in the IUCN dataset
  
  # reference to hydrobasins level 12
  lst <- st_contains(sp,points,sparse = T)
  # lst is a sparse matrix where each entry is a row of sp and contains a list of hybas12 points falling within that species polygos
  
  # make database where each entry is a hybas ID and 
  # loop through the species
  # for each species, create a table with hybasID and species id_no
  
  tab <- lapply(seq_along(lst),function(i){
    hb <- points$HYBAS_ID[lst[[i]]]
    if(length(hb) > 0){
      return(
        data.frame(HYBAS_ID = hb,
                   binomial = sp$name[i])
      )
    }
  }
  ) %>% do.call('rbind',.) %>% distinct()
  
  write.csv(tab,paste0('proc/hybas12_fish_custom_ranges_occth',min_no_occ,'.csv'),row.names = F)
  
  cat('\n\nMinimum no. occurrences per species: ',min_no_occ,
      '\nTotal no. of extra species referenced on hybas12: ',length(unique(tab$binomial)))
  
}


