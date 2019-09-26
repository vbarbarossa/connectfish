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

# load species shapefile
sp <- rbind(read_sf(file_iucn_fish1),read_sf(file_iucn_fish2))

#filter out occurrence records not wanted here
sp <- sp[sp$presence %in% c(1,2),]

# filter out lentic species
habitat <- read.csv(file_iucn_habitat_type)
sp <- sp[sp$binomial %in% habitat$binomial[habitat$lotic == 1],]

# reference to hydrobasins level 12
lst <- st_contains(sp,points,sparse = T)
# lst is a sparse matrix where each entry is a row of sp and contains a list of hybas12 points falling within that species polygos

# make database where each entry is a hybas ID and 
# loop through the species
# for each species, create a table with hybasID and species id_no

tab <- do.call('rbind',
               parallel::mclapply(seq_along(lst),function(i){
                 hybasIDs <- points$HYBAS_ID[lst[[i]]]
                 return(
                   cbind(data.frame(HYBAS_ID = hybasIDs),
                         do.call("rbind", replicate(length(hybasIDs),as.data.frame(sp[i,])[,1:(ncol(sp)-3)], simplify = FALSE)))
                 )
               },mc.cores = 20)
)

write.csv(tab,'HydroBASINS/hybas12_fish.csv',row.names = F)
