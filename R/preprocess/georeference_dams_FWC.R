source('R/MASTER.R')

#-------------------------------------------------------------------------
#>> Dams data

#GRanD v1.3
grand <- st_read(file_grand_dams) #7,320
# GOOD2 unsnapped
good2 <- st_read(file_good2_dams) #32,613

dams_cur <- rbind(
  cbind(data.frame(ID = grand$GRAND_ID,database = 'GRanD'),st_coordinates(grand)),
  cbind(data.frame(ID = good2$DAM_ID,database = 'GOOD2'),st_coordinates(good2))
)

# # current dams from GranD
# dams_cur <- foreign::read.dbf('data/GRanD_Version_1_3/GRanD_dams_v1_3.dbf')

# convert to sf spatial points
sdams_cur <- st_as_sf(dams_cur,coords = c('X','Y'),crs=4326)

st_write(sdams_cur,'proc/dams_current.gpkg')

#-------------------------------------------------------------------------
#>> Hydrobasins data
dir_hybas12 <- '~/surfdrive/data/HydroBASINS/global_lev12'
# read hydrobasins data
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp')) %>% st_buffer(0)

# intersect dams and hydrobasins
sdams_cur_hb <- st_intersection(hb_data,sdams_cur)
saveRDS(sdams_cur_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_current_hydrobasins.rds')


# future dams
names_fut <- gsub('.csv','',list.files('data/IWC2_Remain_HE_SP/'))

for(n in names_fut){
  dams_fut <- read.csv(paste0('data/IWC2_Remain_HE_SP/',n,'.csv'))
  # convert to spatial points
  sdams_fut <- st_as_sf(dams_fut,coords = c('lon','lat'),crs=4326)
  
  st_write(sdams_fut,paste0('proc/dams_future_',n,'.gpkg'))
  
  sdams_fut_hb <- st_intersection(hb_data,sdams_fut)
  saveRDS(sdams_fut_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),paste0('proc/dams_future_hydrobasins',n,'.rds'))
  
}




