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

# future dams from Zarfl
dams_fut <- read.csv(file_frhed_dams)
# convert to spatial points
sdams_fut <- st_as_sf(dams_fut,coords = c('Lon_2016','Lat_2016'),crs=4326)

st_write(sdams_fut,'proc/dams_future.gpkg')

sdams_ffr <- st_read(file_ffr_dams)
#-------------------------------------------------------------------------
#>> Hydrobasins data

# read hydrobasins data
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))

# intersect dams and hydrobasins
sdams_cur_hb <- st_intersection(hb_data,sdams_cur)
sdams_fut_hb <- st_intersection(hb_data,sdams_fut)
sdams_ffr_hb <- st_intersection(hb_data,sdams_ffr)


# save only the data frame with the intersected metadata
saveRDS(sdams_cur_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_current_hydrobasins.rds')
saveRDS(sdams_fut_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_future_hydrobasins.rds')
saveRDS(sdams_ffr_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_currentFFR_hydrobasins.rds')

#>> count

# > nrow(sdams_cur) #original no. dams
# [1] 39933
# > nrow(sdams_cur_hb) #no. dams referenced on hybas12
# [1] 39912
# > length(unique(sdams_cur_hb$HYBAS_ID)) #no. of dammed hybasins
# [1] 22245

# > nrow(sdams_fut)
# [1] 3682
# > nrow(sdams_fut_hb)
# [1] 3681
# > length(unique(sdams_fut_hb$HYBAS_ID))
# [1] 2919


# # visual check
# hb_sel <- hb_data[hb_data$HYBAS_ID %in% sdams_cur_hb$HYBAS_ID,]
# write_sf(hb_sel,'visual_check/hb_sel_check.gpkg')
# write_sf(sdams_cur_hb,'visual_check/dams_cur_hb_check.gpkg')



