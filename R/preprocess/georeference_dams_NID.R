source('R/MASTER.R')

#-------------------------------------------------------------------------
#>> Dams data

#NID
NID <- foreach(f = list.files(dir_NID_dams,full.names = T),.combine = 'rbind') %do% openxlsx::read.xlsx(f) %>%
  as_tibble() %>%
  filter(!is.na(LONGITUDE)) %>%
  st_as_sf(.,coords = c('LONGITUDE','LATITUDE'),crs=4326)

#-------------------------------------------------------------------------
#>> Hydrobasins data

# read hydrobasins data
hb_data <- foreach(i = c('na'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))

# intersect dams and hydrobasins
sdams_hb <- st_intersection(hb_data,NID)

# save only the data frame with the intersected metadata
saveRDS(as.data.frame(sdams_hb)[,1:(ncol(sdams_hb)-1)],'proc/dams_NID_hydrobasins.rds')
