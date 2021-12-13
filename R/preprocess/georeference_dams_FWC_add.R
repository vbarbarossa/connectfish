source('R/MASTER.R')

#-------------------------------------------------------------------------
#>> Dams data

input_FWC_data <- 'data/FWC_Dams_Remain_additional_HE_SP_v2.xlsx'

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

st_write(sdams_cur,'proc/dams_current.gpkg', delete_dsn = T)

#-------------------------------------------------------------------------
#>> Hydrobasins data
# dir_hybas12 <- '~/surfdrive/data/HydroBASINS/global_lev12'
# read hydrobasins data
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp')) %>% st_buffer(0)

# intersect dams and hydrobasins
sdams_cur_hb <- st_intersection(hb_data,sdams_cur)
saveRDS(sdams_cur_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_current_hydrobasins.rds')


# future dams
# names_fut <- gsub('.csv','',list.files('~/data/DAMS/IWC_project/IWC2_Remain_HE_SP/'))
names_fut <- readxl::excel_sheets(input_FWC_data)

# check RoR dams
dror <- data.frame(scen = rep(NA,length(names_fut)), no_RoR = rep(NA,length(names_fut)))
for(i in 1:length(names_fut)){
  n <- names_fut[i]
  dams_fut <- readxl::read_excel(input_FWC_data,sheet = n)[-1,] # first line are all zeroes
  
  # convert to spatial points
  sdams_fut <- st_as_sf(dams_fut,coords = c('lon','lat'),crs=4326)
  sdams_fut$ror <- 0
  sdams_fut$ror[sdams_fut$SysID_1_DiversionalCanalPower_2_RiverPower_ == 2] <- 1
  
  # print(n)
  # print(table(sdams_fut$ror))
  dror$scen[i] <- n
  dror$no_RoR[i] <- sum(as.numeric(sdams_fut$ror))
}


for(n in names_fut){
  
  dams_fut <- readxl::read_excel(input_FWC_data,sheet = n)[-1,] # first line are all zeroes
  
  # convert to spatial points
  sdams_fut <- st_as_sf(dams_fut,coords = c('lon','lat'),crs=4326)
  sdams_fut$ror <- 0
  sdams_fut$ror[sdams_fut$SysID_1_DiversionalCanalPower_2_RiverPower_ == 2] <- 1 # <<<< mistake, should equal 1, correct in modelling
  # write for 2050 and 2100 time horizons
  sdams_fut_2050 <- sdams_fut %>% filter(Year <= 2050)
  st_write(sdams_fut_2050,paste0('proc/dams_future_',n,'_2050.gpkg'),delete_dsn = T)
  st_write(sdams_fut,paste0('proc/dams_future_',n,'.gpkg'),delete_dsn = T)
  
  sdams_fut_hb <- st_intersection(hb_data,sdams_fut)
  sdams_fut_hb_2050 <- st_intersection(hb_data,sdams_fut_2050)
  saveRDS(sdams_fut_hb_2050 %>% as_tibble() %>% select(HYBAS_ID,ror) %>% distinct(),paste0('proc/dams_future_hydrobasins',n,'_2050.rds'))
  saveRDS(sdams_fut_hb %>% as_tibble() %>% select(HYBAS_ID,ror) %>% distinct(),paste0('proc/dams_future_hydrobasins',n,'.rds'))
  
}




