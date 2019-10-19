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

# names already in GRanD
names_grand <- c(as.character(grand$DAM_NAME),as.character(grand$ALT_NAME)) %>% .[!is.na(.)]

# large dams for brazil
BRA_l <- read_sf('data/brazil_hydrodams/Usinas_Hidrel├йtricas_UHE.shp') %>%
  filter(!NOME %in% names_grand) %>% #64
  mutate(database = 'BRA_l') %>%
  mutate(ID = 1:nrow(.)) %>%
  select(ID,database) %>%
  st_transform(st_crs(sdams_cur))

BRA_s <- read_sf('data/brazil_hydrodams/Pequenas_Centrais_Hidrel├йtricas_PCH.shp') %>%
  filter(!NOME %in% names_grand) %>% #44
  mutate(database = 'BRA_s') %>%
  mutate(ID = 1:nrow(.)) %>%
  select(ID,database) %>%
  st_transform(st_crs(sdams_cur))

# merge G&G with BRA
# all
sdams_cur$ID <- as.numeric(sdams_cur$ID)
all <- BRA_l %>%
  rbind(.,BRA_s) %>%
  rbind(.,sdams_cur)

large <- BRA_l %>%
  rbind(.,sdams_cur)

#-------------------------------------------------------------------------
#>> Hydrobasins data

# read hydrobasins data
hb_data <- foreach(i = c('sa'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))
# extract only US HB units
sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'Brazil',returnclass = 'sf'),54009),
                     st_transform(hb_data,54009),
                     sparse = T)
hb_data <- hb_data[sel[[1]],]
# pdf('check.pdf')
# plot(st_geometry(hb_data))
# dev.off()

# intersect dams and hydrobasins
sdams_hb <- st_intersection(hb_data,all)

sdams_hb_large <- st_intersection(hb_data,large)

sdams_hb_GGonly <- st_intersection(hb_data,sdams_cur)

# save only the data frame with the intersected metadata
saveRDS(sdams_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_BRA_hydrobasins.rds')
saveRDS(sdams_hb_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_BRA_hydrobasins_large.rds')
saveRDS(sdams_hb_GGonly %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_BRA_hydrobasins_GGonly.rds')

# 24616 HB units in NID
# 4582 HB units in current_dams # 5.4 times less than NID

# 1105 HB units in Current_dams but not in NID
# 427 HB units in GranD but not in NID
# 890 HB units in GOODD but not in NID