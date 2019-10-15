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

# add NID data to G&G
NID <- foreach(f = list.files(dir_NID_dams,full.names = T),.combine = 'rbind') %do% openxlsx::read.xlsx(f) %>%
  as_tibble() %>%
  filter(!is.na(LONGITUDE))

# purposes to include
purp <- strsplit(unique(NID$PURPOSES),'') %>% do.call('c',.) %>% unique(.) %>% .[!. %in% c('P', 'D', 'T', 'O') & !is.na(.)]
# names already in GRanD
names_grand <- toupper(c(as.character(grand$DAM_NAME),as.character(grand$ALT_NAME))) %>% .[!is.na(.)]

NID <- NID %>%
  # filter records based on purp
  filter(lapply(strsplit(PURPOSES,''),function(x) sum(x %in% purp) > 0 ) %>% do.call('c',.)) %>%
  # filter out dams already in GRanD (based on matching names)
  filter(!DAM_NAME %in% names_grand) %>%
  # create column with height in meters (from feet)
  mutate(height_m = NID_HEIGHT*0.3048) %>%
  # create column with storage in cubic meters (from acres-feet)
  mutate(storage_m3 = NID_STORAGE*1233.48) %>%
  # convert to sf
  st_as_sf(.,coords = c('LONGITUDE','LATITUDE'),crs=4326)

# according to ICOLD:
# Definition of a Large Dam
# A dam with a height of 15 metres or greater from lowest foundation to crest 
# or a dam between 5 metres and 15 metres impounding more than 3 million cubic metres.
NID_large <- NID %>%
  filter(height_m >= 15 | storage_m3 > 3*10**6)

NID_large2 <- NID %>%
  filter(height_m >= 15)

# st_write(NID,'proc/dams_NID.gpkg')

# merge G&G with NID
# all
sdams_cur$ID <- as.numeric(sdams_cur$ID)
GGNID <- NID %>%
  mutate(database = 'NID') %>%
  select(ID = RECORDID,database,geometry) %>%
  rbind(.,sdams_cur)

GGNID_large <- NID_large %>%
  mutate(database = 'NID') %>%
  select(ID = RECORDID,database,geometry) %>%
  rbind(.,sdams_cur)

GGNID_large2 <- NID_large2 %>%
  mutate(database = 'NID') %>%
  select(ID = RECORDID,database,geometry) %>%
  rbind(.,sdams_cur)

#-------------------------------------------------------------------------
#>> Hydrobasins data

# read hydrobasins data
hb_data <- foreach(i = c('na','ar'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))
# extract only US HB units
sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'United States of America',returnclass = 'sf'),54009),
                     st_transform(hb_data,54009),
                     sparse = T)
hb_data <- hb_data[sel[[1]],]
# pdf('check.pdf')
# plot(st_geometry(hb_data))
# dev.off()

# intersect dams and hydrobasins
sdams_hb <- st_intersection(hb_data,GGNID)

# intersect dams and hydrobasins
sdams_hb_large <- st_intersection(hb_data,GGNID_large)

sdams_hb_large2 <- st_intersection(hb_data,GGNID_large2)


# save only the data frame with the intersected metadata
saveRDS(sdams_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_NID_hydrobasins.rds')
saveRDS(sdams_hb_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_NID_hydrobasins_large.rds')
saveRDS(sdams_hb_large2 %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'proc/dams_NID_hydrobasins_large2.rds')

# 24616 HB units in NID
# 4582 HB units in current_dams # 5.4 times less than NID

# 1105 HB units in Current_dams but not in NID
# 427 HB units in GranD but not in NID
# 890 HB units in GOODD but not in NID