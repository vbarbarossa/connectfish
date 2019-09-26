source('R/MASTER.R')

library(sf);

# DAMS LOCATION ############################################################################################
grand <- st_read('data/GRanD_Version_1_3/GRanD_dams_v1_3.shp') #7,320
# GOOD2 unsnapped
good2 <- st_read('data/GOOD2_unsnapped/GOOD2_unsnapped.shp') #32,613
dams_cur <- rbind(
  cbind(data.frame(ID = grand$GRAND_ID,database = 'GRanD'),st_coordinates(grand)),
  cbind(data.frame(ID = good2$DAM_ID,database = 'GOOD2'),st_coordinates(good2))
)
# compare to the actual georeferenced data used in the study
dams_cur <- dams_cur[dams_cur$ID %in% readRDS('data/dams_current_hydrobasins.rds')$ID,]
# convert to sf spatial points
sdams_cur <- st_as_sf(dams_cur,coords = c('X','Y'),crs=4326)

# future dams from Zarfl
dams_fut <- read.csv('data/17_0116_future_dams_update_final_v2.csv')
dams_fut <- dams_fut[dams_fut$DAM_ID %in% readRDS('data/dams_future_hydrobasins.rds')$DAM_ID,]

# convert to spatial points
sdams_fut <- st_as_sf(dams_fut,coords = c('Lon_2016','Lat_2016'),crs=4326)

# put datasets together
# ID database type
dfut <- sdams_fut[,c('DAM_ID')]
colnames(dfut)[1] <- 'ID'
dfut$database <- 'FHReD'
dfut$type <- 'Future'
sdams_cur$type <- 'Present'

dd <- rbind(sdams_cur,dfut)
dd$type <- factor(dd$type,levels = c('Present','Future'))

library(ggplot2)
crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- rnaturalearth::ne_countries(returnclass = "sf")[,1]
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")

# dd <- dd[c(1:100,35000:35200,40000:40100),]

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = dd,aes(fill = type),size = .5,shape = 21,alpha = 0.5) +
  scale_fill_manual(values = c('white','red')) +
  coord_sf(crs = crs_custom) +
  facet_wrap('type',ncol=1) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1.5, size = 16),
        legend.title = element_blank()
  )

ggsave('figs/Figure_SI_dams_distribution.jpg',p,
       width = 200,height = 220,units = 'mm',dpi = 600)
