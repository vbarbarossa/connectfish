source('R/MASTER.R')

crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# hybas12 data
hb_data <- readRDS('proc/hybas12poly_global_w_area_main.rds')
# extract main_bas_area per MAIN_BAS
# hb <- as.data.table(hb_data)
# hb <- hb[,-15]
# main_bas_area <- do.call('rbind',lapply(split(hb,hb$MAIN_BAS),function(x) data.frame(MAIN_BAS = x$MAIN_BAS[1], MAIN_BAS_AREA = x$MAIN_BAS_AREA[1])))

# species data on hybas12
if(file.exists('proc/hybas12_fish_w_area.rds')){
  sp_data <- readRDS('proc/hybas12_fish_w_area.rds')
}else{
  sp_data <- readRDS('data/hybas12_fish.rds')
  sp_data <- merge(sp_data,as.data.frame(hb_data[,c("HYBAS_ID","MAIN_BAS","SUB_AREA","MAIN_BAS_AREA")])[,-5],by="HYBAS_ID")
  saveRDS(sp_data,'proc/hybas12_fish_w_area.rds')
}

### load Fishbase metadta
fishbase <- read.csv('proc/iucn_fishbase.csv')
fishbase <- fishbase[!duplicated(fishbase$iucn_name),]
levels(fishbase$AnaCat) <- c(NA,rep('Diad.',5),'Non.','Ocea.','Pota.')
# assign diadromous-non diadromous category
sp_data$diad <- 'f'
sp_data$diad[sp_data$binomial %in% fishbase$iucn_name[fishbase$AnaCat == 'Diad.']] <- 't'

### MAIN_BAS #################################################

# compute species richness per MAIN_BAS
if(file.exists('proc/SR_per_MAIN_BAS.rds')){
  tabulated2 <- readRDS('proc/SR_per_MAIN_BAS.rds')
}else{
  tabulated2 <- do.call('rbind',lapply( #takes a while (~15-20min)
    split(sp_data,sp_data$MAIN_BAS),function(x){
      data.frame(
        MAIN_BAS = rep(unique(as.character(x$MAIN_BAS)),3),
        sr = c(length(unique(as.character(x$binomial))),
               sp_pota = length(unique(as.character(x$binomial[x$diad != 't']))),
               sp_diad = length(unique(as.character(x$binomial[x$diad == 't'])))),
        category = c('Total','Potamodromous','Diadromous')
      )
    }
  ))
  saveRDS(tabulated2,'proc/SR_per_MAIN_BAS.rds')
  
}

watersheds <- readRDS('proc/watersheds_hybas12.rds')
watersheds <- merge(watersheds,tabulated2,by='MAIN_BAS')
watersheds_sub <- watersheds[watersheds$category != 'Total',]
watersheds_sub$category <- factor(watersheds_sub$category)

watersheds_sub$category <- factor(watersheds_sub$category,levels=c('Diadromous','Potamodromous'))
levels(watersheds_sub$category) <- c('Diadromous','Non-diadromous')

watersheds_sub <- st_crop(watersheds_sub,xmin = -180,xmax = 180,ymin = -90,ymax = 90)

# world <- readRDS('proc/world_boundaries.rds')
world <- rnaturalearth::ne_countries(returnclass = "sf") #<<<< TMP!!!!!
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = watersheds_sub, aes(fill = sr), lwd = NA) +
  scale_fill_viridis(trans = 'log10',na.value = 'grey90') +
  coord_sf(crs = crs_custom) +
  facet_wrap('category',ncol = 1) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(8,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1.5, size = 16),
        legend.title = element_blank()
  )

ggsave('figs/Figure_1_initial_sr_MAIN_BAS.jpg',p,
       width = 220,height = 260,units = 'mm',dpi = 600)

