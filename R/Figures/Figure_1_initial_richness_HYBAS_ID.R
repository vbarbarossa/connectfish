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

### HYBAS_ID #################################################

# split by continents
sp_data$cont <- floor(sp_data$MAIN_BAS/1000000000)


# compute species richness per HYBAS_ID
if(file.exists('proc/SR_per_HYBAS_ID.rds')){
  tabulated <- readRDS('proc/SR_per_HYBAS_ID.rds')
}else{
  tabulated <- 
    do.call('rbind',
            parallel::mclapply(
              split(sp_data,sp_data$cont),
              function(y){
                do.call('rbind',
                        lapply(
                          split(y,y$HYBAS_ID),
                          function(x){
                            data.frame(
                              HYBAS_ID = rep(unique(as.character(x$HYBAS_ID)),3),
                              sr = c(length(unique(as.character(x$binomial))),
                                     length(unique(as.character(x$binomial[x$diad != 't']))),
                                     length(unique(as.character(x$binomial[x$diad == 't'])))),
                              category = c('Total','Potamodromous','Diadromous')
                            )
                          })
                )},mc.cores = 9))
  
  saveRDS(tabulated,'proc/SR_per_HYBAS_ID.rds')
  
}

hybas <- merge(hb_data[,"HYBAS_ID"],tabulated,by='HYBAS_ID')

hybas_sub <- hybas[hybas$category != 'Total',]
hybas_sub$category <- factor(hybas_sub$category)

hybas_sub$category <- factor(hybas_sub$category,levels=c('Diadromous','Potamodromous'))
levels(hybas_sub$category) <- c('Diadromous','Non-diadromous')

hybas_sub <- st_crop(hybas_sub,xmin = -180,xmax = 180,ymin = -90,ymax = 90)

world <- rnaturalearth::ne_countries(returnclass = "sf")
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")
p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = hybas_sub, aes(fill = sr), alpha=1, lwd = NA) +
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

ggsave('figs/Figure_1_initial_sr_HYBAS_ID.jpg',p,
       width = 220,height = 260,units = 'mm',dpi = 600)

