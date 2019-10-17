source('R/MASTER.R')

library(sf); library(data.table); library(ggplot2); library(viridis); library(foreach)

crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

global_tab <- rbind(readRDS('proc/CI_tab_global.rds'),
                    readRDS('proc/CI_tab_global_min10k.rds'))
global_tab <- global_tab[global_tab$alpha == 0.55,]# alpha exponent used for transformation from area to length

if(file.exists('proc/CI_stats_MAIN_BAS.rds')){
  ws_tab <- readRDS('proc/CI_stats_MAIN_BAS.rds')
}else{
  # compute stats
  ws_tab <- foreach(i = levels(global_tab$category),.combine = 'rbind') %do% {
    
    cat = 'Potamodromous'
    if(i == 'diadromous') cat = 'Diadromous'
    
    dat <- droplevels(global_tab[global_tab$category == i,])
    
    return(
      do.call('rbind',parallel::mclapply(
        split(dat,dat$MAIN_BAS),function(x){
          x$diff <- x$connectivity.cur - x$connectivity.fut
          data.frame(
            MAIN_BAS = x$MAIN_BAS[1],
            mean = c(mean(x$connectivity.cur),mean(x$connectivity.fut),mean(x$diff)),
            median = c(median(x$connectivity.cur,na.rm=T),median(x$connectivity.fut,na.rm=T),median(x$diff,na.rm=T)),
            type = c('Present','Future','DELTA'),
            cat = cat,
            no.sp = nrow(x),
            dams.no = c(unique(x$dams.cur.no),(unique(x$dams.cur.no) + unique(x$dams.fut.no)),unique(x$dams.fut.no))
          )
        },mc.cores = 10
      ))
    )
  }
  saveRDS(ws_tab,'proc/CI_stats_MAIN_BAS.rds')
}

#TMP <<<<<<<< need to check these two values that are negative
ws_tab$mean[ws_tab$mean < 0] <- 0

# merge with the sf polygons
watersheds <- readRDS('proc/watersheds_hybas12.rds')
watersheds <- merge(watersheds,ws_tab,by='MAIN_BAS')
watersheds_sub <- watersheds
watersheds_sub$type <- factor(watersheds_sub$type,levels=c('Present','Future','DELTA'))
levels(watersheds_sub$type) <- c('Present','Future','Difference')

watersheds_sub$cat <- factor(watersheds_sub$cat,levels=c('Diadromous','Potamodromous'))
levels(watersheds_sub$cat) <- c('Diadromous','Non-diadromous')


watersheds_sub <- st_crop(watersheds_sub,xmin = -180,xmax = 180,ymin = -90,ymax = 90)

# world <- readRDS('proc/world_boundaries.rds')
world <- rnaturalearth::ne_countries(returnclass = "sf")[,1]
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = watersheds_sub, aes(fill = mean), lwd = NA) +
  scale_fill_viridis_c(breaks = seq(0,100,10),
                       labels = seq(0,100,10),
                       limits = c(0,100),
                       option = 'C') +
  coord_sf(crs = crs_custom) +
  facet_grid(type~cat) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(8,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1.5, size = 16),
        legend.title = element_blank()
  )

ggsave('figs/Figure_2_CI_MAIN_BAS_mean.jpg',p,
       width = 300,height = 260,units = 'mm',dpi = 600)

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = watersheds_sub, aes(fill = median), lwd = NA) +
  scale_fill_viridis_c(breaks = seq(0,100,10),
                       labels = seq(0,100,10),
                       limits = c(0,100),
                       option = 'C') +
  coord_sf(crs = crs_custom) +
  facet_grid(type~cat) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(8,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1.5, size = 16),
        legend.title = element_blank()
  )

ggsave('figs/Figure_2_CI_MAIN_BAS_median.jpg',p,
       width = 300,height = 260,units = 'mm',dpi = 600)

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = watersheds_sub, aes(fill = dams.no), lwd = NA) +
  scale_fill_viridis_c(option = 'E',direction = -1) +
  coord_sf(crs = crs_custom) +
  facet_wrap('type',ncol = 1) +
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

ggsave('figs/Figure_SI_NoDams_MAIN_BAS.jpg',p,
       width = 220,height = 360,units = 'mm',dpi = 600)
