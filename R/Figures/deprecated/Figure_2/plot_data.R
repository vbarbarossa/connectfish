source('R/MASTER.R')

crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

hb_data <- readRDS('proc/hybas12poly_global_w_area_main.rds')

if(file.exists('proc/CI_stats_HYBAS_ID.rds')){
  tab <- readRDS('proc/CI_stats_HYBAS_ID.rds')
}else{
  # load files produced by Figure_2/tabulate_per_HYBAS_ID
  tab <- foreach(i = 1:9,.combine = 'rbind') %do% readRDS(paste0('proc/Figure_2_intermediate_steps/CI_stats_HYBAS_ID_',i,'.rds'))
  
  saveRDS(tab,'proc/CI_stats_HYBAS_ID.rds')
}

# merge polygon data with tab
hybas <- merge(hb_data,tab,by = 'HYBAS_ID')

hybas$type <- factor(hybas$type,levels=c('Present','Future','DELTA'))
levels(hybas$type) <- c('Present','Future','Difference')

hybas$cat <- factor(hybas$cat,levels=c('Diadromous','Potamodromous'))
levels(hybas$cat) <- c('Diadromous','Non-diadromous')

hybas_sub <- st_crop(hybas,xmin = -180,xmax = 180,ymin = -90,ymax = 90)

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
  geom_sf(data = hybas_sub, aes(fill = mean), lwd = NA) +
  scale_fill_viridis_c(breaks = seq(0,100,10),
                       labels = seq(0,100,10),
                       limits = c(0,100),
                       option = 'C',na.value = "grey90") +
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

ggsave('figs/Figure_2_CI_HYBAS_ID_mean.jpg',p,
       width = 300,height = 260,units = 'mm',dpi = 600)


p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = hybas_sub, aes(fill = median), lwd = NA) +
  scale_fill_viridis_c(breaks = seq(0,100,10),
                       labels = seq(0,100,10),
                       limits = c(0,100),
                       option = 'C',na.value = "grey90") +
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

ggsave('figs/Figure_2_CI_HYBAS_ID_median.jpg',p,
       width = 300,height = 260,units = 'mm',dpi = 600)

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = hybas_sub, aes(fill = count), alpha=1, lwd = NA) +
  scale_fill_viridis_c(trans = 'log10',na.value = "grey90") +
  coord_sf(crs = crs_custom) +
  facet_wrap('cat',ncol = 1) +
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

ggsave('figs/Figure_1_initial_sr_HYBAS_ID_count.jpg',p,
       width = 220,height = 260,units = 'mm',dpi = 600)

