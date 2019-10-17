source('R/MASTER.R')

crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# HB data
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))

# HB unit
hb_unit <- hb_data %>%
  select(HYBAS_ID) %>%
  # table with CI values per HB unit
  inner_join(.,readRDS('proc/CI_HB_FFR.rds'),by = 'HYBAS_ID') #%>%
# st_crop(.,xmin = -180,xmax = 180,ymin = -90,ymax = 90)

# BAS unit
bas_unit <- hb_data %>%
  group_by(MAIN_BAS) %>%
  summarize() %>%
  inner_join(.,readRDS('proc/CI_BAS_FFR.rds'),by = 'MAIN_BAS') #%>%
# st_crop(.,xmin = -180,xmax = 180,ymin = -90,ymax = 90)

# base layers
world <- rnaturalearth::ne_countries(returnclass = "sf")[,1]
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")

# CI per BAS unit----------------------------------------------------------------------
print('writing BAS maps..')


p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = 0) +
  geom_sf(data = bas_unit, aes(fill = value), lwd = 0) +
  scale_fill_viridis_c(breaks = seq(0,100,10),
                       labels = seq(0,100,10),
                       limits = c(0,100),
                       option = 'C',na.value = "grey90") +
  coord_sf(crs = crs_custom) +
  facet_grid(CI~cat) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(8,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 16),
        legend.title = element_blank()
  )

ggsave('figs/map_CI_BAS_mean_FFR.jpg',p,
       width = 300,height = 260,units = 'mm',dpi = 600,type = 'cairo')


# CI per HB unit----------------------------------------------------------------------
print('writing HB maps..')


p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = 0) +
  geom_sf(data = hb_unit, aes(fill = value), lwd = 0) +
  scale_fill_viridis_c(breaks = seq(0,100,10),
                       labels = seq(0,100,10),
                       limits = c(0,100),
                       option = 'C',na.value = "grey90") +
  coord_sf(crs = crs_custom) +
  facet_grid(CI~cat) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(8,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 16),
        legend.title = element_blank()
  )

ggsave('figs/map_CI_HB_mean_FFR.jpg',p,
       width = 300,height = 260,units = 'mm',dpi = 600,type = 'cairo')


# SR per HB unit----------------------------------------------------------------------
print('writing SR maps..')


sr_hb <- hb_unit %>%
  as_tibble() %>%
  select(HYBAS_ID,sr,cat) %>%
  distinct() %>%
  inner_join(hb_data,.,by = 'HYBAS_ID')


p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = 0) +
  geom_sf(data = sr_hb, aes(fill = sr), alpha=1, lwd = 0) +
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
        strip.text = element_text(angle = 0, vjust = -1, size = 16),
        legend.title = element_blank()
  )

ggsave('figs/map_SR_HB_FFR.jpg',p,
       width = 220,height = 260,units = 'mm',dpi = 600, type = 'cairo')
