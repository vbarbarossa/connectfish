#Valerio Barbarossa, 26 Sep 2019
# comparison of total number of species per main basin
# between Tedesco et al. and this study ranges (IUCN+custom)

source('R/MASTER.R')

# read hybas12 on Tedesco
ted <- vroom('proc/hybas12_tedesco.csv',delim=',')

ted_occ <- ted %>%
  select(BasinName,no_species) %>%
  distinct()

# read hybas12 on IUCN
iucn <- vroom('proc/hybas12_fish.csv',delim = ',')

# read hybas12 on customRanges
cr <- vroom(paste0('proc/hybas12_fish_custom_ranges_occth',min_no_occ,'.csv'),delim = ',')

# merge iucn and custom ranges
ranges <- bind_rows(iucn,cr)

# merge with tedesco data HYBAS_ID,BasinName
ranges_bas <- inner_join(ranges,ted %>% select(HYBAS_ID,BasinName)) %>%
  select(BasinName,binomial) %>%
  distinct() %>%
  group_by(BasinName) %>%
  summarize(no_species_ranges = n()) # summarize no species per basin

# merge with no species of tedesco BasinName,no_species
tab <- inner_join(ranges_bas,ted_occ) %>%
  mutate(ratio = no_species_ranges/no_species*100) # percentage difference

sum(tab$ratio > 100)
sum(tab$ratio < 100)

tab_sf <- inner_join(read_sf(paste0(dir_ted_bas,'Basin042017_3119.shp')),tab) %>%
  select(BasinName,Country,Ecoregion,no_species_ranges,no_species,ratio,geometry)

tab_sf$ratio[tab_sf$ratio > 100] <- 100

# plot the ratio-------------------------------------------------------------------------------------
world <- rnaturalearth::ne_countries(returnclass = "sf")
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")
p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = 0) +
  geom_sf(data = tab_sf, aes(fill = ratio), alpha=1, lwd = 0) +
  scale_fill_viridis(na.value = 'grey90') +
  coord_sf(crs = crs_custom) +
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

ggsave(paste0('figs/Figure_SI_compare_total_basin_richness_with_tedesco_oth',min_no_occ,'.jpg'),p,
       width = 220,height = 130,units = 'mm',dpi = 600,type = 'cairo')


# plot also only IUCN coverage------------------------------------------------------------------------
ranges <- iucn

# merge with tedesco data HYBAS_ID,BasinName
ranges_bas <- inner_join(ranges,ted %>% select(HYBAS_ID,BasinName)) %>%
  select(BasinName,binomial) %>%
  distinct() %>%
  group_by(BasinName) %>%
  summarize(no_species_ranges = n()) # summarize no species per basin

# merge with no species of tedesco BasinName,no_species
tab <- inner_join(ranges_bas,ted_occ) %>%
  mutate(ratio = no_species_ranges/no_species*100) # percentage difference

sum(tab$ratio > 100)
sum(tab$ratio < 100)

tab_sf <- inner_join(read_sf(paste0(dir_ted_bas,'Basin042017_3119.shp')),tab) %>%
  select(BasinName,Country,Ecoregion,no_species_ranges,no_species,ratio,geometry)

tab_sf$ratio[tab_sf$ratio > 100] <- 100

# plot the ratio
world <- rnaturalearth::ne_countries(returnclass = "sf")
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")
p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = 0) +
  geom_sf(data = tab_sf, aes(fill = ratio), alpha=1, lwd = 0) +
  scale_fill_viridis(na.value = 'grey90') +
  coord_sf(crs = crs_custom) +
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

ggsave('figs/Figure_SI_compare_total_basin_richness_with_tedesco_IUCN_only.jpg',p,
       width = 220,height = 130,units = 'mm',dpi = 600,type = 'cairo')
