source('R/MASTER_local.R')

names_fut <- c(readxl::excel_sheets('data/FWC_Dams_Remain_low_med_high_v2.xlsx'),
               readxl::excel_sheets('data/FWC_Dams_Remain_additional_HE_SP_v2.xlsx'))

names_fut <- c(names_fut,paste0(names_fut,'_2050'))

years <- c(rep(2100,length(names_fut)/2),rep(2050,length(names_fut)/2))
ambition <- rep(c(rep('low',3),rep('medium',3),rep('high',3),rep(c('base','half-earth','shared-planet'),3)),2)
RCP <- rep(c(rep(c('6.0','2.6','2.6_re'),3),rep(c('6.0','2.6','2.6_re'),each=3)),2)

dir_proc <- '~/surfdrive/tmp/connectfish_fwc_proc_20220207/'


area_threshold <- 2000

# base layers
crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# crs_custom <- st_crs(4326)
crop_main <- c(-180, 180, -60, 90)
cropping_poly <- st_bbox(raster::extent(crop_main), crs = st_crs(4326)) %>% st_as_sfc(.)

sf_use_s2(FALSE)
world <- read_sf('~/surfdrive/data/naturalearth/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')[,1] %>%
  st_crop(cropping_poly) %>%
  st_transform(crs_custom)

bb <- read_sf('~/surfdrive/data/naturalearth/ne_110m_graticules_all/ne_110m_wgs84_bounding_box.shp') %>%
  st_crop(cropping_poly) %>%
  st_transform(crs_custom)

graticules <- read_sf('~/surfdrive/data/naturalearth/ne_110m_graticules_all/ne_110m_graticules_30.shp') %>%
  st_crop(cropping_poly) %>%
  st_transform(crs_custom)




# DAMS distribution -----------------------------
t <- foreach(i = 1:length(names_fut),.combine = 'rbind') %do% {
  
  # spl <- strsplit(sc,'_')[[1]]
  # splyr <- spl[length(spl)] %>% as.numeric()
  scn <- names_fut[i]
  yr <- years[i]
  amb <- ambition[i]
  rcp <- RCP[i]
  # if(!is.na(spl[4]) & spl[4] != 2050) rcp <- '2.6'
  # if(!is.na(spl[4]) & !is.na(spl[5]) & spl[4] != 2050 & spl[5] != 2050) rcp <- '2.6_re'
  
  # if(!is.na(splyr)){
  #   if(splyr == 2050){
  #     yr <- 2050
  #     scn <- paste(spl[1:(length(spl)-1)],collapse = '_')
  #   }
  # }
  s <- read_sf(paste0(dir_proc,'dams_future_',scn,'.gpkg')) %>%
    select(geom,ror) %>%
    mutate(scenario = scn, year = yr, ambition = amb, rcp = rcp)
  
  nd <- s %>% as_tibble %>% group_by(ror) %>% summarise(no=n())
  no_dams_res <- nd$no[nd$ror == 1]
  no_dams_ror <- nd$no[nd$ror == 0]
  
  if(length(no_dams_res) == 0) no_dams_res <- 0
  if(length(no_dams_ror) == 0) no_dams_ror <- 0
  
  return(
    s %>%
      mutate(no_dams = nrow(s), no_dams_res = no_dams_res, no_dams_ror = no_dams_ror)
  )
}

t$scenario <- as.factor(t$scenario)
t$year <- as.factor(t$year)
t$ambition <- as.factor(t$ambition)
t$rcp <- as.factor(t$rcp)
levels(t$rcp) <- c('2.6','2.6(RE)','6.0')
# t$rcp <- factor(t$rcp, levels = c('6.0','2.6','2.6(RE)'))
# levels(t$ambition) <- paste0(c('High','Low','Medium'),' policy')
t$ambition <- factor(t$ambition,levels = c('base','low','medium','high','half-earth','shared-planet'))

library(RColorBrewer)
tann <- t %>% as_tibble() %>% select(-geom,-ror) %>% distinct()
t$ror <- as.factor(t$ror)
p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = t,aes(color = ror),size = .5,shape = 20,alpha = 0.5) +
  # annotate(data = tann, geom = 'Text',label = no_dams, size = 4, x = 0, y = 0) +
  geom_text(
    data    = tann,
    mapping = aes(x = -Inf, y = -Inf, label = no_dams_res)
    ,hjust   = -0.1,
    vjust   = -1.5,
    color = colorRampPalette(brewer.pal(9, "PiYG"))(3)[c(1)]
  ) +
  geom_text(
    data    = tann,
    mapping = aes(x = -Inf, y = -Inf, label = no_dams_ror)
    ,hjust   = -0.1,
    vjust   = -0.1,
    color = colorRampPalette(brewer.pal(9, "PiYG"))(3)[c(3)]
  ) +
  scale_color_manual(values = colorRampPalette(brewer.pal(9, "PiYG"))(3)[c(3,1)]
  ) +
  xlab('') + ylab('') +
  # scale_fill_manual(values = c('white','red')) +
  coord_sf(crs = crs_custom) +
  facet_grid(year + rcp ~ ambition) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_line(color=NA),
    axis.text = element_blank(),
    legend.position = 'none',
    strip.background = element_blank(),
    # strip.text = element_text(angle = 0, vjust = -1.5, size = 16),
    legend.title = element_blank()
  )
p

ggsave('figs/FWC_dams_distribution.jpg',p,
       width = 350,height = 200,units = 'mm',dpi = 600)

write_sf(t,'tabs/FWC/dams_location_scenarios.gpkg')

# overall BARPLOTS -------------------------------

t <- foreach(i = 1:length(names_fut),.combine = 'rbind') %do% {
  
  scn <- names_fut[i]
  yr <- years[i]
  amb <- ambition[i]
  rcp <- RCP[i]
  
  return(
    read.csv(paste0('tabs/FWC/species_ci/species_ci_summary_oth10_',scn,'.csv')) %>% 
      mutate(ci_current = ci_cur_meanW, ci_future = ci_fut_meanW,delta = ci_cur_meanW - ci_fut_meanW) %>%
      select(type,ci_current,ci_future,delta) %>%
      as_tibble() %>%
      mutate(type = forcats::fct_recode(type, 'Non diadromous' = 'potamodromous', 'Diadromous' = 'diadromous')) %>%
      mutate(type = factor(type, levels = c('Non diadromous','Diadromous'))) %>%
      mutate(scenario = scn, year = yr, ambition = amb, rcp = rcp)
  )
}
t$scenario <- as.factor(t$scenario)
t$year <- as.factor(t$year)
t$ambition <- as.factor(t$ambition)
t$rcp <- as.factor(t$rcp)
levels(t$rcp) <- c('2.6','2.6(RE)','6.0')
t$ambition <- factor(t$ambition,levels = c('base','low','medium','high','half-earth','shared-planet'))


library(RColorBrewer)
p <- ggplot(t,aes(y = delta, x = rcp, fill = ambition)) +
  geom_bar(stat='identity',position = 'dodge',width = 0.9) + #position = 'identity',
  scale_fill_manual(values = colorRampPalette(brewer.pal(6, "Dark2"))(6)
                    # ,labels = c(expression('1.5'^o*C),expression('2.0'^o*C),expression('3.2'^o*C),expression('4.5'^o*C))
  ) +
  # scale_y_continuous(breaks = seq(0,1,0.25),labels = seq(0,1,0.25)) +
  # guides(fill = guide_legend(title=NULL)) +
  ylab('Delta fragmentation (CI present - CI future) [%]') +
  xlab('') +
  coord_flip(expand = F) + #ylim = c(0,1.0001)
  facet_grid(type ~ year,scales = 'free_y',space = 'free_y',switch = 'y') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(),
        # legend.direction = 'horizontal',
        panel.grid.major.x = element_line(linetype = 'dashed',color='black'),
        # legend.position = c(0.94,0.29),
        # legend.background = element_blank(),
        strip.placement = 'outside',
        strip.background = element_blank(),
        panel.spacing = unit(1.2, "lines"))
p

ggsave('figs/FWC_barplot_delta.jpg',p,
       width = 180,height = 120,units = 'mm',dpi = 600)

p <- ggplot(t,aes(y = ci_future, x = rcp, fill = ambition)) +
  geom_bar(stat='identity',position = 'dodge',width = 0.9) + #position = 'identity',
  scale_fill_manual(values = colorRampPalette(brewer.pal(6, "Dark2"))(6)
                    # ,labels = c(expression('1.5'^o*C),expression('2.0'^o*C),expression('3.2'^o*C),expression('4.5'^o*C))
  ) +
  geom_hline(data = t %>% select(type,ci_current) %>% distinct, aes(yintercept = ci_current),
             color = 'red', linetype = 'dotted', size = 1) +
  ylab('CI future [%]') +
  xlab('') +
  coord_flip(expand = F) + #ylim = c(0,1.0001)
  facet_grid(type ~ year,scales = 'free_y',space = 'free_y',switch = 'y') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(),
        # legend.direction = 'horizontal',
        panel.grid.major.x = element_line(linetype = 'dashed',color='black'),
        # legend.position = c(0.94,0.29),
        # legend.background = element_blank(),
        strip.placement = 'outside',
        strip.background = element_blank(),
        panel.spacing = unit(1.2, "lines"))
p

ggsave('figs/FWC_barplot.jpg',p,
       width = 180,height = 120,units = 'mm',dpi = 600)

write.csv(t,'tabs/FWC/CI_species_average.csv',row.names = F)

# by MAIN BASIN ---------------------------------------------------------------------------------------

# HB units
if(file.exists('proc/hb_global_buffered.rds')){
  hb_data <- readRDS('proc/hb_global_buffered.rds')
}else{
  hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp')) %>%
    st_buffer(0)
  saveRDS(hb_data,'proc/hb_global_buffered.rds')
}

# BAS units
if(file.exists(paste0('proc/BAS_global_.rds'))){
  bas_unit <- readRDS(paste0('proc/BAS_global_.rds'))
}else{
  bas_unit <- hb_data %>%
    group_by(MAIN_BAS) %>%
    summarize(Area = sum(SUB_AREA)) %>%
    st_crop(.,xmin = -180,xmax = 180,ymin = -90,ymax = 90)
  
  saveRDS(bas_unit, paste0('proc/BAS_global_.rds'))
}

bas_unit <- bas_unit %>% filter(Area >= area_threshold)

t <- foreach(i = 1:length(names_fut),.combine = 'rbind') %do% {
  
  scn <- names_fut[i]
  yr <- years[i]
  amb <- ambition[i]
  rcp <- RCP[i]
  
  return(
    bas_unit %>%
      left_join(.,readRDS(paste0('proc/CI_BAS_',scn,'.rds')) %>% filter(CI == 'Difference'),by = 'MAIN_BAS') %>%
      mutate(scenario = scn, year = yr, ambition = amb, rcp = rcp)
  )
  
}
t$scenario <- as.factor(t$scenario)
t$year <- as.factor(t$year)
t$ambition <- as.factor(t$ambition)
t$rcp <- as.factor(t$rcp)
levels(t$rcp) <- c('2.6','2.6(RE)','6.0')
t$ambition <- factor(t$ambition,levels = c('base','low','medium','high','half-earth','shared-planet'))

t$value_adj <- t$value
t$value_adj[t$value > 10] <- 15

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = 0) +
  geom_sf(data = t %>% filter(year == 2050 & Area > 1000000), aes(fill = value_adj), lwd = 0) +
  scale_fill_viridis_c(
    breaks = seq(0,100,10),
    labels = seq(0,100,10),
    # limits = c(0,100),
    option = 'C',na.value = "grey90") +
  coord_sf(crs = crs_custom) +
  facet_grid(ambition ~ rcp) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(8,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1),
        legend.title = element_blank()
  )
# p
ggsave(paste0('figs/FWC_CI_BAS_mean_2050.jpg'),p,
       width = 300,height = 300,units = 'mm',dpi = 600)

