source('R/MASTER_local.R')

names_fut <- readxl::excel_sheets('data/FWC_Dams_Remain_low_med_high_v1.xlsx')
names_fut <- c(names_fut,paste0(names_fut,'_2050'))

dir_proc <- '~/surfdrive/tmp/fwc_proc/'

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
t <- foreach(sc = names_fut,.combine = 'rbind') %do% {
  
  spl <- strsplit(sc,'_')[[1]]
  splyr <- spl[length(spl)] %>% as.numeric()
  yr <- 2100
  scn <- sc
  amb <- spl[2]
  rcp <- 'base'
  if(!is.na(spl[4]) & spl[4] != 2050) rcp <- '2.6'
  if(!is.na(spl[4]) & !is.na(spl[5]) & spl[4] != 2050 & spl[5] != 2050) rcp <- '2.6_re'
  
  if(!is.na(splyr)){
    if(splyr == 2050){
      yr <- 2050
      scn <- paste(spl[1:(length(spl)-1)],collapse = '_')
    }
  }
  s <- read_sf(paste0(dir_proc,'dams_future_',sc,'.gpkg')) %>%
    select(geom) %>%
    mutate(scenario = scn, year = yr, ambition = amb, rcp = rcp)
  return(
    s %>%
      mutate(no_dams = nrow(s))
  )
}

t$scenario <- as.factor(t$scenario)
t$year <- as.factor(t$year)
t$ambition <- as.factor(t$ambition)
t$rcp <- as.factor(t$rcp)
levels(t$ambition) <- c('High','Low','Medium')
t$ambition <- factor(t$ambition, levels = c('Low','Medium','High'))

tann <- t %>% as_tibble() %>% select(-geom) %>% distinct()
p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = t,size = .5,shape = 20,alpha = 0.5) +
  # annotate(data = tann, geom = 'Text',label = no_dams, size = 4, x = 0, y = 0) +
  geom_text(
    data    = tann,
    mapping = aes(x = -Inf, y = -Inf, label = no_dams)
    ,hjust   = -0.1,
    vjust   = -0.1
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
       width = 200,height = 200,units = 'mm',dpi = 600)

# overall BARPLOTS -------------------------------

t <- foreach(sc = names_fut,.combine = 'rbind') %do% {
  
  spl <- strsplit(sc,'_')[[1]]
  splyr <- spl[length(spl)] %>% as.numeric()
  yr <- 2100
  scn <- sc
  amb <- spl[2]
  rcp <- 'base'
  if(!is.na(spl[4]) & spl[4] != 2050) rcp <- '2.6'
  if(!is.na(spl[4]) & !is.na(spl[5]) & spl[4] != 2050 & spl[5] != 2050) rcp <- '2.6_re'
  
  if(!is.na(splyr)){
    if(splyr == 2050){
      yr <- 2050
      scn <- paste(spl[1:(length(spl)-1)],collapse = '_')
    }
  }
  
  return(
    read.csv(paste0('tabs/FWC/species_ci/species_ci_summary_oth10_',sc,'.csv')) %>% 
      mutate(delta = ci_cur_meanW - ci_fut_meanW) %>%
      select(type,delta) %>%
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
levels(t$ambition) <- c('High','Low','Medium')
t$ambition <- factor(t$ambition, levels = c('High','Medium','Low'))


library(RColorBrewer)
p <- ggplot(t,aes(y = delta, x = rcp, fill = ambition)) +
  geom_bar(stat='identity',position = 'identity',width = 0.5) + #position = 'identity',
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "PiYG"))(40)[c(40,30,1)]
                    # ,labels = c(expression('1.5'^o*C),expression('2.0'^o*C),expression('3.2'^o*C),expression('4.5'^o*C))
  ) +
  # scale_y_continuous(breaks = seq(0,1,0.25),labels = seq(0,1,0.25)) +
  # guides(fill = guide_legend(title=NULL)) +
  ylab('Delta fragmentation') +
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

ggsave('figs/FWC_barplot_overall.jpg',p,
       width = 180,height = 120,units = 'mm',dpi = 600)

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

t <- foreach(sc = names_fut,.combine = 'rbind') %do% {
  
  spl <- strsplit(sc,'_')[[1]]
  splyr <- spl[length(spl)] %>% as.numeric()
  yr <- 2100
  scn <- sc
  amb <- spl[2]
  rcp <- 'base'
  if(!is.na(spl[4]) & spl[4] != 2050) rcp <- '2.6'
  if(!is.na(spl[4]) & !is.na(spl[5]) & spl[4] != 2050 & spl[5] != 2050) rcp <- '2.6_re'
  
  if(!is.na(splyr)){
    if(splyr == 2050){
      yr <- 2050
      scn <- paste(spl[1:(length(spl)-1)],collapse = '_')
    }
  }
  
  return(
    bas_unit %>%
      left_join(.,readRDS(paste0('proc/CI_BAS_',sc,'.rds')) %>% filter(CI == 'Difference'),by = 'MAIN_BAS') %>%
      mutate(scenario = scn, year = yr, ambition = amb, rcp = rcp)
  )
  
}
t$scenario <- as.factor(t$scenario)
t$year <- as.factor(t$year)
t$ambition <- as.factor(t$ambition)
t$rcp <- as.factor(t$rcp)
levels(t$ambition) <- c('High','Low','Medium')
t$ambition <- factor(t$ambition, levels = c('Low','Medium','High'))


p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = 0) +
  geom_sf(data = t, aes(fill = value), lwd = 0) +
  scale_fill_viridis_c(
    # breaks = seq(0,100,10),
    # labels = seq(0,100,10),
    # limits = c(0,100),
    option = 'C',na.value = "grey90") +
  coord_sf(crs = crs_custom) +
  facet_grid(year + rcp ~ ambition) +
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
ggsave(paste0('figs/FWC_maps_CI_BAS_mean.jpg'),p,
       width = 300,height = 300,units = 'mm',dpi = 600)



#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ends here for now <<<<<<<<<<<<<<<<<<<<


# get area info
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))  %>% 
  as_tibble() %>% 
  select(MAIN_BAS,SUB_AREA) %>%
  group_by(MAIN_BAS) %>%
  summarize(MAIN_BAS_AREA = sum(SUB_AREA))

t <- foreach(sc = names_fut,.combine = 'rbind') %do% {
  
  spl <- strsplit(sc,'_')[[1]]
  splyr <- spl[length(spl)] %>% as.numeric()
  yr <- 2100
  scn <- sc
  amb <- spl[2]
  rcp <- '6.0'
  if(!is.na(spl[4]) & spl[4] != 2050) rcp <- '2.6'
  if(!is.na(spl[4]) & !is.na(spl[5]) & spl[4] != 2050 & spl[5] != 2050) rcp <- '2.6_re'
  
  if(!is.na(splyr)){
    if(splyr == 2050){
      yr <- 2050
      scn <- paste(spl[1:(length(spl)-1)],collapse = '_')
    }
  }
  
  return(
    read.csv(paste0('tabs/FWC/species_ci/species_ci_oth10_',sc,'.csv')) %>% 
      mutate(delta = ci_currentW - ci_futureW) %>%
      select(binomial,type,delta) %>%
      # reshape2::melt(.,id.vars = c('binomial','type')) %>%
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

library(ggplot2)
pb <- ggplot(data = t %>% filter(year == 2100), aes(x = rcp,y = delta)) +
  #   geom_boxplot(aes(fill = ambition))
  # pb
  geom_violin(aes(fill = ambition),color = 'transparent',alpha = 1,lwd = .5, scale = 'width') +
  # geom_boxplot(notch = F, outlier.size = 0.2,width = 0.1, position = position_dodge2(preserve = "total")) +
  # scale_fill_manual(values = two_stage_col) +
  ylab('delta CI') +
  xlab('Scenario') +
  # stat_summary(fun.y=mean, geom="point", aes(fill = ambition),
  #              shape=23, size=2,show.legend = FALSE) +
  # facet_wrap('type',ncol = 1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        # legend.position = 'none',
        # legend.title = element_blank(),
        # axis.text.x=element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey'),
        strip.text = element_blank()
  )
pb





tws <- inner_join(readRDS(paste0('proc/CI_BAS_',sc,'.rds')),hb_data,by = 'MAIN_BAS') %>%
  filter(MAIN_BAS_AREA != 0) %>%
  filter(CI == 'Difference') %>%
  droplevels(.) %>%
  mutate(arealog = log10(MAIN_BAS_AREA))
# rm(hb_data)

length(unique(tws$MAIN_BAS))
# 53,809 #no. of basins

tws$area_cat <- '<2'
tws$area_cat[tws$arealog >= 2] <- '2-3'
tws$area_cat[tws$arealog >= 3] <- '3-4'
tws$area_cat[tws$arealog >= 4] <- '4-5'
tws$area_cat[tws$arealog >= 5] <- '>=5'

tws$area_cat <- factor(tws$area_cat,levels = c('<2','2-3','3-4','4-5','>=5'))

sapply(
  split(tws,tws$area_cat), 
  function(x) data.frame(p = length(unique(x$MAIN_BAS[x$cat == 'Non diadromous'])),
                         d = length(unique(x$MAIN_BAS[x$cat == 'Diadromous'])))
)

tws$type <- factor(tws$CI,levels = c('Present','Future'))
tws$cat <- factor(tws$cat,levels = c('Non diadromous','Diadromous'))

two_stage_col <- RColorBrewer::brewer.pal(9,'BuPu')[c(8,3)]#viridis(n = 40,direction = -1,alpha = 1,option = 'C')[c(5,10)] #brewer.pal('Purples',n=9)[c(4,7)]

pa <- ggplot(data = tws, aes(x = area_cat, y = value, color = type)) +
  geom_violin(aes(fill = type),color = 'transparent',alpha = 1,lwd = .5, scale = 'width') +
  geom_boxplot(fill='white',notch = F, outlier.size = 0.1,width = 0.2,lwd=0.5,position = position_dodge(0.9)) +
  # xlab(bquote('Area of main watershed ['~log[10]~'-'~km^2~']')) +
  xlab('Area of main watershed [Log-sqkm]') +
  ylab('') +
  stat_summary(fun.y=mean, geom="point",aes(fill = type), 
               shape=23, size=2,position = position_dodge(width = 0.9),show.legend = FALSE) +
  scale_fill_manual(values = two_stage_col) +
  scale_color_manual(values = c('Black','Black')) +
  facet_wrap('cat',ncol = 1,strip.position = 'right') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y=element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey'),
        legend.position = 'none')
pa

# OVERALL ---------------------------------------------------------------------------------------

t <- read.csv(paste0('tabs/FWC/species_ci/species_ci_oth10_',sc,'.csv')) %>%
  select(binomial,type,ci_currentW,ci_futureW) %>%
  reshape2::melt(.,id.vars = c('binomial','type')) %>%
  as_tibble() %>%
  mutate(variable = forcats::fct_recode(variable, Present = 'ci_currentW', Future = 'ci_futureW')) %>%
  mutate(type = forcats::fct_recode(type, 'Non diadromous' = 'potamodromous', 'Diadromous' = 'diadromous')) %>%
  mutate(type = factor(type, levels = c('Non diadromous','Diadromous')))

library(ggplot2)
pb <- ggplot(data = t, aes(x = variable,y = value)) +
  geom_violin(aes(fill = variable),color = 'transparent',alpha = 1,lwd = .5, scale = 'width') +
  geom_boxplot(fill='white',notch = F, outlier.size = 0.2,width = 0.1, position = position_dodge2(preserve = "total")) +
  scale_fill_manual(values = two_stage_col) +
  ylab('CI [%]') +
  xlab('All') +
  stat_summary(fun.y=mean, geom="point", aes(fill = variable),
               shape=23, size=2,show.legend = FALSE) +
  facet_wrap('type',ncol = 1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        # legend.title = element_blank(),
        # axis.text.x=element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey'),
        strip.text = element_blank()
  )
pb



library(ggpubr)
p <- ggarrange(ggplotGrob(pb),
               ggplotGrob(pa),
               nrow = 1,ncol = 2,widths = c(0.5,1.1))
ggsave(filename = paste0('figs/FWC/violin_plot_by_species_',sc,'.jpg'),p,
       width = 87*2,height = 87,units = 'mm',dpi = 1000,type='cairo')
# ggsave(filename = 'figs/FWC/violin_plot_by_species.pdf',p,
#        width = 87*2,height = 87,units = 'mm')

# cowplot::get_legend(pb)

