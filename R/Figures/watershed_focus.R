source('R/MASTER.R')

library(ggpubr)

# computed CI for current and future for each species-basin (data.frame) --------------------------------------
CI_tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  readRDS(paste0('proc/CI_tab_global_',cont,'.rds'))} %>%
  filter(alpha == 0.55) %>%
  as_tibble() %>%
  mutate_each(as.numeric, starts_with("connectivity")) %>%
  mutate_each(as.numeric, starts_with("patches")) %>%
  filter(binomial %in% read.csv('tabs/species_ci/species_ci_oth10.csv')$binomial)
CI_tab$category <- factor(CI_tab$category, levels = c('potamodromous','diadromous'))
levels(CI_tab$category) <- c('Non diadromous','Diadromous')

# HYBAS DATA --------------------------------------------------------------------------------------------------
hb_data <- readRDS('proc/hybas12poly_global_w_area_main.rds')
main_bas <- readRDS('proc/watersheds_hybas12.rds')
colnames(main_bas)[2] <- 'geometry'
st_geometry(main_bas) <- 'geometry'

# BASINS SELECTION --------------------------------------------------------------------------------------------

basin_IDs <- c(6120007000,1120020040,4120017020,1120022420,1120023890,2120008490,4120023060,5120041230)
basin_names <- c('Amazon','Congo','Mekong','Niger','Comoe','Danube','Salween','Purari')
# basin_IDs <- c(6120007000,1120020040,4120017020,1120022420,4120023810,2120008490,4120023060,5120041230)
# basin_names <- c('Amazon','Congo','Mekong','Niger','Irrawaddy','Danube','Salween','Purari')

# and filter data
hb_data <- hb_data[hb_data$MAIN_BAS %in% basin_IDs,]

# tabulated CI at the HYBAS level
CI_stats <- as.data.table(readRDS('proc/CI_HB.rds'))
CI_stats <- merge(hb_data[,c('HYBAS_ID','MAIN_BAS')],CI_stats,by = 'HYBAS_ID')


# DAMS LOCATION -----------------------------------------------------------------------------------------------
grand <- st_read(file_grand_dams) #7,320
# GOOD2 unsnapped
good2 <- st_read(file_good2_dams) #32,613
dams_cur <- rbind(
  cbind(data.frame(ID = grand$GRAND_ID,database = 'GRanD'),st_coordinates(grand)),
  cbind(data.frame(ID = good2$DAM_ID,database = 'GOOD2'),st_coordinates(good2))
)
# compare to the actual georeferenced data used in the study
# dams_cur <- dams_cur[dams_cur$ID %in% readRDS('proc/dams_current_hydrobasins.rds')$ID,]
# convert to sf spatial points
sdams_cur <- st_as_sf(dams_cur,coords = c('X','Y'),crs=4326)

# future dams from Zarfl
dams_fut <- read.csv(file_frhed_dams)
# dams_fut <- dams_fut[dams_fut$DAM_ID %in% readRDS('proc/dams_future_hydrobasins.rds')$DAM_ID,]

# convert to spatial points
sdams_fut <- st_as_sf(dams_fut,coords = c('Lon_2016','Lat_2016'),crs=4326)

# select dams for each basin
dcur <- foreach(i = 1:length(basin_names),.combine='rbind') %do% {
  bname <- basin_names[i]
  bid <- basin_IDs[i]
  d <- sdams_cur[st_intersects(sdams_cur,main_bas[main_bas$MAIN_BAS == bid,],sparse = F),]
  if(nrow(d) > 0) d$name <- bname
  return(d)
}

dfut <- foreach(i = 1:length(basin_names),.combine='rbind') %do% {
  bname <- basin_names[i]
  bid <- basin_IDs[i]
  d <- sdams_fut[st_intersects(sdams_fut,main_bas[main_bas$MAIN_BAS == bid,],sparse = F),]
  if(nrow(d) > 0) d$name <- bname
  return(d)
}

# SELECTED RIVERS ###############################################################################################
rivers <- foreach(i = basin_names,.combine = 'rbind') %do% {
  r <- st_read(paste0('proc/Figure_3/',i,'_riv.shp'))
  r$name <- i
  return(r[r$Log_Q_avg >= 2,])
}

#################################################################################################################
# MAIN PLOTS

# rivers #list
# dcur and dfut #list
# basins
basins <- foreach(i = 1:length(basin_names),.combine = 'rbind') %do% {
  b <- main_bas[main_bas$MAIN_BAS %in% basin_IDs[i],]
  b$name <- basin_names[i]
  return(b)
}
basins$name <- factor(basins$name)
rivers$name <- factor(rivers$name)
dcur$name <- factor(dcur$name)
dfut$name <- factor(dfut$name)
  
# do separate graphs for diadromous and potamodromous
for(i in 1:length(basin_names)){
  
  # plot.list1 <- foreach(i = ind_cat:(ind_cat + 3)) %do% {
  
  # MAPS ##############################s################################
  bas_id <- basin_IDs[i]
  ws_lab <- basin_names[i]
  # headers <- F
  # if(i %in% c(1,5)) headers <- T
  
  # retrieve spatial variables for the basin
  bas <- main_bas[main_bas$MAIN_BAS == bas_id,]
  riv <- rivers[rivers$name == ws_lab,]
  dc <- dcur[dcur$name == ws_lab,]
  df <- dfut[dfut$name == ws_lab,]
  
  riv1 <- riv[riv$Log_Q_avg < 3,]
  riv2 <- riv[riv$Log_Q_avg >=3 & riv$Log_Q_avg <4,]
  riv3 <- riv[riv$Log_Q_avg >=4 & riv$Log_Q_avg <5,]
  riv4 <- riv[riv$Log_Q_avg >=5,]
  
  # map the morphology + dams
  p_base <- ggplot(bas) +
    geom_sf(color = NA,fill='grey90') + #no border
    geom_sf(data = riv1,size = 0.1,color = 'grey40') +
    geom_sf(data = riv2,size = 0.5,color = 'grey40') +
    geom_sf(data = riv3,size = 1,color = 'grey40') +
    geom_sf(data = riv4,size = 1.5,color = 'grey40') +
    geom_sf(data = dc,size = 1.5,fill = 'white',shape = 21,alpha = 0.5) +
    geom_sf(data = df,size = 1.5,fill = 'red',shape = 21,alpha = 0.5) +
    theme_bw() +
    theme(
      text = element_text(size = 15),
      panel.grid = element_line(colour = 'transparent'),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.background.x = element_blank(),
      strip.background.y = element_blank(),
      legend.position = 'none'
    )
  # p_base
  
  for(cat_fish in c('Non diadromous','Diadromous')){
    # for(ind_cat in c(1,5)){
    # cat_fish <- 'Potamodromous'
    # if(ind_cat == 5) cat_fish <- 'Diadromous'
    
    ci_main <- CI_stats[CI_stats$MAIN_BAS == basin_IDs[i] & CI_stats$cat == cat_fish,] %>%
      filter(CI != 'Difference') # filter out
    if(nrow(ci_main) > 0){
      ci_main$CI <- factor(ci_main$CI,levels = c('Present','Future'))
      ci_main$cat <- factor(ci_main$cat, levels = cat_fish)
      ci_main$name <- basin_names[i]
      ci_main$name <- factor(ci_main$name)
      
    }else{
      ci_main[1:2,1:5] <- rep(NA,10)
      ci_main$CI <- c('Present','Future')
      ci_main$CI <- factor(ci_main$CI,levels = c('Present','Future'))
      ci_main$cat <- cat_fish
      ci_main$cat <- factor(ci_main$cat, levels = cat_fish)
      ci_main$name <- basin_names[i]
      ci_main$name <- factor(ci_main$name)
      
      
    }
    
    ci_spatial <- ci_main[ci_main$name == ws_lab,]
    
    p_maps <- foreach(ty = c('Present','Future')) %do% {
      
      ci_sel <- ci_spatial[ci_spatial$CI == ty,]
      ci_sel$CI <- factor(ci_sel$CI)
      
      # map the CI
      p <- ggplot(bas) +
        geom_sf(color = NA,fill='grey90') + #no border
        geom_sf(data = ci_sel,aes(fill = value),color=NA) +
        scale_fill_viridis_c(breaks = seq(0,100,10),
                             labels = seq(0,100,10),
                             limits = c(0,100),
                             option = 'C',na.value = "grey90") +
        # facet_wrap('CI',ncol=2) +
        theme_bw() +
        theme(
          text = element_text(size = 15),
          panel.grid = element_line(colour = 'transparent'),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          # strip.background.x = element_blank(),
          # strip.background.y = element_blank(),
          # strip.text = element_blank(),
          legend.position = 'none'
          
        )
      
      return(p)
      
    }
    
    
    # if(!headers) p_maps <- p_maps + theme(strip.text = element_blank())
    # p_maps
    
    # BOXPLOT ####################################################################
    
    # create dataset for boxplots
    ci <- CI_tab[CI_tab$category == cat_fish & CI_tab$MAIN_BAS == bas_id,]
    ci$category <- factor(ci$category, levels = cat_fish)
    ci <- reshape2::melt(ci,measure.vars = c('connectivity.cur','connectivity.fut'))
    ci$variable <- factor(ci$variable,levels = c('connectivity.cur','connectivity.fut'))
    levels(ci$variable) <- c('Pres.','Futu.')
    
    p_bp <- ggplot(ci) +
      geom_boxplot(aes(x = variable, y = value),fill = 'grey90',color = 'grey40',width = 0.4,outlier.size=0.5) +
      scale_y_continuous(limits = c(0,100),breaks = c(0,50,100),labels = c('0%','50%','100%')) +
      xlab('') +
      ylab('CI') +
      stat_summary(aes(x = variable, y = value),fun.y=mean, geom="point", color = 'grey40',fill = 'red',
                   shape=23, size=1,show.legend = FALSE) +
      coord_cartesian(xlim = c(0.6,2.4),expand = F) +
      ggtitle(' ') +
      theme_bw() +
      theme(
        text = element_text(size = 10),
        panel.grid = element_line(colour = 'transparent'),
        panel.border = element_blank(),
        axis.line.y.left = element_line(),
        axis.ticks.x = element_blank(),
        legend.position = 'none'
        # axis.text = element_blank(),
      )
    
    p <- annotate_figure(ggarrange(ggplotGrob(p_base),ggplotGrob(p_maps[[1]]),ggplotGrob(p_maps[[2]]),ggplotGrob(p_bp),
                                   nrow=1,ncol = 4,widths = c(0.5,0.5,0.5,0.3)),
                         left = text_grob(paste0(ws_lab,' (',length(unique(ci$binomial)),')'),rot = 90))
    
    # return(p)
    ggsave(filename = paste0(dir_(paste0(dir_('figs/Figure_3/'),cat_fish,'/')),ws_lab,'.jpg'),
           p,width = 200,height = 50,units = 'mm',dpi = 600,type = 'cairo')
    
    if(i == 1){
      
      # header
      p <- annotate_figure(ggarrange(text_grob("Hydrography"),
                                     text_grob("Present"),
                                     text_grob("Future"),
                                     NULL,
                                     nrow=1,ncol = 4,widths = c(0.5,0.5,0.5,0.3))
                           ,left = text_grob("Am", color = "white",rot = 90))
      
      ggsave(filename = paste0(dir_('figs/Figure_3/'),'header.jpg'),
             p,width = 200,height = 8,units = 'mm',dpi = 600,type='cairo')
      
      # legend for CI
      p_leg_ci <- ggplot(bas) +
        geom_sf(color = NA,fill='grey90') + #no border
        geom_sf(data = ci_sel[1:10,],aes(fill = value),color=NA) +
        scale_fill_viridis_c(breaks = seq(0,100,10),
                             labels = seq(0,100,10),
                             limits = c(0,100),
                             option = 'C',na.value = "grey90") +
        # guides(fill = guide_legend(title = '')) +
        theme_bw() +
        theme(
          text = element_text(size = 12),
          panel.grid = element_line(colour = 'transparent'),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.width = unit(3,'line'),
          legend.key.height = unit(0.8,'line'),
          legend.title = element_blank(),
          legend.position = 'bottom'
        )
      
      # legend for dams
      d <- data.frame(a = c(0,0),b = c(0,0), fac = factor(c('Present','Future'),levels = c('Present','Future')))
      p_leg_dams <- ggplot(d) +
        geom_point(aes(x = a,y = b,fill = fac), shape = 21,alpha = 0.5,size = 2,color = 'black') +
        scale_fill_manual(values = c('white','red')) +
        theme_bw() +
        theme(
          text = element_text(size = 12),
          panel.grid = element_line(colour = 'transparent'),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.direction = 'horizontal'
        )
      
      ggsave(filename = paste0(dir_('figs/Figure_3/'),'legend.jpg'),
             annotate_figure(ggarrange(cowplot::get_legend(p_leg_dams),cowplot::get_legend(p_leg_ci),NULL,
                                       nrow=1,ncol = 3,widths = c(0.5,1,0.3))
                             ,left = text_grob("Am", color = "white",rot = 90))
             ,width = 200,height = 8,units = 'mm',dpi = 600,type='cairo')
      
      
      
      
    }
    
  }
  
  
  # p<-ggarrange(plotlist = plot.list1,ncol = 1,nrow=4)
  # cowplot::ggsave(filename = paste0('figs/Figure_3_watersheds_zoom_',cat_fish,'.jpg'),p,width = 220,height = 200,units = 'mm',dpi = 600)
  
  
}

crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#basins location map for SI
world <- rnaturalearth::ne_countries(returnclass = "sf")[,1]
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",
                                 returnclass = "sf")
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",
                                         returnclass = "sf")

p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = basins, aes(fill = name), lwd = 0, color = 'Gray90') +
  scale_fill_brewer(palette = 'Set1') +
  coord_sf(crs = crs_custom) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal'
  )

ggsave('figs/basins_location_figure_3.jpg',p,
       width = 200,height = 120,units = 'mm',dpi = 600,type = 'cairo')


