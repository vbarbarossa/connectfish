source('R/MASTER.R')

library(dplyr); library(ggplot2)

### READ DATA
# get area info
hb_data <- readRDS('proc/hybas12poly_global_w_area_main.rds') %>% 
  .[!duplicated(.$MAIN_BAS),] %>% .[,c('MAIN_BAS','MAIN_BAS_AREA')] 
tws <- merge(readRDS('proc/CI_stats_MAIN_BAS.rds'),as.data.frame(hb_data)[,-3],by = 'MAIN_BAS') %>%
  .[.$MAIN_BAS_AREA != 0,] %>%
  .[.$type != 'DELTA',] %>%
  droplevels(.)
rm(hb_data)

length(unique(tws$MAIN_BAS))
# 53,708 #no. of basins

# do binning based on area and no. species
tws$arealog <- log10(tws$MAIN_BAS_AREA)
plot(log10(tws$no.sp[tws$type == 'Present' & tws$cat == 'Potamodromous']),tws$mean[tws$type == 'Present' & tws$cat == 'Potamodromous'])
plot(log10(tws$MAIN_BAS_AREA[tws$type == 'Present' & tws$cat == 'Potamodromous']),tws$mean[tws$type == 'Present' & tws$cat == 'Potamodromous'])

#### AREA #########################################
tws$area_cat <- '<2'
tws$area_cat[tws$arealog >= 2] <- '2-3'
tws$area_cat[tws$arealog >= 3] <- '3-4'
tws$area_cat[tws$arealog >= 4] <- '4-5'
tws$area_cat[tws$arealog >= 5] <- '>=5'

tws$area_cat <- factor(tws$area_cat,levels = c('<2','2-3','3-4','4-5','>=5'))

sapply(
  split(tws,tws$area_cat), 
  function(x) data.frame(p = length(unique(x$MAIN_BAS[x$cat == 'Potamodromous'])),
                         d = length(unique(x$MAIN_BAS[x$cat == 'Diadromous'])))
)

# tws <- droplevels(tws[tws$type != 'DELTA',]) # already filtered out earlier
tws$type <- factor(tws$type,levels = c('Present','Future'))

bquote("Hello" ~ r[xy] == .(cor) ~ "and" ~ B^2)

library(ggplot2)
pa <- ggplot(data = tws, aes(x = area_cat, y = mean, color = type)) +
  geom_boxplot(notch = F, alpha = 0.2,outlier.size = 0.5) +
  xlab(bquote('Area of main watershed ['~log[10]~'-'~km^2~']')) +
  ylab('CI [%]') +
  stat_summary(fun.y=mean, geom="point",fill = 'red', 
               shape=23, size=1,position = position_dodge(width = 0.75),show.legend = FALSE) +
  scale_color_manual(values = c('Grey20','Grey60')) +
  facet_wrap('cat',ncol = 2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey'),
        legend.position = 'none',
        strip.text = element_blank())
pa
# #### SR ###########################################
# tws$sr_cat <- '<10'
# tws$sr_cat[tws$no.sp >= 10] <- '10-50'
# tws$sr_cat[tws$no.sp >= 50] <- '50-100'
# tws$sr_cat[tws$no.sp >= 100] <- '100-200'
# tws$sr_cat[tws$no.sp >= 200] <- '>=200'
# 
# tws$sr_cat <- factor(tws$sr_cat,levels = c('<10','10-50','50-100','100-200','>=200'))
# 
# sapply(
#   split(tws,tws$sr_cat), 
#   function(x) data.frame(p = length(unique(x$MAIN_BAS[x$cat == 'Potamodromous'])),
#                          d = length(unique(x$MAIN_BAS[x$cat == 'Diadromous'])))
# )
# 
# # tws <- droplevels(tws[tws$type != 'DELTA',])
# tws$type <- factor(tws$type,levels = c('Present','Future'))
# 
# library(ggplot2)
# ps <- ggplot() +
#   geom_boxplot(data = tws, aes(x = sr_cat, y = mean, color = type),notch = F, alpha = 0.2,outlier.size = 0.7) +
#   scale_color_manual(values = c('Grey60','Grey20')) +
#   xlab('Number of species in the main watershed') +
#   ylab('CI [%]') +
#   facet_wrap('cat',ncol = 2) +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         legend.position = 'none',
#         strip.text = element_blank())

#### SPECIES LEVEL ###############################################################

t <- read.csv('tabs/species_ci.csv') %>% .[,c(1:2,5:6)] %>% reshape2::melt(.,id.vars = c('binomial','type'))
levels(t$variable) <- c('Present','Future')
levels(t$type) <- c('Diadromous','Non-diadromous')

library(ggplot2)
pb <- ggplot(data = t, aes(x = variable,y = value, color = variable)) +
  # geom_violin(data = t, aes(x= 1,y = value, color = type),draw_quantiles = c(0.25,0.5,0.75)) +
  geom_boxplot(notch = F, alpha = 0.2,outlier.size = 0.5,width = 0.5, position = position_dodge2(preserve = "total")) +
  scale_color_manual(values = c('Grey20','Grey60')) +
  ylab('CI [%]') +
  xlab(' ') +
  stat_summary(fun.y=mean, geom="point", fill = 'red',
               shape=23, size=1.5,show.legend = FALSE) +
  facet_wrap('type',ncol = 2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey')
        )
pb

# # boxplot and violingplot have different way to calculate quantiles, see below
# by(t[,'value'], t[,c('variable','type')], function(v) quantile(density(v)$x)) #violin, based on kernel density
# by(t[,'value'], t[,c('variable','type')], function(x) boxplot.stats(x, coef=5)$stats) #box, based on empirical distribution

# header
# ph <- ggarrange(NULL,text_grob("Diadromous"),text_grob("Potamodromous"),
#                 nrow=1,ncol = 3,widths = c(0.15,0.5,0.5))
library(ggpubr)
# p <- ggarrange(ggplotGrob(pb + theme(legend.position = 'none')),ggplotGrob(pa),ggplotGrob(ps),cowplot::get_legend(pb),
#                nrow = 4,ncol = 1,heights = c(1,1,1,0.1))
p <- ggarrange(ggplotGrob(pb + theme(legend.position = 'none',panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey'))),ggplotGrob(pa),cowplot::get_legend(pb),
               nrow = 3,ncol = 1,heights = c(1,1,0.1))
ggsave(filename = 'figs/Figure_2_boxplots.jpg',p,
       width = 100,height = 150,units = 'mm',dpi = 600)


