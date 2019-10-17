source('R/MASTER.R')


# by BAS AREA ---------------------------------------------------------------------------------------

# get area info
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))  %>% 
  as_tibble() %>% 
  select(MAIN_BAS,SUB_AREA) %>%
  group_by(MAIN_BAS) %>%
  summarize(MAIN_BAS_AREA = sum(SUB_AREA))

tws <- inner_join(readRDS('proc/CI_BAS.rds'),hb_data,by = 'MAIN_BAS') %>%
  filter(MAIN_BAS_AREA != 0) %>%
  filter(CI != 'Difference') %>%
  droplevels(.) %>%
  mutate(arealog = log10(MAIN_BAS_AREA))
rm(hb_data)

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

pa <- ggplot(data = tws, aes(x = area_cat, y = value, color = type)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75),alpha = 0.2,lwd = .5, scale = 'width') +
  # geom_boxplot(notch = F, alpha = 0.2,outlier.size = 0.5) +
  xlab(bquote('Area of main watershed ['~log[10]~'-'~km^2~']')) +
  ylab('CI [%]') +
  stat_summary(fun.y=mean, geom="point",fill = 'red', 
               shape=23, size=1,position = position_dodge(width = 0.9),show.legend = FALSE) +
  scale_color_manual(values = c('Grey20','Grey60')) +
  facet_wrap('cat',ncol = 2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey'),
        legend.position = 'none',
        strip.text = element_blank())
# pa

# OVERALL ---------------------------------------------------------------------------------------

t <- read.csv('tabs/species_ci/species_ci_oth10.csv') %>% 
  select(binomial,type,ci_currentW,ci_futureW) %>%
  reshape2::melt(.,id.vars = c('binomial','type')) %>%
  as_tibble() %>%
  mutate(variable = forcats::fct_recode(variable, Present = 'ci_currentW', Future = 'ci_futureW')) %>%
  mutate(type = forcats::fct_recode(type, 'Non diadromous' = 'potamodromous', 'Diadromous' = 'diadromous')) %>%
  mutate(type = factor(type, levels = c('Non diadromous','Diadromous')))

library(ggplot2)
pb <- ggplot(data = t, aes(x = variable,y = value, color = variable)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75),alpha = 0.2,lwd = .5, scale = 'width') +
  # geom_boxplot(notch = F, alpha = 0.2,outlier.size = 0.5,width = 0.5, position = position_dodge2(preserve = "total")) +
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
# pb

library(ggpubr)
p <- ggarrange(ggplotGrob(pb + theme(legend.position = 'none',panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey'))),ggplotGrob(pa),cowplot::get_legend(pb),
               nrow = 3,ncol = 1,heights = c(1,1,0.1))
ggsave(filename = 'figs/violin_plot_by_species.jpg',p,
       width = 100,height = 150,units = 'mm',dpi = 600,type='cairo')

