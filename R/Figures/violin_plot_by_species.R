source('R/MASTER.R')


# by BAS AREA ---------------------------------------------------------------------------------------
dir_hybas12 <- 'F:/hydroBASINS/global_lev12'
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

t <- read.csv('tabs/species_ci/species_ci_oth10.csv') %>% 
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
ggsave(filename = 'figs/violin_plot_by_species.jpg',p,
       width = 87*2,height = 87,units = 'mm',dpi = 1000,type='cairo')
ggsave(filename = 'figs/violin_plot_by_species.pdf',p,
       width = 87*2,height = 87,units = 'mm')

cowplot::get_legend(pb)