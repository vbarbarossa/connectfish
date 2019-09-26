# alpha-sensitivity

source('R/MASTER.R')

library(foreach)
tab <- foreach(i = 1:9,.combine = 'rbind') %do% readRDS(paste0('proc/alpha_sensitivity/CI_stats_HYBAS_ID_',i,'.rds'))

# for testing tab[c(1:1000,35000,36000),]
library(ggplot2)
p1 <- ggplot() + 
  geom_point(data = tab,aes(x = mean, y = range, fill = category, shape = category), alpha = 0.4) +
  xlab('CI mean [%]') +
  ylab('CI range (max - min) [%]') +
  facet_wrap('type',nrow=1) +
  theme_bw() + 
  theme(
    text = element_text(size = 12),
    legend.position = 'top',legend.direction = 'horizontal',
    legend.title = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank())

# and max vs min
p2 <- ggplot() + 
  geom_point(data = tab,aes(x = min, y = max, fill = category, shape = category), alpha = 0.4) +
  xlab('CI min [%]') +
  ylab('CI max [%]') +
  facet_wrap('type',nrow=1) +
  theme_bw() + 
  theme(
    text = element_text(size = 12),
    legend.position = 'none',
    strip.text = element_blank(),
    strip.background = element_rect(colour = 'white'),
    panel.grid = element_blank())

p <- gridExtra::arrangeGrob(p1,p2,ncol = 1,heights = c(1,0.85))
ggsave('figs/Figure_SI_alpha_sensitivity.jpg',p,width = 220,height = 220,dpi = 600,units = 'mm')


