source('R/MASTER.R')

nid <- readRDS('proc/compare_NID.rds') %>%
  as_tibble() %>%
  mutate(region = 'United States')
bra <- readRDS('proc/compare_BRA.rds') %>%
  as_tibble() %>%
  mutate(region = 'Brazil')
mis <- readRDS('proc/compare_MEK.rds') %>%
  as_tibble() %>%
  mutate(region = 'Greater Mekong')



tab <- bind_rows(nid,bra,mis) %>%
  mutate(variable = factor(variable))
levels(tab$variable) <- c('large','large+small')

library(tidyr)
stats <- tab %>%
  spread(variable,value) %>%
  mutate(diff = large - `large+small`) %>%
  group_by(region) %>%
  summarize(rsq = paste0('R2 = ',round(valerioUtils::r.squared(`large+small`,large),2)),
            rmse = paste0('RMSE = ',round(valerioUtils::rmse(`large+small`,large),0)),
            n = paste0('n = ',length(unique(binomial))),
            mean_diff = round(mean(diff),2)
  )

# # A tibble: 3 x 5
# region         rsq       rmse      n        mean_diff
# <chr>          <chr>     <chr>     <chr>        <dbl>
#   1 Brazil         R2 = 0.96 RMSE = 7  n = 2006      2.95
# 2 Greater Mekong R2 = 0.95 RMSE = 9  n = 937       3.68
# 3 United States  R2 = 0.87 RMSE = 18 n = 1394     11.6 

p <- ggplot(data = tab, aes(x = variable, y = value)) +
  geom_violin(scale='width',color = 'transparent',fill = 'Grey70',alpha = 0.8) +
  geom_boxplot(fill = 'white',width = 0.1) +
  facet_wrap('region') +
  xlab(' ') +
  ylab('CI [%]') +
  stat_summary(fun.y=mean, geom="point", aes(fill = variable),
               shape=23, size=2,show.legend = FALSE, color = 'black',fill='red') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey')
  )
p
ggsave(filename = 'figs/compare_with_small_dams.jpg',p,
       width = 114,height = 87,units = 'mm',dpi = 1000,type='cairo')
ggsave(filename = 'figs/compare_with_small_dams.pdf',p,
       width = 114,height = 87,units = 'mm')

p <- ggplot(data = tab %>% spread(variable,value) %>% mutate(diff = large - `large+small`), 
            aes(x = region, y = diff)) +
  geom_violin(scale='width',color = 'transparent',fill = 'Grey70',alpha = 0.8) +
  geom_boxplot(fill = 'white',width = 0.1,outlier.size = 0.2) +
  stat_summary(fun.y=mean, geom="point", aes(fill = variable),
               shape=23, size=2,show.legend = FALSE, color = 'black',fill='red') +
  xlab(' ') +
  ylab('ΔCI (large - large+small) [%]') +
  coord_cartesian(ylim = c(0,100), expand = F) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey')
  )
p
ggsave(filename = 'figs/compare_with_small_dams_diffviolin.jpg',p,
       width = 87,height = 87,units = 'mm',dpi = 1000,type='cairo')
ggsave(filename = 'figs/compare_with_small_dams_diffviolin.pdf',p,
       width = 87,height = 87,units = 'mm')


