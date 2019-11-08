source('R/MASTER.R')

library(tidyr)
nid <- readRDS('proc/compare_NID_large.rds') %>%
  as_tibble() %>%
  mutate(region = 'United States') %>%
  spread(variable,value)
bra <- readRDS('proc/compare_BRA_large.rds') %>%
  as_tibble() %>%
  mutate(region = 'Brazil') %>%
  spread(variable,value)
mis <- readRDS('proc/compare_MEK_large.rds') %>%
  as_tibble() %>%
  mutate(region = 'Greater Mekong') %>%
  spread(variable,value)

tab <- bind_rows(nid,bra,mis) %>%
  mutate(region = as.factor(region)) %>%
  select(-type) %>%
  mutate(diff = ci - ci_nid)

stats <- tab %>%
  group_by(region) %>%
  summarize(rsq = paste0('R2 = ',round(valerioUtils::r.squared(ci_nid,ci),2)),
            rmse = paste0('RMSE = ',round(valerioUtils::rmse(ci_nid,ci),0)),
            n = paste0('n = ',length(unique(binomial))),
            mean_diff = round(mean(diff),2)
            )

            

p <- ggplot(tab) +
  geom_point(aes(x=ci_nid,y=ci),alpha = 0.5) +
  geom_abline(slope = 1,intercept=0,color = 'red') +
  # geom_text(data=stats,mapping = aes(x = 98, y = 20, label = rsq),hjust = 1) +
  geom_text(data=stats,mapping = aes(x = 98, y = 13, label = rsq),hjust = 1) +
  geom_text(data=stats,mapping = aes(x = 98, y = 6, label = n),hjust = 1) +
  xlab('CI (GRanD+GOODD+National) [%]') +
  ylab('CI (GRanD+GOODD) [%]') +
  coord_cartesian(expand=F) +
  facet_wrap('region',ncol = 3) +
  theme_bw()
p
ggsave(filename = 'figs/compare_with_large_dams_scatter.jpg',p,
       width = 300,height = 100,units = 'mm',dpi = 1000,type='cairo')

p <- ggplot(data = tab, aes(x = region, y = diff)) +
  geom_violin(scale='width',color = 'transparent',fill = 'Grey70',alpha = 0.8) +
  geom_boxplot(fill = 'white',width = 0.1,outlier.size = 0.2) +
  stat_summary(fun.y=mean, geom="point", aes(fill = variable),
               shape=23, size=2,show.legend = FALSE, color = 'black',fill='red') +
  xlab(' ') +
  ylab('ΔCI (G&G - G&G+National_large) [%]') +
  coord_cartesian(ylim = c(0,100), expand = F) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey')
  )
p
ggsave(filename = 'figs/compare_with_large_dams_violin.jpg',p,
       width = 100,height = 100,units = 'mm',dpi = 1000,type='cairo')


# # A tibble: 3 x 5
# region         rsq       rmse      n        mean_diff
# <fct>          <chr>     <chr>     <chr>        <dbl>
#   1 Brazil         R2 = 0.91 RMSE = 10 n = 2006      5   
# 2 Greater Mekong R2 = 0.82 RMSE = 17 n = 937       9.72
# 3 United States  R2 = 0.98 RMSE = 7  n = 1394      3.67