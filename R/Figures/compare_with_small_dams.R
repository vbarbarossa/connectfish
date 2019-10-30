source('R/MASTER.R')

nid <- readRDS('proc/compare_NID.rds') %>%
  as_tibble() %>%
  mutate(region = 'USA')
bra <- readRDS('proc/compare_BRA.rds') %>%
  as_tibble() %>%
  mutate(region = 'BRA')
mis <- readRDS('proc/compare_MEK.rds') %>%
  as_tibble() %>%
  mutate(region = 'MIS')


tab <- bind_rows(nid,bra,mis) %>%
  mutate(variable = factor(variable))
levels(tab$variable) <- c(' ','+ small dams')

p <- ggplot(data = tab, aes(x = variable, y = value)) +
  geom_violin(scale='width',color = 'transparent',fill = 'Grey70',alpha = 0.8) +
  geom_boxplot(fill = 'white',width = 0.1) +
  facet_wrap('region') +
  xlab(' ') +
  ylab('CI [%]') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey')
  )

ggsave(filename = 'figs/compare_with_small_dams.jpg',p,
       width = 120,height = 70,units = 'mm',dpi = 600,type='cairo')


