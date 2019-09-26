t <- read.csv('data/17_0116_future_dams_update_final_v2.csv')

plot(t$Capacity..MW.)
hist(log10(t$Capacity..MW.))
median(t$Capacity..MW.,na.rm = T)
IQR(t$Capacity..MW.,na.rm=T)
sum(!is.na(t$Capacity..MW.))

library(ggplot2)
p <- ggplot(t) + 
  geom_histogram(aes(x = log10(Capacity..MW.)),binwidth = 0.3,fill = 'white',color = 'black',na.rm = T) +
  xlab('Hydropower capacity [Log10-MW]') +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave('figs/Figure_SI_future_dams_hydropower_capacity_hist.jpg',p,width = 100,height = 100,dpi = 600, units = 'mm')
