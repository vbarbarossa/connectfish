source('R/MASTER.R')

#------------------------------------------------------------
#> DATA

# computed CI for current and future for each species-basin (data.frame)
CI_tab <- rbind(
  readRDS('proc/CI_tab_global.rds')
  ,readRDS('proc/CI_tab_global_min10k.rds')
)

CI_tab <- CI_tab[CI_tab$alpha == 0.55,]

library(data.table)
CI_tab <- as.data.table(CI_tab)

#there are 2 NAs <<<<<<<<<<<<<<<<<<<<<< would need checking
CI_tab <- CI_tab[!is.na(CI_tab$connectivity.cur),]

# compute the mean across each watershed
tab <- do.call('rbind',
               lapply(
                 split(CI_tab,CI_tab$binomial),function(x) data.frame(
                   binomial = as.character(x$binomial)[1],
                   type = as.character(x$category)[1],
                   ci_current = round(mean(x$connectivity.cur),2),
                   ci_future = round(mean(x$connectivity.fut),2),
                   ci_currentW = round(sum(x$connectivity.cur*x$patches.cum.area)/sum(x$patches.cum.area),2),
                   ci_futureW = round(sum(x$connectivity.fut*x$patches.cum.area)/sum(x$patches.cum.area),2),
                   no_basins = nrow(x),
                   area_total = round(sum(x$patches.cum.area,na.rm=T),2),
                   area_mean = round(mean(x$patches.cum.area,na.rm=T),2),
                   area_median = round(median(x$patches.cum.area,na.rm=T),2)
                   
                 )
               ))

str(tab)

write.csv(tab,'tabs/species_ci.csv',row.names = F)

summary <- do.call(
  'rbind',
  lapply(split(tab,tab$type),
         function(x) data.frame(
           n = nrow(x),
           ci_cur_mean = mean(x$ci_current),
           ci_cur_sd = sd(x$ci_current),
           ci_cur_median = median(x$ci_current),
           ci_cur_iqr = IQR(x$ci_current),
           ci_fut_mean = mean(x$ci_future),
           ci_fut_sd = sd(x$ci_future),
           ci_fut_median = median(x$ci_future),
           ci_fut_iqr = IQR(x$ci_future),
           
           ci_cur_meanW = mean(x$ci_currentW),
           ci_cur_sdW = sd(x$ci_currentW),
           ci_cur_medianW = median(x$ci_currentW),
           ci_cur_iqrW = IQR(x$ci_currentW),
           ci_fut_meanW = mean(x$ci_futureW),
           ci_fut_sdW = sd(x$ci_futureW),
           ci_fut_medianW = median(x$ci_futureW),
           ci_fut_iqrW = IQR(x$ci_futureW)
           
         )
  ))

write.csv(round(summary,2),'tabs/species_ci_summary.csv')






