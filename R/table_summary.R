source('R/MASTER.R')

#------------------------------------------------------------
#> DATA

# computed CI for current and future for each species-basin (data.frame)
CI_tab <- rbind(
  readRDS('proc/CI_tab_global.rds')
  ,readRDS('proc/CI_tab_global_min10k.rds')
) %>%
  filter(alpha == 0.55) %>%
  as_tibble() %>%
  mutate_each(as.numeric, starts_with("connectivity")) %>%
  mutate_each(as.numeric, starts_with("patches"))


# compute stats across each watershed
tab <- CI_tab %>%
  group_by(binomial) %>%
  summarize(
    type = as.character(unique(category)),
    ci_current = round(mean(connectivity.cur),2),
    ci_future = round(mean(connectivity.fut),2),
    ci_currentW = round(sum(connectivity.cur*patches.cum.area)/sum(patches.cum.area),2),
    ci_futureW = round(sum(connectivity.fut*patches.cum.area)/sum(patches.cum.area),2),
    no_basins = n(),
    area_total = round(sum(patches.cum.area,na.rm=T),2),
    area_mean = round(mean(patches.cum.area,na.rm=T),2)
  )

write.csv(tab,'tabs/species_ci.csv',row.names = F)

# filter for min occurrence records here <<<<<<<<<<<
# and show CI at different thresholds

# read specie table with occurrences

# 

summary <- tab %>%
  group_by(type) %>%
  summarize(
    n = n(),
    ci_cur_mean = mean(ci_current),
    ci_cur_sd = sd(ci_current),
    ci_cur_median = median(ci_current),
    ci_cur_iqr = IQR(ci_current),
    ci_fut_mean = mean(ci_future),
    ci_fut_sd = sd(ci_future),
    ci_fut_median = median(ci_future),
    ci_fut_iqr = IQR(ci_future),
    
    ci_cur_meanW = mean(ci_currentW),
    ci_cur_sdW = sd(ci_currentW),
    ci_cur_medianW = median(ci_currentW),
    ci_cur_iqrW = IQR(ci_currentW),
    ci_fut_meanW = mean(ci_futureW),
    ci_fut_sdW = sd(ci_futureW),
    ci_fut_medianW = median(ci_futureW),
    ci_fut_iqrW = IQR(ci_futureW)
  )

write.csv(round(summary,2),'tabs/species_ci_summary.csv')





