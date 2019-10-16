source('R/MASTER.R')

#------------------------------------------------------------
#> DATA

# computed CI for current and future for each species-basin (data.frame)
CI_tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  readRDS(paste0('proc/CI_tab_global_',cont,'.rds'))} %>%
  filter(alpha == 0.55) %>%
  as_tibble() %>%
  mutate_each(as.numeric, starts_with("connectivity")) %>%
  mutate_each(as.numeric, starts_with("patches"))



# SPECIE SELECTION----------------------------------------------------------------------------------------------------
# split the overall CI table based on IUCN species and customRanges
iucn_ref <- vroom('proc/hybas12_fish.csv',delim=',') %>%
  select(binomial) %>% distinct()
# load synonyms table
synonyms_table <- vroom(list.files(dir_synonyms_table,full.names = T),delim=',') %>%
  filter(name_iucn %in% iucn_ref$binomial)
# exclude names in iucn data and names in fishbase that are synonyms for iucn
iucn_names <- unique(c(iucn_ref$binomial,synonyms_table$name_src))

# exclude exclusively lentic species
# habitat tables
# for IUCN
habitat_iucn <- read.csv(file_iucn_habitat_type)

# for custom ranges
habitat_custom <- read.csv(file_custom_ranges_habitat_type)

# do the filtering
iucnCI <- CI_tab %>%
  filter(binomial %in% iucn_names) %>%
  filter(binomial %in% habitat_iucn$binomial[habitat_iucn$lotic == 1]) #exclude only lentic

customCI <- CI_tab %>%
  filter(!binomial %in% iucn_names) %>%
  filter(!binomial %in% unique(as.character(habitat_custom$name[habitat_custom$OnlyLake == -1]))) #exclude only lentic

# read specie table with occurrences
occ <- read_sf(file_custom_ranges) %>%
  as_tibble() %>%
  select(-geom) %>%
  filter(name %in% customCI$binomial) # filter out species already covered in the IUCN dataset

# tables creation---------------------------------------------------------------------------------------------------

comparetab <- list()
g = 1
for(oth in c(1,5,10,20,30,50,10**9)){
  
  names_out <- occ$name[occ$no_occ < oth]
  
  # compute stats across each watershed
  tab <- bind_rows(iucnCI,customCI %>% filter(!binomial %in% names_out)) %>%
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
  
  write.csv(tab,paste0('tabs/species_ci_oth',oth,'.csv'),row.names = F)
  
  summary <- tab %>%
    group_by(type) %>%
    summarize(
      n = n(),
      ci_cur_mean = mean(ci_current,na.rm=T),
      ci_cur_sd = sd(ci_current,na.rm=T),
      ci_cur_median = median(ci_current,na.rm=T),
      ci_cur_iqr = IQR(ci_current,na.rm=T),
      ci_fut_mean = mean(ci_future,na.rm=T),
      ci_fut_sd = sd(ci_future,na.rm=T),
      ci_fut_median = median(ci_future,na.rm=T),
      ci_fut_iqr = IQR(ci_future,na.rm=T),
      
      ci_cur_meanW = mean(ci_currentW,na.rm=T),
      ci_cur_sdW = sd(ci_currentW,na.rm=T),
      ci_cur_medianW = median(ci_currentW,na.rm=T),
      ci_cur_iqrW = IQR(ci_currentW,na.rm=T),
      ci_fut_meanW = mean(ci_futureW,na.rm=T),
      ci_fut_sdW = sd(ci_futureW,na.rm=T),
      ci_fut_medianW = median(ci_futureW,na.rm=T),
      ci_fut_iqrW = IQR(ci_futureW,na.rm=T)
    )
  
  write.csv(summary,paste0('tabs/species_ci_summary_oth',oth,'.csv'),row.names = F)
  
  comparetab[[g]] <- data.frame(
    oth = oth,
    diad_n = summary$n[1],
    diad_ci_c = summary$ci_cur_meanW[1],
    diad_ci_f = summary$ci_fut_meanW[1],
    pota_n = summary$n[2],
    pota_ci_c = summary$ci_cur_meanW[2],
    pota_ci_f = summary$ci_fut_meanW[2]
    
  )
  
  g = g+1
  
}

comparetab <- do.call('rbind',comparetab)

write.csv(comparetab,'tabs/compare_CI_different_oth.csv',row.names = F)

# compare to old results-----------------------------------------------------------------------------------------------
# computed CI for current and future for each species-basin (data.frame)
CI_tab_old <- rbind(
  readRDS('../data/connectfish/CI_tab_global.rds')
  ,readRDS('../data/connectfish/CI_tab_global_min10k.rds')
) %>%
  filter(alpha == 0.55) %>%
  as_tibble() %>%
  mutate_each(as.numeric, starts_with("connectivity")) %>%
  mutate_each(as.numeric, starts_with("patches"))


# compute stats across each watershed
tab <- CI_tab_old %>%
  group_by(binomial) %>%
  summarize(
    type = as.character(unique(category)),
    ci_current = round(mean(connectivity.cur,na.rm=T),2),
    ci_future = round(mean(connectivity.fut,na.rm=T),2),
    ci_currentW = round(sum(connectivity.cur*patches.cum.area,na.rm=T)/sum(patches.cum.area),2),
    ci_futureW = round(sum(connectivity.fut*patches.cum.area,na.rm=T)/sum(patches.cum.area),2),
    no_basins = n(),
    area_total = round(sum(patches.cum.area,na.rm=T),2),
    area_mean = round(mean(patches.cum.area,na.rm=T),2)
  )

write.csv(tab,paste0('tabs/species_ci_OLD.csv'),row.names = F)

summary <- tab %>%
  group_by(type) %>%
  summarize(
    n = n(),
    ci_cur_mean = mean(ci_current,na.rm=T),
    ci_cur_sd = sd(ci_current,na.rm=T),
    ci_cur_median = median(ci_current,na.rm=T),
    ci_cur_iqr = IQR(ci_current,na.rm=T),
    ci_fut_mean = mean(ci_future,na.rm=T),
    ci_fut_sd = sd(ci_future,na.rm=T),
    ci_fut_median = median(ci_future,na.rm=T),
    ci_fut_iqr = IQR(ci_future,na.rm=T),
    
    ci_cur_meanW = mean(ci_currentW,na.rm=T),
    ci_cur_sdW = sd(ci_currentW,na.rm=T),
    ci_cur_medianW = median(ci_currentW,na.rm=T),
    ci_cur_iqrW = IQR(ci_currentW,na.rm=T),
    ci_fut_meanW = mean(ci_futureW,na.rm=T),
    ci_fut_sdW = sd(ci_futureW,na.rm=T),
    ci_fut_medianW = median(ci_futureW,na.rm=T),
    ci_fut_iqrW = IQR(ci_futureW,na.rm=T)
  )

write.csv(summary,paste0('tabs/species_ci_summary_OLD.csv'),row.names = F)

comparetab <- rbind(comparetab,
                    data.frame(
                      oth = 'iucn_old',
                      diad_n = summary$n[1],
                      diad_ci_c = summary$ci_cur_meanW[1],
                      diad_ci_f = summary$ci_fut_meanW[1],
                      pota_n = summary$n[2],
                      pota_ci_c = summary$ci_cur_meanW[2],
                      pota_ci_f = summary$ci_fut_meanW[2]
                    ))
comparetab <- comparetab %>%
  mutate(diad_diff = diad_ci_c - diad_ci_f,
         pota_diff = pota_ci_c - pota_ci_f)
write.csv(comparetab,'tabs/compare_CI_different_oth.csv',row.names = F)

# NID comparison------------------------------------------------------------------------------------------------------

# hybas_na does not include Alaska
hb_data <- read_sf(paste0(dir_hybas12,'/hybas_na_lev12_v1c.shp'))

# select only hybas that intersect the US
sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'United States of America',returnclass = 'sf'),54009),
                     st_transform(hb_data,54009),
                     sparse = T)

hb_data <- hb_data[sel[[1]],] %>%
  as_tibble() %>%
  select(-geometry)

for(oth in c(1,5,10,20,30,50,10**9)){
  
  # set min occurrences
  names_out <- occ$name[occ$no_occ < oth]
  
  # need to first filter out raw data based on hybas 
  # compute stats across each watershed
  tab_na <- bind_rows(iucnCI,customCI %>% filter(!binomial %in% names_out)) %>%
    filter(MAIN_BAS %in% unique(hb_data$MAIN_BAS)) %>%
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
    ) %>%
    select(binomial,ci = ci_currentW) %>%
    mutate(binomial = as.character(binomial)) %>%
    arrange(binomial)
  
  # get NID data
  tab_nid <- rbind(
    readRDS('proc/CI_tab_NID_large2.rds')
  ) %>%
    filter(alpha == 0.55) %>%
    as_tibble() %>%
    mutate_each(as.numeric, starts_with("connectivity")) %>%
    mutate_each(as.numeric, starts_with("patches")) %>%
    filter(binomial %in% unique(as.character(tab_na$binomial))) %>%
    filter(MAIN_BAS %in% unique(hb_data$MAIN_BAS)) %>%
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
    )%>%
    select(binomial,ci_nid = ci_currentW,type) %>%
    mutate(binomial = as.character(binomial)) %>%
    arrange(binomial)
  
  compare_na_nid <- inner_join(tab_na,tab_nid) %>%
    mutate(type = factor(recode(type, potamodromous = 'non-diadromous')))
  
  # overall
  valerioUtils::r.squared(compare_na_nid$ci,compare_na_nid$ci_nid)
  valerioUtils::rmse(compare_na_nid$ci,compare_na_nid$ci_nid)
  valerioUtils::KGE(compare_na_nid$ci,compare_na_nid$ci_nid)
  
  stats <- compare_na_nid %>%
    group_by(type) %>%
    summarize(rsq = paste0('R2 = ',round(valerioUtils::r.squared(ci_nid,ci),2)),
              rmse = paste0('RMSE = ',round(valerioUtils::rmse(ci_nid,ci),0)),
              n = paste0('n = ',length(unique(binomial)))) %>%
    mutate(x = 90,y = c(20,10))
  
  p <- ggplot(compare_na_nid) +
    geom_point(aes(x=ci_nid,y=ci),alpha = 0.5) +
    geom_abline(slope = 1,intercept=0,color = 'red') +
    geom_text(data=stats,mapping = aes(x = 98, y = 20, label = rsq),hjust = 1) +
    geom_text(data=stats,mapping = aes(x = 98, y = 13, label = rmse),hjust = 1) +
    geom_text(data=stats,mapping = aes(x = 98, y = 6, label = n),hjust = 1) +
    xlab('CI (NID) [%]') +
    ylab('CI (GRanD+GOODD) [%]') +
    coord_cartesian(expand=F) +
    facet_wrap('type',ncol = 2) +
    theme_bw()
  ggsave(paste0('figs/compare_NID_oth',oth,'.jpg'),p,width = 200, height = 100,units = 'mm',type='cairo')
  
  
  p <- ggplot(compare_na_nid %>% reshape2::melt(data=.,id.vars = c('binomial','type')) %>% as_tibble()) +
    geom_boxplot(aes(x = type, y = value, color = variable)) +
    theme_bw()
  ggsave(paste0('figs/compare_NID_oth',oth,'_boxplot.jpg'),p,width = 200, height = 100,units = 'mm',type='cairo')
  
}





