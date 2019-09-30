#Valerio Barbarossa, 29 Sep 2019
# script that produces tables used for figures

source('R/MASTER.R')

# hydrobasins data
hb_data <- readRDS('proc/hybas12poly_global_w_area_main.rds')

# fishbase metadata
fishbase <- taxonomy() %>% # get all species available (vector)
  species(.,fields = c('Species','AnaCat')) %>% # get species table for all species
  rename(binomial = Species)

fishbase$AnaCat <- as.factor(fishbase$AnaCat) 
levels(fishbase$AnaCat) <- c(rep('Diad.',6),'Non.','Ocea.','Ocea.','Pota.','Pota.')

# habtat metadata


# fish data
sp_data <- bind_rows(
  # read hybas12 on IUCN
  vroom('proc/hybas12_fish.csv',delim=','),
  # read hybas12 on customRanges
  vroom(paste0('proc/hybas12_fish_custom_ranges_occth',min_no_occ,'.csv'),delim=',')
) %>%
  inner_join(.,hb_data %>% as_tibble() %>% select(HYBAS_ID,MAIN_BAS,SUB_AREA,MAIN_BAS_AREA),by="HYBAS_ID") %>%
  as.data.table(.)

# assign diadromous-non diadromous category
sp_data$diad <- 'f'
sp_data$diad[sp_data$binomial %in% fishbase$binomial[fishbase$AnaCat == 'Diad.']] <- 't'

# Initial richness------------------------------------------------------------

# species data on hybas12
if(file.exists('proc/hybas12_fish_w_area.rds')){
  sp_data <- readRDS('proc/hybas12_fish_w_area.rds')
}else{
  sp_data <- readRDS('data/hybas12_fish.rds')
  sp_data <- merge(sp_data,as.data.frame(hb_data[,c("HYBAS_ID","MAIN_BAS","SUB_AREA","MAIN_BAS_AREA")])[,-5],by="HYBAS_ID")
  saveRDS(sp_data,'proc/hybas12_fish_w_area.rds')
}

### load Fishbase metadta
fishbase <- read.csv('proc/iucn_fishbase.csv')
fishbase <- fishbase[!duplicated(fishbase$iucn_name),]
levels(fishbase$AnaCat) <- c(NA,rep('Diad.',5),'Non.','Ocea.','Pota.')
# assign diadromous-non diadromous category
sp_data$diad <- 'f'
sp_data$diad[sp_data$binomial %in% fishbase$iucn_name[fishbase$AnaCat == 'Diad.']] <- 't'

### HYBAS_ID #################################################

# split by continents
sp_data$cont <- floor(sp_data$MAIN_BAS/1000000000)


# compute species richness per HYBAS_ID
if(file.exists('proc/SR_per_HYBAS_ID.rds')){
  tabulated <- readRDS('proc/SR_per_HYBAS_ID.rds')
}else{
  tabulated <- 
    do.call('rbind',
            parallel::mclapply(
              split(sp_data,sp_data$cont),
              function(y){
                do.call('rbind',
                        lapply(
                          split(y,y$HYBAS_ID),
                          function(x){
                            data.frame(
                              HYBAS_ID = rep(unique(as.character(x$HYBAS_ID)),3),
                              sr = c(length(unique(as.character(x$binomial))),
                                     length(unique(as.character(x$binomial[x$diad != 't']))),
                                     length(unique(as.character(x$binomial[x$diad == 't'])))),
                              category = c('Total','Potamodromous','Diadromous')
                            )
                          })
                )},mc.cores = 9))
  
  saveRDS(tabulated,'proc/SR_per_HYBAS_ID.rds')
  
}

hybas <- merge(hb_data[,"HYBAS_ID"],tabulated,by='HYBAS_ID')

hybas_sub <- hybas[hybas$category != 'Total',]
hybas_sub$category <- factor(hybas_sub$category)

hybas_sub$category <- factor(hybas_sub$category,levels=c('Diadromous','Potamodromous'))
levels(hybas_sub$category) <- c('Diadromous','Non-diadromous')

hybas_sub <- st_crop(hybas_sub,xmin = -180,xmax = 180,ymin = -90,ymax = 90)



# CI ------------------------------------------------------------

#> DATA


# filter species here




# computed CI for current and future for each species-basin (data.frame)
CI_tab <- rbind(
  readRDS('proc/CI_tab_global.rds')
  ,readRDS('proc/CI_tab_global_min10k.rds')
) %>%
  filter(alpha == 0.55) %>%
  as_tibble() %>%
  mutate_each(as.numeric, starts_with("connectivity")) %>%
  mutate_each(as.numeric, starts_with("patches"))

# filter for min occurrence records here <<<<<<<<<<<

# determine species to exclude because already in iucn database
# load IUCN data
iucn <- rbind(read_sf(file_iucn_fish1),read_sf(file_iucn_fish2))
# load synonyms table
synonyms_table <- vroom(list.files(dir_synonyms_table,full.names = T),delim=',') %>%
  filter(name_iucn %in% iucn$binomial)
# exclude names in iucn data and names in fishbase that are synonyms for iucn
names_to_exclude <- unique(c(iucn$binomial,synonyms_table$name_src))

# read specie table with occurrences
occ <- read_sf(file_custom_ranges) %>%
  filter(!name %in% names_to_exclude) # filter out species already covered in the IUCN dataset




comparetab <- list()
g = 1
for(oth in c(1,5,10,20,30,50,10**9)){
  
  names_out <- occ$name[occ$no_occ < oth]
  
  # compute stats across each watershed
  tab <- CI_tab %>%
    filter(!binomial %in% names_out) %>%
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
  
  #<<<< need to undesrand why I lose so many species compared to original
  
  write.csv(tab,paste0('tabs/species_ci_oth',oth,'.csv'),row.names = F)
  
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

# compute stats across each watershed
tab <- CI_tab %>%
  filter(binomial %in% unique(as.character(iucn$binomial))) %>%
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

#<<<< need to undesrand why I lose so many species compared to original

write.csv(tab,paste0('tabs/species_ci_iucnOnly.csv'),row.names = F)

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

write.csv(summary,paste0('tabs/species_ci_summary_iucnOnly.csv'),row.names = F)


# compare to old results-----------------------------------------------------
# computed CI for current and future for each species-basin (data.frame)
CI_tab <- rbind(
  readRDS('../data/connectfish/CI_tab_global.rds')
  ,readRDS('../data/connectfish/CI_tab_global_min10k.rds')
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


