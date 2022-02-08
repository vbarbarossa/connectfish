source('R/MASTER_local.R')

#------------------------------------------------------------
#> DATA
dir_ci <- valerioUtils::dir_('tabs/FWC/species_ci/')
dir_proc <- '~/surfdrive/tmp/connectfish_fwc_proc_20220207/'

names_fut <- c(readxl::excel_sheets('data/FWC_Dams_Remain_low_med_high_v2.xlsx'),
               readxl::excel_sheets('data/FWC_Dams_Remain_additional_HE_SP_v2.xlsx'))
names_fut <- c(names_fut,paste0(names_fut,'_2050'))

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

# read specie table with occurrences
occ_all <- read_sf(file_custom_ranges) %>%
  as_tibble() %>%
  select(-geom) 

for(sc in names_fut){
  # computed CI for current and future for each species-basin (data.frame)
  CI_tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    readRDS(paste0(dir_proc,'CI_tab_global_',cont,'_',sc,'.rds'))} %>%
    filter(alpha == 0.55) %>%
    as_tibble() %>%
    mutate_each(as.numeric, starts_with("connectivity")) %>%
    mutate_each(as.numeric, starts_with("patches"))
  
  
  # do the filtering
  iucnCI <- CI_tab %>%
    filter(binomial %in% iucn_names) %>%
    filter(binomial %in% habitat_iucn$binomial[habitat_iucn$lotic == 1]) #exclude only lentic
  
  customCI <- CI_tab %>%
    filter(!binomial %in% iucn_names) %>%
    filter(!binomial %in% unique(as.character(habitat_custom$name[habitat_custom$OnlyLake == -1]))) #exclude only lentic
  
  # read specie table with occurrences
  occ <- occ_all %>%
    filter(name %in% customCI$binomial) # filter out species already covered in the IUCN dataset
  
  # tables creation---------------------------------------------------------------------------------------------------
  
  # comparetab <- list()
  # g = 1
  for(oth in c(10)){
    
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
    
    write.csv(tab,paste0(dir_ci,'species_ci_oth',oth,'_',sc,'.csv'),row.names = F)
    
    if(oth == 10) tab_compare_ffr <- tab
    
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
    
    write.csv(summary,paste0(dir_ci,'species_ci_summary_oth',oth,'_',sc,'.csv'),row.names = F)
    # 
    # comparetab[[g]] <- data.frame(
    #   oth = oth,
    #   diad_n = summary$n[1],
    #   diad_ci_c = summary$ci_cur_meanW[1],
    #   diad_ci_f = summary$ci_fut_meanW[1],
    #   pota_n = summary$n[2],
    #   pota_ci_c = summary$ci_cur_meanW[2],
    #   pota_ci_f = summary$ci_fut_meanW[2]
    #   
    # )
    # 
    # g = g+1
    
  }
  
  # comparetab <- do.call('rbind',comparetab)
  
  # write.csv(comparetab,paste0('tabs/compare_CI_different_oth_',sc,'.csv'),row.names = F)
  
}

