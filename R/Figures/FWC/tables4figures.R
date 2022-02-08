#Valerio Barbarossa, 29 Sep 2019
# script that produces tables used for figures

source('R/MASTER_local.R')

dir_proc <- '~/surfdrive/tmp/connectfish_fwc_proc_20220207/'

names_fut <- c(readxl::excel_sheets('data/FWC_Dams_Remain_low_med_high_v2.xlsx'),
               readxl::excel_sheets('data/FWC_Dams_Remain_additional_HE_SP_v2.xlsx'))
names_fut <- c(names_fut,paste0(names_fut,'_2050'))

oth <- 10
FFR <- FALSE

# DATA ---------------------------------------------------------------------------
# hb units no sf
hb_simple <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp')) %>% 
  as_tibble() %>% select(HYBAS_ID,MAIN_BAS,SUB_AREA)

# load fishbase metadata
# <<<<<<<<<<<<<<<<<<<<<<<<<<< always check if categories are the same
fishbase <- species(fields = c('Species','AnaCat')) %>% # get species table for all species
  rename(binomial = Species)
fishbase$AnaCat <- as.factor(fishbase$AnaCat)
levels(fishbase$AnaCat) <- c(rep('Diad.',23),'Non.','Ocea.','Ocea.','Pota.','Pota.')

sp_data_all <- bind_rows(
  # read hybas12 on IUCN
  vroom('proc/hybas12_fish.csv',delim=','),
  # read hybas12 on customRanges
  vroom(paste0('proc/hybas12_fish_custom_ranges_occth',min_no_occ,'.csv'),delim=',')
)

for(sc in names_fut){
  
  # results used to filter names from 'clean_and_check.R'
  sp_reference <- read.csv(paste0('tabs/FWC/species_ci/species_ci_oth',oth,'_',sc,'.csv')) %>%
    as_tibble()
  
  # CI data
  
  CI_tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    readRDS(paste0(dir_proc,'CI_tab_global_',cont,'_',sc,'.rds'))} %>%
    filter(alpha == 0.55) %>%
    as_tibble() %>%
    mutate_each(as.numeric, starts_with("connectivity")) %>%
    mutate_each(as.numeric, starts_with("patches")) %>%
    filter(binomial %in% as.character(sp_reference$binomial))
  
  # species on HB
  sp_data <- sp_data_all %>%
    filter(binomial %in% as.character(sp_reference$binomial)) %>%
    inner_join(.,hb_simple, by = 'HYBAS_ID') %>%
    inner_join(.,CI_tab,by = c('binomial','MAIN_BAS'))
  
  sp_data$diad <- 'f'
  sp_data$diad[sp_data$binomial %in% fishbase$binomial[fishbase$AnaCat == 'Diad.']] <- 't'
  
  # summarize no. species and CI per HYBAS_ID
  HB_summary <- foreach(tp = c('t','f'),.combine='rbind') %do%{
    sp_data %>%
      filter(diad == tp) %>%
      group_by(HYBAS_ID) %>%
      summarize(
        sr = n(),
        Present = mean(connectivity.cur,na.rm=T),
        Future = mean(connectivity.fut,na.rm=T),
        cat = tp
      )
  } %>%
    mutate(Difference = Present - Future) %>%
    reshape2::melt(.,measure.vars = c('Present','Future','Difference'),variable.name = 'CI') %>%
    mutate(cat = forcats::fct_recode(cat, 'Diadromous' = 't','Non diadromous' = 'f')) %>%
    as_tibble()
  
  saveRDS(HB_summary,paste0('proc/CI_HB_',sc,'.rds'))
  
  BAS_summary <- foreach(tp = c('diadromous','potamodromous'),.combine='rbind') %do%{
    CI_tab %>%
      filter(category == tp) %>%
      group_by(MAIN_BAS) %>%
      summarize(
        sr = n(),
        Present = sum(connectivity.cur*patches.cum.area,na.rm=T)/sum(patches.cum.area,na.rm=T),
        Future = sum(connectivity.fut*patches.cum.area,na.rm=T)/sum(patches.cum.area,na.rm=T),
        cat = tp
      )
  } %>%
    mutate(Difference = Present - Future) %>%
    reshape2::melt(.,measure.vars = c('Present','Future','Difference'),variable.name = 'CI') %>%
    mutate(cat = forcats::fct_recode(cat, 'Diadromous' = 'diadromous','Non diadromous' = 'potamodromous')) %>%
    as_tibble()
  
  saveRDS(BAS_summary,paste0('proc/CI_BAS_',sc,'.rds'))
}

