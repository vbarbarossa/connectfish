source('R/MASTER.R')

global_tab <- rbind(readRDS('proc/CI_tab_global.rds'),
                    readRDS('proc/CI_tab_global_min10k.rds'))
global_tab <- global_tab[global_tab$alpha == 0.55,] # alpha exponent used for transformation from area to length

hb_data <- readRDS('proc/hybas12poly_global_w_area_main.rds')
sp_data <- readRDS('proc/hybas12_fish_w_area.rds')

sp_data <- merge(sp_data,as.data.table(global_tab),by = c('binomial','MAIN_BAS'))
sp_data$cont <- floor(sp_data$MAIN_BAS/1000000000)

sp_data_spl <- split(sp_data,sp_data$cont)

for(i in 1:9){
  
  saveRDS(sp_data_spl[[i]],paste0(dir_('proc/Figure_2_intermediate_steps/'),'sp_data_w_CI_by_cont_',i,'.rds'))
  
}


