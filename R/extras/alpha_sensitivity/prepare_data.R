source('R/MASTER.R')

library(sf); library(data.table); library(ggplot2); library(viridis); library(foreach); library(spatstat)

global_tab <- rbind(readRDS('proc/CI_tab_global.rds'),
                    readRDS('proc/CI_tab_global_min10k.rds'))

global_tab$cont <- floor(global_tab$MAIN_BAS/1000000000)

sp_data_spl <- split(global_tab,global_tab$cont)

for(i in 1:9){
  
  saveRDS(sp_data_spl[[i]],paste0(dir_('proc/alpha_sensitivity/'),'sp_data_w_CI_by_cont_',i,'.rds'))
  
}

system('cd batch && sbatch alpha_sensitivity_2.sh')

