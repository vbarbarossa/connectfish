#sbatch --array=1-9

slurm_arrayid<-Sys.getenv("SLURM_ARRAY_TASK_ID")
nodenr<-as.numeric(slurm_arrayid)

for(g in nodenr:nodenr){
  
  source('R/MASTER.R')
  
  library(sf); library(data.table); library(foreach)
  
  global_tab <- as.data.table(readRDS(paste0('proc/alpha_sensitivity/sp_data_w_CI_by_cont_',g,'.rds')))
  
  library(data.table)
  global_tab <- as.data.table(droplevels(global_tab[global_tab$alpha != 1,])) # exlude alpha=1 => only 0.5-0.6 range
  global_tab$col <- paste(global_tab$MAIN_BAS,global_tab$binomial,sep='_')
  
  tab_cur <- do.call('rbind',
                     lapply(split(global_tab,global_tab$col),
                            function(x) data.frame(binomial = x$binomial[1],
                                                   MAIN_BAS = x$MAIN_BAS[1],
                                                   col = x$col[1],
                                                   patches.no = x$patches.no[1],
                                                   patches.cum.area = x$patches.cum.area[1],
                                                   range = max(x$connectivity.cur) - min(x$connectivity.cur),
                                                   mean = mean(x$connectivity.cur),
                                                   max = max(x$connectivity.cur),
                                                   min = min(x$connectivity.cur),
                                                   category = x$category[1],
                                                   type = 'Present'
                            )
                     )
  )
  
  tab_fut <- do.call('rbind',
                     lapply(split(global_tab,global_tab$col),
                            function(x) data.frame(binomial = x$binomial[1],
                                                   MAIN_BAS = x$MAIN_BAS[1],
                                                   col = x$col[1],
                                                   patches.no = x$patches.no[1],
                                                   patches.cum.area = x$patches.cum.area[1],
                                                   range = max(x$connectivity.fut) - min(x$connectivity.fut),
                                                   mean = mean(x$connectivity.fut),
                                                   max = max(x$connectivity.fut),
                                                   min = min(x$connectivity.fut),
                                                   category = x$category[1],
                                                   type = 'Future'
                            )
                     )
  )
  
  # 
  tab <- rbind(tab_cur,tab_fut)
  
  tab$category <- factor(tab$category, levels = c('diadromous','potamodromous'))
  levels(tab$category) <- c('Diadromous','Non-diadromous')
  
  saveRDS(tab,paste0('proc/alpha_sensitivity/CI_stats_HYBAS_ID_',g,'.rds'))
  
}
