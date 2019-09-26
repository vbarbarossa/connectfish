#sbatch --array=1-9

slurm_arrayid<-Sys.getenv("SLURM_ARRAY_TASK_ID")
nodenr<-as.numeric(slurm_arrayid)

for(g in nodenr:nodenr){
  
  source('R/MASTER.R')
  
  sp_data <- as.data.table(readRDS(paste0('proc/Figure_2_intermediate_steps/sp_data_w_CI_by_cont_',g,'.rds')))
  
  tab <- foreach(i = c('diadromous','potamodromous'),.combine = 'rbind') %do% {
    
    cat = 'Potamodromous'
    if(i == 'diadromous') cat = 'Diadromous'
    
    dat <- sp_data[sp_data$category == i,]
    
    return(
      
      do.call('rbind',lapply(
        split(dat,dat$HYBAS_ID),function(x){
          x$diff <- x$connectivity.cur - x$connectivity.fut
          data.frame(
            HYBAS_ID = x$HYBAS_ID[1],
            count = nrow(x),
            mean = c(mean(x$connectivity.cur),mean(x$connectivity.fut),mean(x$diff)),
            median = c(median(x$connectivity.cur),median(x$connectivity.fut),median(x$diff)),
            type = c('Present','Future','DELTA'),
            cat = cat
          )
        }))
      
    )
  }
  
  saveRDS(tab,paste0('proc/Figure_2_intermediate_steps/CI_stats_HYBAS_ID_',g,'.rds'))
  
}
