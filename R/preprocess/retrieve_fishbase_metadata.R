# DEPRECATED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

source('R/MASTER.R')
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< need to check this script
Sys.setenv(FISHBASE_VERSION="17.07")

# read the iucn fish data
iucn <- rbind(foreign::read.dbf(paste0(dir_iucn_fish,'/FW_FISH_PART_1.dbf')),
              foreign::read.dbf(paste0(dir_iucn_fish,'/FW_FISH_PART_2.dbf')))

# read unique sp names
iucn_names <- as.character(unique(iucn$binomial))

# need to check why it was necessary to split this
# maybe to run the lapply on validate_names
ind <- cut(1:length(iucn_names),20,labels = F)

data <- foreach(i = 1:max(ind),.combine = 'rbind') %do% { 
  
  iucn_names_sel <- iucn_names[ind==i]
  
  # validate names for fishbase.org
  fishbase_names <- do.call('rbind',lapply(iucn_names_sel,function(x) validate_names(x)[1]))
  
  dat <- data.frame(iucn_name = iucn_names_sel,sciname = fishbase_names[,1])
  dat <- dat[complete.cases(dat),]
  
  # retrieve migratory category AnaCat from fishbase.org
  args_species <- species(dat$sciname, limit=nrow(dat))
  Sys.sleep(2)
  args_ecology <- do.call('rbind',lapply(dat$sciname,function(x) ecology(x)[1,]))
  
  args <- merge(args_species,args_ecology,by = 'Species',all.x = T)
  
  return(merge(dat,args,by.x='sciname',by.y = 'Species',all.x=T,sort=F))
  
}

write.csv(data,'proc/iucn_fishbase.csv',row.names = F)

