source('R/MASTER.R')
# RETRIEVE WATERSHED NAMES FROM TEDESCO ET AL. ###############################################################

# # sample based on tedesco basins
library(sf)
wsted <- readRDS('data/compare_SR_tedesco.rds')
library(countrycode)
wsted$continent <- countrycode(sourcevar = wsted$Country,origin = 'country.name',destination = 'continent')

#load watersheds layer, consider only biggest basins
main_bas <- readRDS('proc/watersheds_hybas12.rds')
# load hb_data to retrieve info on area
hb_data <- readRDS('proc/hybas12poly_global_w_area_main.rds')
hb_data <- hb_data[!duplicated(hb_data$MAIN_BAS),]
main_bas <- merge(main_bas,as.data.frame(hb_data[,c('MAIN_BAS','MAIN_BAS_AREA')])[,-3],by = 'MAIN_BAS')
rm(hb_data)

main_bas <- main_bas[main_bas$MAIN_BAS_AREA > 10000,] #305 biggest

centr_ <- st_point_on_surface(wsted) #centroid always within polygon
centr <- st_intersection(centr_,main_bas)

length(unique(centr$MAIN_BAS)) #593
sum(!is.na(centr$MAIN_BAS)) #671
# there are less MAIN_BAS than wsted, probably some smaller watershed were aggregated in hybasins

# filter out multiple records by selecting the largest ted watershed
library(foreach)# order to select largest watersheds
centrdf <- as.data.frame(centr)[,-ncol(centr)]
centrdf <- do.call('rbind',lapply(
  split(centrdf,centrdf$MAIN_BAS),function(x) x[which(x$Surf_area == max(x$Surf_area,na.rm=T)),]
))

main_bas <- merge(main_bas,centrdf,by='MAIN_BAS')
row.names(main_bas) <- NULL

# MERGE WITH WATERSHEDS STATS #########################################################################
CI_stats <- readRDS('proc/CI_stats_MAIN_BAS.rds') %>%
  .[.$type == 'DELTA',] %>%
  droplevels(.)

tab <- merge(CI_stats,main_bas,by = 'MAIN_BAS')

t <- tab[tab$cat == 'Potamodromous',] %>% droplevels(.) %>% 
  .[.$MAIN_BAS_AREA.x > 10000,] %>% 
  .[order(.$mean, decreasing = T),] %>%
  .[,-ncol(.)] # remove geometry col

write.csv(t,'tabs/CI_delta_watershed_maj_10kArea_pota.csv',row.names = F)

t <- tab[tab$cat == 'Diadromous',] %>% droplevels(.) %>% 
  .[.$MAIN_BAS_AREA.x > 10000,] %>%
  .[order(.$mean, decreasing = T),]%>%
  .[,-ncol(.)] # remove geometry col

write.csv(t,'tabs/CI_delta_watershed_maj_10kArea_diad.csv',row.names = F)

t <- tab[tab$cat == 'Potamodromous',] %>% droplevels(.) %>% 
  .[.$MAIN_BAS_AREA.x > 10000,] %>% .[.$no.sp > 100,] %>% 
  .[.$mean > 5,] %>%
  .[order(.$mean, decreasing = T),] %>%
  .[,-ncol(.)] # remove geometry col

write.csv(t,'tabs/CI_delta_watershed_maj_10kArea_100sp_5perc.delta_pota.csv',row.names = F)

t <- tab[tab$cat == 'Diadromous',] %>% droplevels(.) %>% 
  .[.$MAIN_BAS_AREA.x > 10000,] %>% 
  .[.$no.sp > 10,] %>%
  .[.$mean > 5,] %>%
  .[order(.$mean, decreasing = T),]%>%
  .[,-ncol(.)] # remove geometry col

write.csv(t,'tabs/CI_delta_watershed_maj_10kArea_10sp_5perc.delta_diad.csv',row.names = F)


# SELECTED
t <- tab[tab$cat == 'Potamodromous',] %>% droplevels(.) %>% 
  .[.$MAIN_BAS_AREA.x > 500000,] %>% 
  .[.$no.sp > 200,] %>% 
  .[.$mean > 10,] %>%
  .[order(.$mean, decreasing = T),] %>%
  .[,-ncol(.)] # remove geometry col

t <- tab[tab$cat == 'Diadromous',] %>% droplevels(.) %>% 
  .[.$MAIN_BAS_AREA.x > 10000,] %>% 
  .[.$no.sp > 10,] %>% 
  .[.$mean > 10,] %>%
  .[order(.$mean, decreasing = T),] %>%
  .[,-ncol(.)] # remove geometry col




