library(sf); library(foreach)

# dissolve MAIN_BAS
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% {
  hybas <- st_read(paste0('F:/hydroBASINS/global_lev12/hybas_',i,'_lev12_v1c.shp'))
  s <- split(hybas,hybas$MAIN_BAS)
  res <- st_sf(data.frame(MAIN_BAS = names(s)),
        geom = st_sfc(
          do.call('rbind',lapply(s,function(x) st_union( st_buffer(x,0.00001) ) )) #core function
        ),crs = 4326)
  saveRDS(res,paste0('proc/watersheds_hybas12/',i,'.rds'))
  return(res)
}

# hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% readRDS(paste0('proc/watersheds_hybas12/',i,'.rds'))

saveRDS(hb_data,'proc/watersheds_hybas12.rds')
st_write(hb_data,'proc/watersheds_hybas12.shp')

# create a global world boundaries based on hybas12
world_boundaries <- st_sf(data.frame(ID = 'WORLD'),geom=st_sfc(st_union(hb_data)))
saveRDS(world_boundaries,'proc/world_boundaries.rds')
st_write(world_boundaries,'proc/world_boundaries.shp')
