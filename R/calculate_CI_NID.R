source('R/MASTER.R')

# HydroBASINS data ------------------------------------------------------------------------------------------
# read hydrobasins data
hb_data <- foreach(i = c('na'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_',i,'_lev12_v1c.shp'))
# add basin area
# main_bas_area <- do.call('rbind',lapply(split(hb_data_frame,hb_data_frame$MAIN_BAS),function(x) data.frame(MAIN_BAS = unique(x$MAIN_BAS),MAIN_BAS_AREA = sum(x$SUB_AREA))))
cat('\nCompiling main basin area..')

main_bas_area <- hb_data %>%
  as_tibble() %>%
  select(HYBAS_ID,MAIN_BAS,SUB_AREA) %>%
  group_by(MAIN_BAS) %>%
  summarize(MAIN_BAS_AREA = sum(SUB_AREA))

hb_data <- inner_join(hb_data,main_bas_area,by='MAIN_BAS')


# select diadromous and non-diadromous species------------------------------------------------------------

cat('\nRetrieving diadromous species from fishbase..')

# load fishbase metadata
fishbase <- taxonomy() %>% # get all species available (vector)
  species(.,fields = c('Species','AnaCat')) %>% # get species table for all species
  rename(binomial = Species)

fishbase$AnaCat <- as.factor(fishbase$AnaCat) 
levels(fishbase$AnaCat) <- c(rep('Diad.',6),'Non.','Ocea.','Ocea.','Pota.','Pota.')
# table(fishbase$AnaCat)

# Species rage data --------------------------------------------------------------------------------------

cat('\nReading species data..')

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

# Dams data ---------------------------------------------------------------------------------------------
dams_NID <- readRDS('proc/dams_NID_hydrobasins.rds')

# FUNCTION TO FIND UPSTREAM IDs -------------------------------------------------------------------------

# new column for master_table with concatenated next downstream ID
next_down <- function(tab){
  tb <- tab[,(ncol(tab)-1):ncol(tab)]
  colnames(tb) <- c('id','nd')
  d1 <- rep(0,nrow(tb))
  for(i in 1:nrow(tb)){
    if(tb$nd[i] != 0 & tb$nd[i] %in% tb$id){
      d1[i] <- unique(tb$nd[tb$id == tb$nd[i]])
    }else{
      d1[i] <- 0
    }
  }
  return(d1)
  
}

# function that takes in input the master table and selects the unique upstream IDs
upstream_sel <- function(t,id){
  return(
    unique(
      do.call('c',as.list(apply(t,1,function(x){
        sel <- which(x == id)
        if(length(sel) != 0) return(as.vector(x[1:sel]))
      })))
    )
  )
}

find_upstream_ids <- function(t,IDs){
  
  ### concatenate next_downstream basins ids
  c = 1
  while(sum(t[,ncol(t)]) != 0){
    t[,paste0('d',c)] <- next_down(t)
    c = c+1
  }
  
  l <- lapply(IDs,function(x) upstream_sel(t,x))
  names(l) <- IDs
  return(l)
}

# FUNCTION THAT CALCULATES CI PER MAIN BASIN ------------------------------------------------------------------

# Danube 2120008490

basin_connectivity <- function(main_bas_id){
  
  sbas <- hb_data%>%
    filter(MAIN_BAS == main_bas_id)
  sbas_sp <- sp_data %>%
    filter(MAIN_BAS == main_bas_id)
  
  # select dams for the current basin
  dcur <- merge(sbas,dams_NID,by='HYBAS_ID')
  dfut <- merge(sbas,dams_NID,by='HYBAS_ID')
  
  dams_no_cur <- 0
  dams_no_fut <- 0
  
  if(sum(nrow(dcur),nrow(dfut)) > 0){
    
    # divide the basins in groups
    # store dams no first for data frame <<<<<<<<<<<
    dams_no_cur <- nrow(dcur)
    dams_no_fut <- nrow(dfut)
    
    # clean duplicates
    dcur <- dcur[!duplicated(dcur[,1:2]),]
    dfut <- rbind(dcur[,1:9],dfut[,1:9])
    dfut <- dfut[!duplicated(dfut[,1:2]),]
    
    # sort them, higher PFAFSTETTER number = more upstream
    dcur <- dcur[order(dcur$PFAF_ID.x,decreasing = T),]
    dfut <- dfut[order(dfut$PFAF_ID.x,decreasing = T),]
    
    # find upstrea IDs of each dam
    upstream_list <- find_upstream_ids(t=as.data.frame(sbas[,c('HYBAS_ID','NEXT_DOWN')])[,-3], #removed the geom column
                                       IDs=dfut$HYBAS_ID) # dfut has both cur and fut
    # create ID groups from most upstream to downstream
    # exclude IDs in downstream groups already present in upstream groups
    if(nrow(dcur) > 0){
      groups_cur <- list()
      
      for(i in 1:nrow(dcur)){
        # store the hybas_id of the upstream basins
        groups_cur[[i]] <- upstream_list[[as.character(dcur$HYBAS_ID[i])]]
        # need to exclude basins that are in the upstream groups
        if(i > 1) groups_cur[[i]] <- groups_cur[[i]][!groups_cur[[i]] %in% do.call('c',groups_cur[1:(i-1)])]
      }
      names(groups_cur) <- dcur$HYBAS_ID
      groups_cur <- groups_cur[sapply(groups_cur,function(x) length(x) != 0)]
      
    }else{
      groups_cur <- list('0000000000' = '0000000000')
    }
    
    groups_fut <- list()
    
    for(i in 1:nrow(dfut)){
      
      # store the hybas_id of the upstream basins
      groups_fut[[i]] <- upstream_list[[as.character(dfut$HYBAS_ID[i])]]
      # need to exclude basins that are in the upstream groups
      if(i > 1) groups_fut[[i]] <- groups_fut[[i]][!groups_fut[[i]] %in% do.call('c',groups_fut[1:(i-1)])]
      
    }
    names(groups_fut) <- dfut$HYBAS_ID
    groups_fut <- groups_fut[sapply(groups_fut,function(x) length(x) != 0)]
    
  }else{
    groups_cur <- groups_fut <- list('0000000000' = '0000000000')
  }
  
  # function that allocates the row to a different group based on hybas_id
  # if no group is found, returns 0
  assign_group <- function(hybas_id,group){
    a <- 0
    for(j in 1:length(group)){
      if(hybas_id %in% group[[j]]) a <- j
    }
    return(a) 
  }
  
  tab <- foreach(sp = unique(as.character(sbas_sp$binomial)),.combine = 'rbind') %do% {
    
    occ <- sbas_sp[sbas_sp$binomial == sp,]
    occ$group_cur <- sapply(occ$HYBAS_ID,function(x) assign_group(x,groups_cur))
    occ$group_fut <- sapply(occ$HYBAS_ID,function(x) assign_group(x,groups_fut))
    
    foreach(alpha = c(0.55),.combine = 'rbind') %do% {
      
      L <- occ$SUB_AREA**alpha
      
      if(occ$diad[1] == 't'){
        # total area
        A = sum(L)
        sa_cur = sum(L[occ$group_cur == min(occ$group_cur)])
        sa_fut = sum(L[occ$group_fut == min(occ$group_fut)])
        cat <- 'diadromous'
      }else{
        # total area
        A = sum(L)**2
        # sum pf areas for patches from different groups
        sa_cur = 0
        sa_fut = 0
        for(i in unique(occ$group_cur)) sa_cur = sa_cur + sum(L[occ$group_cur == i])**2
        for(i in unique(occ$group_fut)) sa_fut = sa_fut + sum(L[occ$group_fut == i])**2
        cat <- 'potamodromous'
        
      }
      
      # output
      # main basin id, species name, no.patches, sum area patches, connectivity current, connectivity future
      
      data.frame(
        MAIN_BAS = unique(occ$MAIN_BAS),
        MAIN_BAS_AREA = occ$MAIN_BAS_AREA[1],
        binomial = sp,
        patches.no = nrow(occ),
        patches.cum.area = sum(occ$SUB_AREA),
        dams.cur.no = dams_no_cur,
        dams.fut.no = dams_no_fut,
        dams.cur.no.bas = nrow(dcur),
        dams.fut.no.bas = nrow(dfut) - nrow(dcur),
        category = cat,
        connectivity.cur = (sa_cur/A)*100,
        connectivity.fut = (sa_fut/A)*100,
        alpha = alpha
      )
      
    }
  }
  
  return(tab)
  
  
}

# execution in parallel---------------------------------------------------------------------------------------------

cat('\nCalculating CI..\n\n')

global_tab <- do.call('rbind',
                      parallel::mclapply(
                        unique(sp_data$MAIN_BAS),
                        basin_connectivity,
                        mc.cores = NC
                      ))

warnings()

cat('\nSaving CI table..\n\n')

saveRDS(global_tab,'proc/CI_tab_NID.rds')
