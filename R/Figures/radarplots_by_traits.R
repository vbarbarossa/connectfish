source('R/MASTER.R')

# Library
library(fmsb); library(grid); library(RColorBrewer); library(rredlist)

#------------------------------------------------------------
#> DATA

#iucn data for threat status**************************************************
token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'

iucn_code <- foreach(ts = c("DD", "LC", "NT", "VU", "EN","CR", "EW", "EX", "LRlc", "LRnt", "LRcd"),.combine = 'rbind') %do%{
  t <- rl_sp_category(ts,key = token)$result %>% 
    as_tibble() %>%
    mutate(code = ts) %>%
    select(binomial = scientific_name,code)
  return(t)
} %>% arrange(binomial)


# CI data from this study*****************************************************
tab <- read.csv('tabs/species_ci/species_ci_oth10.csv') %>%
  as_tibble() %>%
  #merge with iucn threat status
  left_join(.,iucn_code,by='binomial')
tab$code[is.na(tab$code)] <- 'DD' # 2,597 probably from new data that do not have a match in IUCN
tab$code[tab$code %in% c('EX','EW')] <- 'DD' # there are supposedly still 4 extinct species
tab$code[tab$code %in% c('LRlc')] <- 'LC'
tab$code[tab$code %in% c('LRnt')] <- 'NT'

tab$code <- factor(tab$code, levels = c('DD','NT','LC','VU','EN','CR'))

# fishbase metadata***********************************************************
# taxonomy
tax <- rfishbase::fishbase %>% as_tibble()
tax$binomial <-apply(tax %>% select(Genus,Species),1,function(x) paste(x,collapse=' '))

# length and migratory
fishbase <- taxonomy() %>% # get all species available (vector)
  species(.) %>% # get species table for all species
  rename(binomial = Species) %>%
  mutate(AnaCat = as.factor(AnaCat)) %>%
  mutate(Importance = as.factor(Importance))

# refactor
levels(fishbase$AnaCat) <- c(rep('Diad.',6),'Non.','Ocea.','Ocea.','Pota.','Pota.')
levels(fishbase$Importance) <- c('Com.','HCom.','MCom.','NoInt.','NoInt.','Subs.')

# do the merging based on synonyms
iucn_ref <- vroom('proc/hybas12_fish.csv',delim=',') %>%
  select(binomial) %>% distinct()
# load synonyms table
syn <- vroom(list.files(dir_synonyms_table,full.names = T),delim=',') %>%
  filter(name_iucn %in% iucn_ref$binomial) %>% 
  select(name_iucn, name_src) %>% 
  distinct() %>%
  filter(name_iucn != name_src) %>%
  distinct(name_iucn,.keep_all=T) 

tab_syn <- inner_join(tab,syn,c('binomial' = 'name_iucn')) 
tab <- tab %>%
  filter(!binomial %in% tab_syn$binomial) %>%
  mutate(name_src = binomial) %>%
  bind_rows(.,tab_syn) %>%
  select(-binomial) %>%
  mutate(binomial = name_src) %>%
  inner_join(.,tax,by='binomial') %>%
  inner_join(.,fishbase,by = 'binomial')


# climatic zones*************************************************************
hydro <- read.csv('proc/species_on_KG_climate_zones.csv')

tab <- tab %>%
  left_join(.,hydro,by='binomial') %>%
  select(binomial,ci_currentW,ci_futureW,area = area_total,code,order_ = Order,
         AnaCat,Length,Importance,kg_main = KG) %>%
  mutate(order_ = as.factor(order_)) %>%
  mutate(kg_main = as.factor(kg_main))

#---------------------------------------------
#> CATEGORIES ADJUSTMENT

# area category
tab$rangesizecat <- '<4' #100 cells
tab$rangesizecat[tab$area >= 10**4] <- '4-4.5'
tab$rangesizecat[tab$area >= 10**4.5] <- '4.5-5'
tab$rangesizecat[tab$area >= 10**5] <- '5-6'
tab$rangesizecat[tab$area >= 10**6] <- '>6'
tab$rangesizecat <- factor(tab$rangesizecat,levels = c('>6','5-6','4.5-5','4-4.5','<4'))


#save complete list of names for SI
#record level now
t4n <- tab
t4n$order_orig <- t4n$order_
nchar_names <- 5
min_sample_size <- 20
# levels(t4n$order_) <- paste0(as.character(sapply(levels(t4n$order_),function(x) strsplit(x, paste0("(?<=.{",nchar_names,"})"), perl = TRUE)[[1]][1])),'.')
minor_orders <- names(table(t4n$order_)[which(table(t4n$order_) <= min_sample_size)])
levels(t4n$order_)[levels(t4n$order_) %in% minor_orders] <- 'Other'
which(!levels(t4n$order_orig) %in% levels(t4n$order_))
dn <- data.frame(name = levels(t4n$order_orig))
dn$abbr <- as.character(dn$name)
dn$abbr <- paste0(as.character(sapply(dn$abbr,function(x) strsplit(x, paste0("(?<=.{",nchar_names,"})"), perl = TRUE)[[1]][1])),'.')
dn$abbr[which(!levels(t4n$order_orig) %in% levels(t4n$order_))] <- 'Other'
write.csv(dn,'tabs/list_of_order_names_for_figure4.csv',row.names = F)

# abbreviate names for order_
nchar_names <- 7
min_sample_size <- 40
levels(tab$order_)[levels(tab$order_) %in% c("Cypriniformes","Cyprinodontiformes")] <- paste0(as.character(sapply(levels(tab$order_)[levels(tab$order_) %in% c("Cypriniformes","Cyprinodontiformes")],function(x) strsplit(x, paste0("(?<=.{",nchar_names,"})"), perl = TRUE)[[1]][1])),'.')
nchar_names <- 5
levels(tab$order_)[!levels(tab$order_) %in% c("Cyprini.","Cyprino.")] <- paste0(as.character(sapply(levels(tab$order_)[!levels(tab$order_) %in% c("Cyprini.","Cyprino.")],function(x) strsplit(x, paste0("(?<=.{",nchar_names,"})"), perl = TRUE)[[1]][1])),'.')
minor_orders <- names(table(tab$order_)[which(table(tab$order_) <= min_sample_size)])
levels(tab$order_)[levels(tab$order_) %in% minor_orders] <- 'Other'

# koppen-geiger climatic zoning---
# assign names to levels
#A: Equatorial
#B: Arid
#C: Warm-temperate
#D: Snow
#E: Polar
levels(tab$kg_main) <- LETTERS[1:5]
tab$kg_main <- factor(tab$kg_main,levels = rev(LETTERS[1:5]))

#Body Length
tab$lengthcat <- NA
tab$lengthcat[!is.na(tab$Length)] <- '<5'
tab$lengthcat[tab$Length >= 5 & !is.na(tab$Length)] <- '5-10'
tab$lengthcat[tab$Length >= 10 & !is.na(tab$Length)] <- '10-30'
tab$lengthcat[tab$Length >= 30 & !is.na(tab$Length)] <- '30-70'
tab$lengthcat[tab$Length >= 70 & !is.na(tab$Length)] <- '70-100'
tab$lengthcat[tab$Length >= 100 & !is.na(tab$Length)] <- '>=100'

tab$lengthcat <- factor(tab$lengthcat,levels = c('>=100','70-100','30-70','10-30','5-10','<5'))
table(tab$lengthcat)

tab <- droplevels(tab)

# reorder IUCN categories
#---------------------------------------------


#> RADAR PLOTS
library(ggplot2); library(grid); library(ggplotify); library(RColorBrewer)

# reshape table
tab <- tab %>%
  reshape2::melt(.,measure.vars = c('ci_currentW','ci_futureW')) %>%
  as_tibble() %>%
  filter(!is.na(value))
levels(tab$variable) <- c('Current CI','Future CI')

# catv <- c('kg_main','rangesizecat','lengthcat','code','Importance','order_')
# nchar_namesv <- c(rep(NA,5),4)
# tit = NULL
catv=c('kg_main','rangesizecat','lengthcat','code','Importance','order_')
size_namesv = c(rep(0.8,5),0.6)
tit=NULL

# try a few colors
library(viridis)
barplot(rep(5,9),col = viridis(n = 9,direction = -1,alpha = 0.7,option = 'A'))
barplot(rep(5,9),col = viridis(n = 9,direction = -1,alpha = 0.7,option = 'B'))
barplot(rep(5,2),col = viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(5,15)])
barplot(rep(5,9),col = viridis(n = 9,direction = -1,alpha = 0.7,option = 'D'))
barplot(rep(5,9),col = RColorBrewer::brewer.pal(9,'YlGnBu'))
barplot(rep(5,9),col = RColorBrewer::brewer.pal(9,'YlOrRd'))
barplot(rep(5,9),col = RColorBrewer::brewer.pal(9,'BuPu'))

# create a list of favourite binomial colors
two_stage_col_list <- list(
  viridis(n = 9,direction = -1,alpha = 0.7,option = 'C')[c(3,6)],
  viridis(n = 9,direction = -1,alpha = 0.7,option = 'C')[c(2,7)],
  viridis(n = 9,direction = -1,alpha = 0.7,option = 'C')[c(8,4)],
  viridis(n = 9,direction = -1,alpha = 0.7,option = 'C')[c(8,3)],
  RColorBrewer::brewer.pal(9,'YlGnBu')[c(7,3)],
  RColorBrewer::brewer.pal(9,'YlGnBu')[c(3,7)],
  RColorBrewer::brewer.pal(9,'YlGnBu')[c(5,8)],
  RColorBrewer::brewer.pal(9,'YlGnBu')[c(8,5)],
  RColorBrewer::brewer.pal(9,'YlOrRd')[c(3,7)],
  RColorBrewer::brewer.pal(9,'YlOrRd')[c(7,3)],
  RColorBrewer::brewer.pal(9,'BuPu')[c(3,8)],
  RColorBrewer::brewer.pal(9,'BuPu')[c(8,3)],
  viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(5,10)],
  viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(10,5)],
  viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(5,15)],
  viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(15,5)],
  viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(5,20)],
  viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(20,5)]
)


figures <- foreach(i = seq_along(two_stage_col_list)) %do% {
  
  two_stage_col <- two_stage_col_list[[i]]
  radplots <- foreach(i = seq_along(catv)) %do% {
    
    cat <- catv[i]
    n_min <- 1 #n_minv[i]
    nchar_names <- NA #nchar_namesv[i]
    size_names <- size_namesv[i]
    
    data <- foreach(type = levels(tab$variable),.combine = 'rbind') %do% {
      t <- tab[tab$variable == type,]
      ts <- split(t,t[,cat])
      res <- do.call('cbind',lapply(ts,function(x) {x = x[!is.na(x$value),]; c(nrow(x),mean(x$value))}))
      row.names(res) <- c('n',type)
      if(type == levels(tab$variable)[1]){
        return(as.data.frame(res))
      }else{
        res2 <- t(as.data.frame(res[2,]))
        row.names(res2) <- type
        return(res2)
      }
    }
    
    data <- data[,data[1,] >= n_min]
    
    colnames(data) <- paste0(colnames(data),'\n(',as.integer(data['n',]),')')
    
    data <- rbind(rep(100,ncol(data)),rep(0,ncol(data)),data[2:nrow(data),])
    
    custom_pal <- two_stage_col
    colors_border<- custom_pal
    colors_in <- do.call('c',lapply(custom_pal,function(x) rgb(t(col2rgb(x)) ,alpha = 150,maxColorValue = 255)))
    
    
    # par(mar = c(0,0,0,0),oma = c(0,0,0,0))
    library(ggplot2); library(ggplotify)
    p <- ggplotGrob(
      as.ggplot(
        ~radarchart( data  , axistype=1 , pty=32, seg=4,
                     #custom polygon
                     pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
                     #custom the grid
                     cglty=1, cglwd=1, cglcol="black", axislabcol="black", calcex = 0.7, caxislabels=c('0%','','50%','',''), #caxislabels=paste0(seq(0,100,25),'%'),
                     #custom labels
                     vlcex=size_names,title = tit)
      ) + theme(plot.margin = unit(c(-0.5, -0.5, -2, -2), "cm"))
    )
    
    return(p)
    
  }
  
  #------------------------------------------------------------
  #> LEGEND
  p_leg <- ggplot(tab,aes(x = area,y = value,color = variable)) +
    geom_point(alpha = 1) +
    scale_color_manual(values = two_stage_col) +
    guides(colour = guide_legend(title=NULL,override.aes = list(size = 3))) +
    theme_bw() +
    theme(legend.direction = 'horizontal',
          text = element_text(size = 15))
  
  tmp <- ggplot_gtable(ggplot_build(p_leg)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  
  
  library(ggpubr)
  fig <- ggarrange(
    ggarrange(plotlist=radplots,ncol= 3, nrow = length(catv)/3, labels = letters[1:length(catv)]),
    legend,
    ncol = 1,nrow = 2,heights = c((length(catv)/3),0.1))
  
  return(fig)
}
dir_('figs/radarplots')
for(i in seq_along(figures)) ggsave(paste0('figs/radarplots/',i,'.jpg'),figures[[i]],
                                    width = 174,height = 174*0.7,units='mm',dpi = 1000,scale = 1.4)

ggsave('figs/radarplot.pdf',figures[[12]],
       width = 174,height = 174*0.7,units='mm',scale = 1.4)


# #> RADAR PLOTS MEDIAN------------------------------------------------------------------------------------------------
# 
# # catv <- c('kg_main','habtype','AnaCat','rangesizecat','lengthcat','foodtrophcat','code','Importance','order_')
# catv <- c('kg_main','rangesizecat','lengthcat','code','Importance','order_')
# # nchar_namesv <- c(rep(NA,5),4)
# tit = NULL
# 
# # n_minv=c(rep(5,5),20)
# size_namesv = c(rep(0.8,5),0.6)
# 
# radplots <- foreach(i = seq_along(catv)) %do% {
#   
#   cat <- catv[i]
#   n_min <- 1 #n_minv[i]
#   nchar_names <- NA #nchar_namesv[i]
#   size_names <- size_namesv[i]
#   
#   data <- foreach(type = levels(tab$variable),.combine = 'rbind') %do% {
#     t <- tab[tab$variable == type,]
#     ts <- split(t,t[,cat])
#     res <- do.call('cbind',lapply(ts,function(x) {x = x[!is.na(x$value),]; c(nrow(x),median(x$value))}))
#     row.names(res) <- c('n',type)
#     if(type == levels(tab$variable)[1]){
#       return(as.data.frame(res))
#     }else{
#       res2 <- t(as.data.frame(res[2,]))
#       row.names(res2) <- type
#       return(res2)
#     }
#   }
#   
#   data <- data[,data[1,] >= n_min]
#   
#   colnames(data) <- paste0(colnames(data),'\n(',as.integer(data['n',]),')')
#   
#   data <- rbind(rep(100,ncol(data)),rep(0,ncol(data)),data[2:nrow(data),])
#   
#   library(viridis)
#   two_stage_col <- viridis(n = 40,direction = -1,alpha = 0.7,option = 'C')[c(5,10)] #brewer.pal('Purples',n=9)[c(4,7)]
#   
#   custom_pal <- two_stage_col
#   colors_border<- custom_pal
#   colors_in <- do.call('c',lapply(custom_pal,function(x) rgb(t(col2rgb(x)) ,alpha = 150,maxColorValue = 255)))
#   
#   
#   # par(mar = c(0,0,0,0),oma = c(0,0,0,0))
#   library(ggplot2); library(ggplotify)
#   p <- ggplotGrob(
#     as.ggplot(
#       ~radarchart( data  , axistype=1 , pty=32, seg=4,
#                    #custom polygon
#                    pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
#                    #custom the grid
#                    cglty=1, cglwd=1, cglcol="black", axislabcol="black", calcex = 0.7, caxislabels=c('0%','','50%','',''), #caxislabels=paste0(seq(0,100,25),'%'),
#                    #custom labels
#                    vlcex=size_names,title = tit)
#     ) + theme(plot.margin = unit(c(-0.5, -0.5, -2, -2), "cm"))
#   )
#   
#   return(p)
#   
# }
# 
# 
# library(ggpubr)
# fig <- ggarrange(
#   ggarrange(plotlist=radplots,ncol= 3, nrow = length(catv)/3, labels = letters[1:length(catv)]),
#   legend,
#   ncol = 1,nrow = 2,heights = c((length(catv)/3),0.1))
# cowplot::ggsave('figs/radarplots_by_traits_median.jpg',fig,
#                 width = 220,height = (230*length(catv)/3/3),units='mm',dpi = 600,scale = 1.2,type='cairo')
# 

