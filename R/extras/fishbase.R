# checking..
library(rfishbase)
Sys.setenv(FISHBASE_VERSION="17.07")

fishbase <- read.csv('D:/fishsuit/data/iucn_fishbase.csv')
fishbase <- fishbase[!duplicated(fishbase$iucn_name),]
fishbase$AnaCat[fishbase$AnaCat == ' '] <- NA

table(fishbase$AnaCat)

s <- species(as.character(fishbase$sciname),fields = 'AnaCat')
sum(s$AnaCat == fishbase$AnaCat,na.rm = T)
sum(!is.na(s$AnaCat))
sum(!is.na(fishbase$AnaCat))

f <- fishbase
levels(f$AnaCat) <- c(NA,rep('Diad.',5),'Non.','Ocea.','Pota.')
table(f$AnaCat)


# first run tab and reclass from script of FIgure 4 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
td <- droplevels(tab[tab$marine == 't',])
table(td$AnaCat)/2 # double factor for CI 'variable'
sum(is.na(td$AnaCat))/2

tp <- droplevels(tab[tab$marine == 'f',])
table(tp$AnaCat)/2
sum(is.na(tp$AnaCat))/2

td$AnaCat <- as.character(td$AnaCat)
td$AnaCat[is.na(td$AnaCat)] <- 'NC'
td$AnaCat <- factor(td$AnaCat)
levels(td$AnaCat) <- paste0(levels(td$AnaCat),'\n (',as.numeric(table(td$AnaCat)/2),')')
levels(td$variable) <- c('Present','Future')

tp$AnaCat <- as.character(tp$AnaCat)
tp$AnaCat[is.na(tp$AnaCat)] <- 'NC'
tp$AnaCat <- factor(tp$AnaCat)
levels(tp$AnaCat) <- paste0(levels(tp$AnaCat),'\n (',as.numeric(table(tp$AnaCat)/2),')')
levels(tp$variable) <- c('Present','Future')

td$cat <- 'Diadromous'
tp$cat <- 'Non-diadromous'
t <- rbind(td,tp)

library(ggplot2)
ggplot() +
  geom_boxplot(data = t,aes(x = AnaCat, y = value, fill = variable)) +
  ylab('CI [%]') +
  xlab('Migratory category (Fishbase)') +
  facet_grid(.~cat,scales = 'free_x') +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'top',
        legend.direction = 'horizontal')
ggsave('figs/Figure_SI_fishbase_comparison.jpg',width = 180,height = 120,dpi = 600,units='mm')

