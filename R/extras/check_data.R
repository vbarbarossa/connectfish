# count number of basins
hb8 <- foreign::read.dbf('data/hydrobasins/hybas_lake_lev08_v1c_w_islands.dbf')
# try original from hydrobasins
# library(foreach)
# hb8_ <- foreach(i = list.files('F:/hydroBASINS/dbf_lev8',full.names = T),.combine = 'rbind') %do% foreign::read.dbf(i)
# n=190675 (no islands)

# n
length(unique(hb8$HYBAS_ID))
# 234726

# stats
summary(hb8$SUB_AREA)
IQR(hb8$SUB_AREA)
plot(ecdf(hb8$SUB_AREA))
hist(hb8$SUB_AREA[hb8$SUB_AREA < 1000],breaks = seq(0,1000,100))

