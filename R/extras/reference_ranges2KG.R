source('R/MASTER.R')

library(raster)
# read raster layer with KG zones at 1km
ras <- raster(file_KG_ras)

# reclassify the raster to simple capital letters zones (5)
m <- matrix(
  c(0,3,1,
    3,7,2,
    8,16,3,
    17,28,4,
    29,30,5,
    31,Inf,NA)
  ,ncol = 3,byrow = T)

ras_r <- reclassify(ras,m,include.lowest=T)

writeRaster(ras_r,'proc/KG_reclass.tif')

# ras[ras > 30] <- NA
# ras[ras %in% 2:3] <- 1
# ras[ras %in% 4:7] <- 2
# ras[ras %in% 8:16] <- 3
# ras[ras %in% 17:28] <- 4
# ras[ras %in% 29:30] <- 5

# convert raster to polygon
system(
  'gdal_polygonize '
)