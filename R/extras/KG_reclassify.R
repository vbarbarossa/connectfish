source('R/MASTER.R')

library(raster)
# read raster layer with KG zones at 1km

# beginCluster()
ras <- raster(file_KG_ras)

# reclassify the raster to simple capital letters zones (5)
print('clamping to 1:30..')
rc <- clamp(ras,1,30)

print('reclassifying..')
# reclassification matrix
m <- matrix(
  c(1,3,51,
    4,7,52,
    8,16,53,
    17,28,54,
    29,30,55)
  ,ncol = 3,byrow = T)

ras_r <- reclassify(rc,m,include.lowest=T)

print('saving to disk..')
writeRaster(ras_r,'proc/KG_reclass.tif')

# ras[ras > 30] <- NA
# ras[ras %in% 2:3] <- 1
# ras[ras %in% 4:7] <- 2
# ras[ras %in% 8:16] <- 3
# ras[ras %in% 17:28] <- 4
# ras[ras %in% 29:30] <- 5

# convert raster to polygon
print('convert to poly..')
system(
  'gdal_polygonize.py proc/KG_reclass.tif proc/KG_reclass.gpkg'
)

