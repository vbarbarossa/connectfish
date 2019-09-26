library(sf)
main_bas <- read_sf('data/hydrobasins/hybas8_mainbas.shp')

library(mapview)
mapview(main_bas[main_bas$Area > 10**5.5,])
