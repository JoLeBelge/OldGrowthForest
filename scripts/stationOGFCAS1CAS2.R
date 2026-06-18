# carte des stations contraingantes qui sont classée en CAS 2 pour la recherche OGF

require(terra)
setwd("/home/jo/Documents/OGF/out")

r.potSylvi <- rast("/home/jo/Documents/carteApt/Out/CSderives20230807/prod_b.tif")

r.NH <- rast("/home/jo/Documents/Carto/FEEW2020/NH202408.tif")

r.stationOGF <- r.potSylvi
r.stationOGF[]<- 0

