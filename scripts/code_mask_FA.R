library(terra)

r <- rast("C:/Old_Growth_Forest/raster_couchesforestimator/raster/raster/FA.tif")

mask12_01 <- ifel(r %in% c(1,2), 1, 0)
writeRaster(mask12_01,
            "C:/Old_Growth_Forest/raster_couchesforestimator/raster/raster/FA_mask12_01.tif",
            overwrite = TRUE)

plot(mask12_01, col=c("white","black"), legend=FALSE)
