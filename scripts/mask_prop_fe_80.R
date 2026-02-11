library(terra)

r <- rast("C:/Old_Growth_Forest/raster_10m/dendro_PROP_FE_10m.tif")

# 1) regarder vite fait l'échelle des valeurs (0-1 ? 0-100 ?)
mx <- global(r, "max", na.rm = TRUE)[1,1]

seuil <- if (is.finite(mx) && mx <= 1.5) 0.80 else 80  # auto: proportions vs %
cat("max =", mx, " -> seuil utilisé =", seuil, "\n")

# 2) masque binaire : 1 si >= seuil, sinon NA
mask_fe80 <- ifel(r >= seuil, 1, NA)

# (optionnel) garder un raster 0/1 au lieu de NA/1 :
# mask_fe80 <- ifel(r >= seuil, 1, 0)

# 3) export
writeRaster(
  mask_fe80,
  "C:/Old_Growth_Forest/raster_10m/dendro_PROP_FE_10m_mask_FE80.tif",
  overwrite = TRUE
)
