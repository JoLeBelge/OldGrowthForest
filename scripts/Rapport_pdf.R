library(sf)
library(dplyr)
library(curl)
library(magick)

gpkg_path <- "C:/Old_Growth_Forest/DATA/terrain_v7.gpkg"
output_dir <- "C:\\Old_Growth_Forest\\output_2pdf"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

server.api <- "https://forestimator.gembloux.ulg.ac.be/api/"

ogf <- st_read(gpkg_path, layer = "ogf", quiet = TRUE)
centres <- st_read(gpkg_path, layer = "centre_placette", quiet = TRUE)
centres <- st_zm(centres, drop = TRUE, what = "ZM")

centres_l72 <- st_transform(centres, st_crs(ogf))
buffers_30m <- st_buffer(centres_l72, 30)

ids_ok <- centres_l72 %>%
  st_drop_geometry() %>%
  distinct(id_ogf) %>%
  pull(id_ogf)

ogf <- ogf %>% filter(id %in% ids_ok)

download_ortho <- function(pol, outfile) {
  wkt <- st_as_text(st_geometry(pol)[[1]])
  url <- paste0(
    server.api,
    "staticMap/layerCode/ortho2023/sz/1000/polygon/",
    URLencode(wkt, reserved=TRUE)
  )
  curl_fetch_disk(url, outfile)
}

for (i in seq_len(nrow(ogf))) {
  
  pol <- ogf[i, ]
  
  ortho_img <- file.path(output_dir, paste0("pol_", pol$id, "_ortho2023.png"))
  out_img   <- file.path(output_dir, paste0("pol_", pol$id, "_overlay.png"))
  
  cat("\nParcelle", pol$id, "\n")
  
  download_ortho(pol, ortho_img)
  
  centres_i <- centres_l72 %>% filter(id_ogf == pol$id)
  buffers_i <- buffers_30m %>% filter(centres_l72$id_ogf == pol$id)
  
  img <- image_read(ortho_img)
  
  bb <- st_bbox(pol)
  
  to_px <- function(x, y) {
    px <- (x - bb["xmin"]) / (bb["xmax"] - bb["xmin"]) * 1000
    py <- 1000 - (y - bb["ymin"]) / (bb["ymax"] - bb["ymin"]) * 1000
    cbind(px, py)
  }
  
  # ouvrir un device isolé pour dessiner
  dev <- image_draw(img)
  
  plot(0:1000, 0:1000, type="n", asp=1, axes=FALSE, xlab="", ylab="")
  
  if (nrow(buffers_i) > 0) {
    bxy <- st_coordinates(st_cast(buffers_i, "MULTILINESTRING"))
    bpx <- to_px(bxy[,1], bxy[,2])
    lines(bpx[,1], bpx[,2], col="dodgerblue3", lwd=3)
  }
  
  pxy <- st_coordinates(st_cast(pol, "MULTILINESTRING"))
  ppx <- to_px(pxy[,1], pxy[,2])
  lines(ppx[,1], ppx[,2], col="#ff00ff", lwd=4)
  
  if (nrow(centres_i) > 0) {
    cxy <- st_coordinates(centres_i)
    cpx <- to_px(cxy[,1], cxy[,2])
    points(cpx[,1], cpx[,2], pch=16, col="red3", cex=1.2)
  }
  
  dev.off()
  
  image_write(img, out_img)
  
  cat("✔ Image créée:", out_img, "\n")
}
