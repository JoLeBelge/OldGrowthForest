library(terra)
library(sf)
library(DBI)
library(RSQLite)
library(dplyr)

# --- inputs ---
r_hdom <- rast("C:/Old_Growth_Forest/raster_10m/dendro_hdom_10m.tif") / 100  # cm -> m
gpkg_terrain <- "C:/Old_Growth_Forest/DATA/terrain_v7.gpkg"
layer_pts <- "centre_placette"   # <- nom du layer dans ton gpkg (à adapter si besoin)
db_path <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"
# --- dossier outputs GitHub ---
out_dir <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# --- 1) points centre_placette -> buffers 30m ---
pts <- st_read(gpkg_terrain, layer = layer_pts, quiet = TRUE)

# colonnes clés attendues (adapte si le nom diffère)
stopifnot(all(c("id_ogf","id_ue") %in% names(pts)))

# reprojeter si CRS différent
if (!is.na(st_crs(pts)) && !is.na(terra::crs(r_hdom)) && st_crs(pts)$wkt != terra::crs(r_hdom)) {
  pts <- st_transform(pts, terra::crs(r_hdom))
}

buf30 <- st_buffer(pts, dist = 30)  # 30 m de rayon
buf30_v <- vect(buf30)              # terra vect pour extract

# --- 2) extraire HDOM dans chaque buffer (moyenne/mediane/n_pix) ---
h_mean <- terra::extract(r_hdom, buf30_v, fun = mean,   na.rm = TRUE, touches = TRUE)[,2]
h_med  <- terra::extract(r_hdom, buf30_v, fun = median, na.rm = TRUE, touches = TRUE)[,2]
n_pix  <- terra::extract(r_hdom, buf30_v, fun = function(x) sum(!is.na(x)), touches = TRUE)[,2]

tab_hdom_plot <- tibble(
  id_ogf = buf30$id_ogf,
  id_ue  = buf30$id_ue,
  hdom_mean_m   = h_mean,
  hdom_median_m = h_med,
  n_pix = n_pix
)

# --- 3) récupérer typologie plot (UE) depuis la DB et joindre ---
con <- dbConnect(RSQLite::SQLite(), db_path)

typo_plot <- dbReadTable(con, "dendro_plot") %>%
  transmute(
    id_ogf = ues_id_ogf,
    id_ue  = ues_id_ue,
    typologie = typologie_mature_simplifiee
  )

dbDisconnect(con)

tab_hdom_plot_typo <- tab_hdom_plot %>%
  left_join(typo_plot, by = c("id_ogf","id_ue"))

# --- 4) stats HDOM par typologie (en gardant uniquement les UE typées) ---
stats_hdom_typo <- tab_hdom_plot_typo %>%
  filter(!is.na(typologie), typologie != "", is.finite(hdom_mean_m)) %>%
  group_by(typologie) %>%
  summarise(
    n_plots = n(),
    hdom_mean_m   = mean(hdom_mean_m, na.rm = TRUE),
    hdom_median_m = median(hdom_mean_m, na.rm = TRUE),
    hdom_sd_m     = sd(hdom_mean_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_plots))

# --- exports ---
write.csv2(tab_hdom_plot_typo, file.path(out_dir, "hdom_par_parcelle_avec_typologie.csv"), row.names = FALSE)
write.csv2(stats_hdom_typo,    file.path(out_dir, "hdom_stats_par_typologie_parcelle.csv"), row.names = FALSE)


stats_hdom_typo
