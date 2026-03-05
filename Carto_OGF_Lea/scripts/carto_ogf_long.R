library(terra)
library(data.table)
library(openxlsx)
library(ggplot2)


# Feuillus retenus :
# - AU (mixte) conservé car PROP_FE >= 80 assure déjà une majorité feuillus
# - BO, CH, HE, PE
feuillus_compo_vals <- c(1, 2, 3, 6, 8)

min_ha_quantiles <- 20
qs    <- c("q50","q70","q75","q80","q90")
probs <- c(0.50, 0.70, 0.75, 0.80, 0.90)

# ============================================================
# CHEMINS
# ============================================================
pth_terr_eco <- "C:/Old_Growth_Forest/Carto_Lea/data_raw/Territoires_ecologiques/territoires_ecologiques_2011/territoires_ecologiques_2011.shp"
pth_hdom     <- "C:/Old_Growth_Forest/Carto_OGF/raw/dendro_hdom_10m.tif"
pth_propfe   <- "C:/Old_Growth_Forest/Carto_OGF/raw/dendro_PROP_FE_10m.tif"
pth_fa       <- "C:/Old_Growth_Forest/Carto_OGF/raw/FA_mask12_01.tif"
pth_compo    <- "C:/Old_Growth_Forest/Carto_OGF/raw/compo_all_sp10m.tif"

out_dir <- "C:/Old_Growth_Forest/Carto_OGF/outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# DICTIONNAIRE COMPO (1..9)
# ============================================================
dict_compo <- data.table(
  compo_raster = 1:9,
  code_essence = c("AU","BO","CH","DO","EP","HE","MZ","PE","PI"),
  essence = c(
    "Others (mixed)","Birches","Oaks","Douglas fir","Spruces","Beech","Larchs","Poplars","Pines"
  )
)

# ============================================================
# SWITCHES (mets 1 si tu veux recalculer l'étape)
# ============================================================
DO_ALIGN_CROP <- 0  # project/resample/crop/mask des rasters
DO_TERR_ID    <- 0  # rasterize du shp territoires écologiques
DO_MASK       <- 0  # masque FA + FE80 + feuillus
DO_PIX_TABLE  <- 0  # extraction pixels (as.data.frame)
# ============================================================

cache_dir <- file.path(out_dir, "cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# fichiers cache
pth_hdom_crop   <- file.path(cache_dir, "hdom_crop.tif")
pth_propf_crop  <- file.path(cache_dir, "propf_align_crop.tif")
pth_fa_crop     <- file.path(cache_dir, "fa_align_crop.tif")
pth_compo_crop  <- file.path(cache_dir, "compo_align_crop.tif")
pth_terr_id     <- file.path(cache_dir, "terr_eco_id.tif")
pth_mask        <- file.path(cache_dir, "mask_analysis_1NA.tif")  # 1/NA
pth_dt_pix      <- file.path(cache_dir, "dt_pix.rds")

# ============================================================
# 0) LIRE
# ============================================================
terr_eco <- vect(pth_terr_eco)

hdom_raw  <- rast(pth_hdom)     # sert de référence
propf_raw <- rast(pth_propfe)
fa_raw    <- rast(pth_fa)
compo_raw <- rast(pth_compo)

ref <- hdom_raw

# ============================================================
# 1) ALIGN + CROP (lourd) : recalcul 1x puis relire
# ============================================================
if (DO_ALIGN_CROP) {
  
  if (!same.crs(terr_eco, ref)) terr_eco <- project(terr_eco, crs(ref))
  
  # HDOM (référence) -> crop/mask uniquement
  hdom <- crop(hdom_raw, terr_eco) |> mask(terr_eco)
  
  # PROPF
  propf <- propf_raw
  if (!same.crs(propf, ref)) propf <- project(propf, crs(ref))
  propf <- resample(propf, ref, method = "bilinear")
  propf <- crop(propf, terr_eco) |> mask(terr_eco)
  
  # FA
  fa <- fa_raw
  if (!same.crs(fa, ref)) fa <- project(fa, crs(ref))
  fa <- resample(fa, ref, method = "near")
  fa <- crop(fa, terr_eco) |> mask(terr_eco)
  
  # COMPO
  compo <- compo_raw
  if (!same.crs(compo, ref)) compo <- project(compo, crs(ref))
  compo <- resample(compo, ref, method = "near")
  compo[is.nan(compo)] <- NA
  compo <- crop(compo, terr_eco) |> mask(terr_eco)
  
  writeRaster(hdom,  pth_hdom_crop,  overwrite=TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  writeRaster(propf, pth_propf_crop, overwrite=TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  writeRaster(fa,    pth_fa_crop,    overwrite=TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  writeRaster(compo, pth_compo_crop, overwrite=TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  
  cat("✅ Align+crop recalculé et sauvegardé.\n")
  
} else {
  
  # relire les sorties alignées/croppées
  hdom  <- rast(pth_hdom_crop)
  propf <- rast(pth_propf_crop)
  fa    <- rast(pth_fa_crop)
  compo <- rast(pth_compo_crop)
  
  # terr_eco doit être dans le même CRS que ref pour rasterize/crop
  if (!same.crs(terr_eco, hdom)) terr_eco <- project(terr_eco, crs(hdom))
  
  cat("↩️ Align+crop relu depuis cache.\n")
}


# ============================================================
# 2) TERR_ECO ID rasterisé (lourd) : recalcul 1x puis relire
# ============================================================
terr_fields <- setdiff(names(terr_eco), "geometry")
if (length(terr_fields) == 0) stop("Aucun champ attributaire dans le shp territoires écologiques.")
terr_field <- terr_fields[1]
cat("Champ territoire utilisé:", terr_field, "\n")

if (DO_TERR_ID) {
  
  terr_id <- rasterize(terr_eco, hdom, field = terr_field, touches = TRUE)
  terr_id <- crop(terr_id, terr_eco) |> mask(terr_eco)
  writeRaster(terr_id, pth_terr_id, overwrite=TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  
  cat("✅ terr_eco_id recalculé et sauvegardé.\n")
  
} else {
  
  terr_id <- rast(pth_terr_id)
  cat("↩️ terr_eco_id relu depuis cache.\n")
}

# ============================================================
# 3) MASQUE (FA + FE80 + feuillus) : recalcul 1x puis relire
# ============================================================
if (DO_MASK) {
  
  mx <- global(propf, "max", na.rm = TRUE)[1,1]
  thr_propfe <- if (!is.na(mx) && mx <= 1.5) 0.8 else 80
  
  mask_fa_propfe <- (fa == 1) & (propf >= thr_propfe)
  mask_feuillus  <- compo %in% feuillus_compo_vals
  
  # stocké en 1/NA
  mask_1NA <- ifel(mask_fa_propfe & mask_feuillus, 1, NA)
  writeRaster(mask_1NA, pth_mask, overwrite=TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  
  cat("✅ mask recalculé et sauvegardé.\n")
  
} else {
  
  mask_1NA <- rast(pth_mask)
  
  # seuil affichage (recalc léger)
  mx <- global(propf, "max", na.rm = TRUE)[1,1]
  thr_propfe <- if (!is.na(mx) && mx <= 1.5) 0.8 else 80
  
  cat("↩️ mask relu depuis cache.\n")
}

# export “zone” final (léger)
mask_dir <- file.path(out_dir, "masks")
dir.create(mask_dir, showWarnings = FALSE, recursive = TRUE)
out_zone_tif <- file.path(mask_dir, "ZONE_FA_PROPFE80_FEUILLUS_noResineux.tif")
writeRaster(mask_1NA, out_zone_tif, overwrite = TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
cat("✅ Zone écrite:", out_zone_tif, "\n")

mask_bool <- !is.na(mask_1NA)

# ============================================================
# 4) TABLE PIXELS filtrés (très lourd) : recalcul 1x puis relire
# ============================================================
if (DO_PIX_TABLE) {
  
  s <- c(terr_id, compo, hdom)
  names(s) <- c("terr_id", "compo", "hdom")
  
  s_m <- mask(s, mask_1NA)
  df <- as.data.frame(s_m, na.rm = TRUE)
  if (nrow(df) == 0) stop("Aucun pixel ne passe les filtres (FA, PROP_FE, FEUILLUS).")
  
  dt_pix <- as.data.table(df)
  dt_pix[, compo := as.integer(compo)]
  dt_pix[, terr_id_chr := as.character(terr_id)]
  dt_pix[, terr_num := as.integer(factor(terr_id_chr))]
  
  saveRDS(dt_pix, pth_dt_pix)
  cat("✅ dt_pix recalculé et sauvegardé.\n")
  
} else {
  
  dt_pix <- readRDS(pth_dt_pix)
  cat("↩️ dt_pix relu depuis cache.\n")
}

cat("Classes compo après filtre (doit être 1,2,3,6,8) :\n")
print(sort(unique(dt_pix$compo)))

pix_area_m2 <- res(hdom)[1] * res(hdom)[2]

# ============================================================
# 5) STATS
# ============================================================
stats_terr <- dt_pix[
  , .(
    n_pix = .N,
    hdom_mean   = mean(hdom, na.rm = TRUE),
    hdom_median = median(hdom, na.rm = TRUE),
    hdom_sd     = sd(hdom, na.rm = TRUE),
    hdom_min    = min(hdom, na.rm = TRUE),
    hdom_max    = max(hdom, na.rm = TRUE)
  ),
  by = .(terr_id_chr)
][order(terr_id_chr)]

stats_terr[, area_ha := (n_pix * pix_area_m2) / 10000]

stats_terr_compo <- dt_pix[
  , .(
    n_pix = .N,
    hdom_mean   = mean(hdom, na.rm = TRUE),
    hdom_median = median(hdom, na.rm = TRUE),
    hdom_sd     = sd(hdom, na.rm = TRUE),
    hdom_min    = min(hdom, na.rm = TRUE),
    hdom_max    = max(hdom, na.rm = TRUE)
  ),
  by = .(terr_id_chr, terr_num, compo)
][order(terr_id_chr, compo)]

stats_terr_compo[, area_ha := (n_pix * pix_area_m2) / 10000]
stats_terr_compo[, compo_raster := compo]
stats_terr_compo <- merge(stats_terr_compo, dict_compo, by = "compo_raster", all.x = TRUE)

setcolorder(
  stats_terr_compo,
  c("terr_id_chr","terr_num","compo","compo_raster","code_essence","essence",
    "n_pix","area_ha","hdom_mean","hdom_median","hdom_sd","hdom_min","hdom_max")
)

fwrite(stats_terr, file.path(out_dir, "stats_hdom_par_territoire_FEUILLLUS.csv"), sep=";", dec=",")
fwrite(stats_terr_compo, file.path(out_dir, "stats_hdom_par_territoire_et_essence_FEUILLLUS.csv"), sep=";", dec=",")
write.xlsx(stats_terr, file.path(out_dir, "stats_hdom_par_territoire_FEUILLLUS.xlsx"))
write.xlsx(stats_terr_compo, file.path(out_dir, "stats_hdom_par_territoire_et_essence_FEUILLLUS.xlsx"))

# ============================================================
# 6) QUANTILES
# ============================================================
qtab <- dt_pix[
  , as.list(quantile(hdom, probs = probs, na.rm = TRUE, names = FALSE)),
  by = .(terr_id_chr, terr_num, compo)
]
setnames(qtab, old = names(qtab)[4:8], new = qs)

surf_tab <- dt_pix[, .(n_pix = .N), by = .(terr_id_chr, terr_num, compo)]
surf_tab[, area_ha := (n_pix * pix_area_m2) / 10000]
qtab <- merge(qtab, surf_tab, by = c("terr_id_chr","terr_num","compo"), all.x = TRUE)

qtab[, compo_raster := compo]
qtab <- merge(qtab, dict_compo, by = "compo_raster", all.x = TRUE)

fwrite(qtab, file.path(out_dir, "quantiles_hdom_par_territoire_et_essence_FEUILLLUS.csv"), sep=";", dec=",")
write.xlsx(qtab, file.path(out_dir, "quantiles_hdom_par_territoire_et_essence_FEUILLLUS.xlsx"))
cat("✅ Quantiles FEUILLLUS exportés.\n")

# ============================================================
# 7) CARTES quantiles
# ============================================================
# raster terr_num (clé stable pour map terr_id -> terr_num)
terr_vals <- unique(values(terr_id))
terr_vals <- terr_vals[!is.na(terr_vals)]
terr_map <- data.table(terr_val = terr_vals)
terr_map[, terr_num := as.integer(factor(as.character(terr_val)))]

terr_num_r <- classify(terr_id, as.matrix(terr_map), others = NA)
key_r <- terr_num_r * 100 + compo

maps_dir <- file.path(out_dir, "maps_quantiles_FEUILLLUS")
dir.create(maps_dir, showWarnings = FALSE, recursive = TRUE)

for (qq in qs) {
  
  tmp <- qtab[area_ha >= min_ha_quantiles, .(terr_num, compo, seuil = get(qq))]
  tmp[, key := terr_num * 100 + compo]
  tmp <- tmp[!is.na(key) & !is.na(seuil)]
  
  rcl <- as.matrix(tmp[, .(key, seuil)])
  seuil_r <- classify(key_r, rcl, others = NA)
  
  keep <- mask_bool & (hdom > seuil_r)
  out_r <- ifel(keep, 1, NA)
  
  out_tif <- file.path(maps_dir, paste0("OGF_FEUILLLUS_", qq, ".tif"))
  writeRaster(out_r, out_tif, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  
  cat("✅ Carte écrite:", out_tif, "\n")
}

# ============================================================
# 8) GRAPHS
# ============================================================
dt_plot <- copy(stats_terr_compo)
dt_plot <- dt_plot[area_ha >= 1]

graph_dir <- file.path(out_dir, "graphs_par_territoire_FEUILLLUS")
dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

territoires <- unique(dt_plot$terr_id_chr)

for (tname in territoires) {
  
  sub <- dt_plot[terr_id_chr == tname]
  if (nrow(sub) == 0) next
  
  sub <- sub[order(-hdom_median)]
  
  p <- ggplot(sub, aes(x = reorder(code_essence, hdom_median), y = hdom_median)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Territoire écologique", tname, "- FEUILLUS"),
      x = "Essence",
      y = "HDOM médian"
    ) +
    theme_minimal()
  
  out_png <- file.path(graph_dir, paste0("Territoire_", tname, "_FEUILLLUS.png"))
  ggsave(out_png, plot = p, width = 8, height = 6, dpi = 300)
}

cat(
  "\n✅ Terminé.\n",
  "Seuil PROP_FE utilisé:", thr_propfe, "\n",
  "Quantiles:", file.path(out_dir, "quantiles_hdom_par_territoire_et_essence_FEUILLLUS.xlsx"), "\n",
  "Cartes:", maps_dir, "\n",
  "Graphs:", graph_dir, "\n"
  

)



#> cat("\n================ CONTROLE PIXELS AVANT/APRES ================\n")

#================ CONTROLE PIXELS AVANT/APRES ================
# > print(ctrl_tab)
#quantile n_pix_before n_pix_after prop_kept prop_removed
#<char>        <num>       <num>     <num>        <num>
#  1:      q50     14438596     6837894 0.4735844    0.5264156
#2:      q70     14438596     4040033 0.2798079    0.7201921
#3:      q75     14438596     3390517 0.2348232    0.7651768
#4:      q80     14438596     2784221 0.1928318    0.8071682
#5:      q90     14438596     1549916 0.1073453    0.8926547