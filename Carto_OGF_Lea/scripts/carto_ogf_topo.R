library(terra)
library(data.table)
library(openxlsx)

# =====================
# PARAMS
# =====================
min_ha <- 1
qs    <- c("q50","q70","q75","q80","q90")
probs <- c(0.50, 0.70, 0.75, 0.80, 0.90)

# =====================
# CHEMINS
# =====================
out_dir <- "C:/Old_Growth_Forest/Carto_OGF/outputs"
cache_dir <- file.path(out_dir, "cache")

pth_hdom   <- file.path(cache_dir, "hdom_crop.tif")
pth_terrID <- file.path(cache_dir, "terr_eco_id.tif")
pth_mask   <- file.path(cache_dir, "mask_analysis_1NA.tif")

pth_topo_raw <- "C:/Old_Growth_Forest/Carto_OGF/raw/TOPO2020.tif"
pth_topo_on_hdom <- file.path(cache_dir, "topo_on_hdom.tif")  # nouveau cache topo recalé

suffix  <- "FA_PROPFE80_FEUILLUS"
out_tif <- file.path(out_dir, paste0("HDOM_classif_q50_TERRxTOPO_", suffix, ".tif"))

# =====================
# LIRE
# =====================
hdom    <- rast(pth_hdom)
terr_id <- rast(pth_terrID)
mask_1NA <- rast(pth_mask)
mask_bool <- !is.na(mask_1NA)

# =====================
# TOPO : recaler UNE FOIS sur la grille HDOM
# =====================
if (!file.exists(pth_topo_on_hdom)) {
  topo_raw <- rast(pth_topo_raw)
  
  # reprojection si besoin
  if (!same.crs(topo_raw, hdom)) topo_raw <- project(topo_raw, crs(hdom))
  
  # resample + crop sur grille HDOM
  topo <- resample(topo_raw, hdom, method="near")
  topo <- crop(topo, hdom) |> mask(hdom)
  
  # garder uniquement 1..4
  topo[!(topo %in% 1:4)] <- NA
  
  writeRaster(topo, pth_topo_on_hdom, overwrite=TRUE,
              gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  cat("✅ topo recalé écrit :", pth_topo_on_hdom, "\n")
} else {
  topo <- rast(pth_topo_on_hdom)
  cat("↩️ topo recalé relu :", pth_topo_on_hdom, "\n")
}

# sécurité géométrie
if (!isTRUE(compareGeom(hdom, topo, stopOnError=FALSE))) {
  topo <- resample(topo, hdom, method="near") |> crop(hdom) |> mask(hdom)
  topo[!(topo %in% 1:4)] <- NA
}

# aire pixel
pix_area_ha <- (res(hdom)[1] * res(hdom)[2]) / 10000

# =====================
# terr_num stable (évite les soucis de "01" vs "1")
# =====================
norm_id <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("'", "", x)
  x <- gsub("^0+", "", x)
  x[x == ""] <- "0"
  x
}

terr_vals <- unique(values(terr_id))
terr_vals <- terr_vals[!is.na(terr_vals)]
terr_map <- data.table(terr_val = terr_vals)
terr_map[, terr_id_norm := norm_id(as.character(terr_val))]
terr_map[, terr_num := as.integer(factor(terr_id_norm))]
terr_num_r <- subst(terr_id, from=terr_map$terr_val, to=terr_map$terr_num)

# =====================
# TABLE PIXELS sous masque : terr_num, topo, hdom
# =====================
s <- c(terr_num_r, topo, hdom)
names(s) <- c("terr_num","topo","hdom")
s_m <- mask(s, mask_bool, maskvalues=0, updatevalue=NA)

df <- as.data.frame(s_m, na.rm=TRUE)
if (nrow(df) == 0) stop("Aucun pixel après masque_analysis_1NA.")

dt <- as.data.table(df)
dt[, `:=`(
  terr_num = as.integer(terr_num),
  topo = as.integer(topo),
  pix_area_ha = pix_area_ha
)]

# =====================
# STATS terr×topo + export
# =====================
stats_terr_topo <- dt[
  ,
  c(
    .(n_pix=.N, area_ha=sum(pix_area_ha)),
    as.list(quantile(hdom, probs=probs, na.rm=TRUE, names=FALSE))
  ),
  by=.(terr_num, topo)
]
setnames(stats_terr_topo,
         old = names(stats_terr_topo)[(ncol(stats_terr_topo)-length(qs)+1):ncol(stats_terr_topo)],
         new = qs)

stats_use <- stats_terr_topo[area_ha >= min_ha]

fwrite(stats_terr_topo,
       file.path(out_dir, paste0("HDOM_stats_TERRxTOPO_", suffix, "_q50_q70_q75_q80_q90.csv")),
       sep=";", dec=",")
write.xlsx(stats_terr_topo,
           file.path(out_dir, paste0("HDOM_stats_TERRxTOPO_", suffix, "_q50_q70_q75_q80_q90.xlsx")))

# =====================
# CARTE q50 : 1 si HDOM >= q50(territoire×topo), sinon 0, NA hors masque
# =====================
thr_dt <- stats_use[, .(terr_num, topo, q50)]
thr_dt <- thr_dt[!is.na(q50)]
thr_dt[, key := terr_num * 10L + topo]
thr_dt <- unique(thr_dt, by="key")

key_r <- terr_num_r * 10L + topo
q50_r <- subst(key_r, from=thr_dt$key, to=thr_dt$q50)

class_q50 <- ifel(!is.na(q50_r) & (hdom >= q50_r), 1, NA)
class_q50 <- mask(class_q50, mask_bool, maskvalues=0, updatevalue=NA)
class_q50 <- mask(class_q50, hdom)


# aire d’un pixel (ha)
pix_area_ha <- (res(hdom)[1] * res(hdom)[2]) / 10000

# AVANT : pixels dans la zone analysée (mask)
n_before <- global(mask_bool, "sum", na.rm = TRUE)[1,1]

# APRES : pixels gardés (valeur 1)
# Cas KEEP 1/NA :
n_after <- global(class_q50, "sum", na.rm = TRUE)[1,1]

cat("Pixels AVANT (zone analysée) :", n_before, "\n")
cat("Pixels APRES (KEEP q50)      :", n_after, "\n")
cat("Surface AVANT (ha) :", n_before * pix_area_ha, "\n")
cat("Surface APRES (ha) :", n_after  * pix_area_ha, "\n")
cat("Proportion gardée  :", round(100 * n_after / n_before, 2), "%\n")

writeRaster(class_q50, out_tif, overwrite=TRUE,
            gdal=c("COMPRESS=DEFLATE","TILED=YES"))

cat("✅ Carte exportée :", out_tif, "\n")



#Carte sans valeur 2 plateau versant faible pente :


library(terra)

keep_tif <- "C:/Old_Growth_Forest/Carto_OGF/outputs/HDOM_classif_q50_TERRxTOPO_FA_PROPFE80_FEUILLUS.tif"
topo_tif <- "C:/Old_Growth_Forest/Carto_OGF/outputs/cache/topo_on_hdom.tif"
out_tif  <- "C:/Old_Growth_Forest/Carto_OGF/outputs/HDOM_classif_q50_TERRxTOPO_FA_PROPFE80_FEUILLUS_sansPlateau.tif"

keep <- rast(keep_tif)
topo <- rast(topo_tif)

# garde uniquement topo != 2
keep_no2 <- mask(keep, topo != 2, maskvalues=0, updatevalue=NA)

writeRaster(keep_no2, out_tif, overwrite=TRUE,
            gdal=c("COMPRESS=DEFLATE","TILED=YES"))

cat("✅ Export sans topo==2 :", out_tif, "\n")



#carte essence X hdom X Territoire écologique découpé au raster topo pente

library(terra)

# --- fichiers ---
keep_tif <- "C:/Old_Growth_Forest/Carto_OGF/outputs/maps_quantiles_FEUILLLUS/OGF_FEUILLLUS_keep_q50_floor.tif"

# topo déjà recalé sur la grille HDOM (si tu as suivi l’étape précédente)
topo_tif <- "C:/Old_Growth_Forest/Carto_OGF/outputs/cache/topo_on_hdom.tif"

out_tif <- "C:/Old_Growth_Forest/Carto_OGF/outputs/maps_quantiles_FEUILLLUS/OGF_FEUILLLUS_keep_q50_floor_TOPO2.tif"

# --- lire ---
keep <- rast(keep_tif)
topo <- rast(topo_tif)

# sécurité: même grille (sinon on resample topo sur keep)
if (!isTRUE(compareGeom(keep, topo, stopOnError = FALSE))) {
  topo <- resample(topo, keep, method = "near")
  topo <- crop(topo, keep) |> mask(keep)
}

# --- masque plateau ---
m2 <- topo == 2

# --- découpe : on garde keep uniquement là où topo==2 ---
keep_topo2 <- mask(keep, m2, maskvalues = 0, updatevalue = NA)

writeRaster(keep_topo2, out_tif, overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"))

cat("✅ Export plateau (topo==2) :", out_tif, "\n")




library(terra)

# 1) CARTES ENTRÉES
# A) Carte topo q50 SANS plateau (topo != 2)
r_topo_no2 <- rast("C:/Old_Growth_Forest/Carto_OGF/outputs/HDOM_classif_q50_TERRxTOPO_FA_PROPFE80_FEUILLUS_sansPlateau.tif")

# B) Carte essence q50 DÉCOUPÉE AU PLATEAU (topo == 2)
r_ess_plateau <- rast("C:/Old_Growth_Forest/Carto_OGF/outputs/maps_quantiles_FEUILLLUS/OGF_FEUILLLUS_keep_q50_floor_TOPO2.tif")

# C) TOPO recalé (pour décider plateau / non-plateau)
topo <- rast("C:/Old_Growth_Forest/Carto_OGF/outputs/cache/topo_on_hdom.tif")

# 2) SÉCURITÉ : même grille
if (!isTRUE(compareGeom(r_topo_no2, r_ess_plateau, stopOnError=FALSE))) {
  stop("Les deux rasters n'ont pas la même grille. Vérifie qu'ils ont été produits sur la même base (hdom_crop).")
}
if (!isTRUE(compareGeom(r_topo_no2, topo, stopOnError=FALSE))) {
  topo <- resample(topo, r_topo_no2, method="near") |> crop(r_topo_no2) |> mask(r_topo_no2)
}

# 3) MASQUES plateau / hors plateau
m_plateau <- topo == 2
m_no2     <- topo %in% c(1,3,4)

# (optionnel mais propre) forcer chaque raster à n’exister que dans sa zone
r_topo_no2   <- mask(r_topo_no2, m_no2, maskvalues=0, updatevalue=NA)
r_ess_plateau <- mask(r_ess_plateau, m_plateau, maskvalues=0, updatevalue=NA)

# 4) FUSION : plateau => essence ; sinon => topo
# comme c’est du 1/NA, on peut faire un OR après avoir masqué :
r_mix_q50 <- ifel(!is.na(r_topo_no2) | !is.na(r_ess_plateau), 1, NA)

# 5) EXPORT
out_tif <- "C:/Old_Growth_Forest/Carto_OGF/outputs/OGF_KEEP_MIX_q50_TOPOxESSENCE.tif"
writeRaster(r_mix_q50, out_tif, overwrite=TRUE,
            gdal=c("COMPRESS=DEFLATE","TILED=YES"))

cat("✅ Carte finale écrite :", out_tif, "\n")


library(terra)
library(data.table)
library(openxlsx)

# --- dénominateur = tous les pixels dans mask par territoire
n_mask <- zonal(mask_bool, terr_id, fun="sum", na.rm=TRUE)
dt_mask <- as.data.table(n_mask)
setnames(dt_mask, c("terr_id","n_in_mask"))

# --- numérateur keep/removed, mais aussi nb "triés" (status non-NA)
keep_bin    <- status_mix_q50 == 1
removed_bin <- status_mix_q50 == 2
elig_bin    <- !is.na(status_mix_q50)

z_keep <- zonal(keep_bin, terr_id, "sum", na.rm=TRUE)
z_rem  <- zonal(removed_bin, terr_id, "sum", na.rm=TRUE)
z_elig <- zonal(elig_bin, terr_id, "sum", na.rm=TRUE)

dt_keep <- as.data.table(z_keep); setnames(dt_keep, c("terr_id","n_keep"))
dt_rem  <- as.data.table(z_rem);  setnames(dt_rem,  c("terr_id","n_removed"))
dt_elig <- as.data.table(z_elig); setnames(dt_elig, c("terr_id","n_eligible"))

# --- merge
tab <- Reduce(function(x,y) merge(x,y, by="terr_id", all=TRUE),
              list(dt_mask, dt_elig, dt_keep, dt_rem))

for (cc in c("n_in_mask","n_eligible","n_keep","n_removed")) tab[is.na(get(cc)), (cc) := 0L]

# --- aires
pix_area_ha <- (res(terr_id)[1] * res(terr_id)[2]) / 10000
tab[, `:=`(
  area_in_mask_ha = n_in_mask * pix_area_ha,
  area_eligible_ha = n_eligible * pix_area_ha,
  area_keep_ha = n_keep * pix_area_ha,
  area_removed_ha = n_removed * pix_area_ha
)]

# --- proportions
tab[, `:=`(
  prop_keep_vs_mask_pct     = fifelse(n_in_mask > 0, 100 * n_keep / n_in_mask, NA_real_),
  prop_removed_vs_mask_pct  = fifelse(n_in_mask > 0, 100 * n_removed / n_in_mask, NA_real_),
  prop_eligible_vs_mask_pct = fifelse(n_in_mask > 0, 100 * n_eligible / n_in_mask, NA_real_),
  prop_keep_vs_eligible_pct = fifelse(n_eligible > 0, 100 * n_keep / n_eligible, NA_real_)
)]

setorder(tab, terr_id)

# export
fwrite(tab, file.path(out_dir, "q50_MIX_stats_par_territoire_DENOM_MASK.csv"), sep=";", dec=",")
write.xlsx(tab, file.path(out_dir, "q50_MIX_stats_par_territoire_DENOM_MASK.xlsx"), overwrite=TRUE)

tab