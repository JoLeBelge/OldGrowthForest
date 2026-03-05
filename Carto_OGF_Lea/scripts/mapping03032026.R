library(terra)
library(data.table)
library(openxlsx)

# ============================================================
# PARAMS
# ============================================================
min_ha <- 20
qs <- c(q50=0.50, q70=0.70, q75=0.75, q80=0.80, q90=0.90)
topo_classes <- 1:4

# ============================================================
# PATHS (ADAPTE ICI)
# ============================================================
pth_hdom   <- "C:/Old_Growth_Forest/Carto_OGF/raw/dendro_hdom_10m.tif"
pth_topo   <- "C:/Old_Growth_Forest/Carto_OGF/raw/TOPO2020.tif"
pth_compo  <- "C:/Old_Growth_Forest/Carto_OGF/raw/compo_all_sp10m.tif"
pth_terr   <- "C:/Old_Growth_Forest/Carto_Lea/data_raw/Territoires_ecologiques/territoires_ecologiques_2011/territoires_ecologiques_2011.shp"
pth_mask   <- "C:/Old_Growth_Forest/Carto_OGF/outputs/cache/mask_analysis_1NA.tif"  # ton masque final 1/NA

out_dir <- "C:/Old_Growth_Forest/Carto_OGF/outputs_REBOOT"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dir_status <- file.path(out_dir, "status_maps")
dir_tables <- file.path(out_dir, "tables_controls")
dir_cuts   <- file.path(out_dir, "cuts_by_territory")
dir.create(dir_status, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_cuts,   showWarnings = FALSE, recursive = TRUE)

# ============================================================
# HELPERS
# ============================================================
align_to <- function(r, ref, method="near") {
  if (!same.crs(r, ref)) r <- project(r, crs(ref))
  r <- resample(r, ref, method=method)
  r <- crop(r, ref) |> extend(ref)
  r
}

norm_id <- function(x){
  x <- trimws(as.character(x))
  x <- gsub("'", "", x)
  x <- gsub("^0+", "", x)
  x[x==""] <- "0"
  x
}

# zonal mean sur logique (TRUE/FALSE) -> transforme en 0/1
zonal_mean01 <- function(bool_r, zones_r) {
  zonal(as.int(bool_r), zones_r, fun="mean", na.rm=TRUE)
}

# ============================================================
# 1) READ + ALIGN
# ============================================================
cat("1) Lecture & align...\n")

hdom <- rast(pth_hdom)

mask_1NA <- rast(pth_mask)
mask_1NA <- align_to(mask_1NA, hdom, method="near")
mask_bool <- !is.na(mask_1NA)

topo_raw <- rast(pth_topo)
topo <- align_to(topo_raw, hdom, method="near") |> mask(hdom)
topo[!(topo %in% topo_classes)] <- NA

compo_raw <- rast(pth_compo)
compo <- align_to(compo_raw, hdom, method="near") |> mask(hdom)

# ============================================================
# 2) TERRITORIES -> terr_id raster + terr_num stable
# ============================================================
cat("2) Rasterize territoires + terr_num stable...\n")

terr_v <- vect(pth_terr)
if (!same.crs(terr_v, hdom)) terr_v <- project(terr_v, crs(hdom))

terr_field <- setdiff(names(terr_v), "geometry")[1]
if (is.na(terr_field) || terr_field=="") stop("Impossible de trouver un champ attributaire dans le shp.")

terr_id <- rasterize(terr_v, hdom, field=terr_field, touches=TRUE) |> mask(terr_v)
terr_id <- align_to(terr_id, hdom, method="near") |> mask(hdom)

terr_vals <- unique(values(terr_id))
terr_vals <- terr_vals[!is.na(terr_vals)]
terr_map <- data.table(terr_val=terr_vals)
terr_map[, terr_norm := norm_id(terr_val)]
terr_map[, terr_num := as.integer(factor(terr_norm, levels=sort(unique(terr_norm))))]

terr_num_r <- classify(terr_id, as.matrix(terr_map[, .(terr_val, terr_num)]), others=NA)
terr_num_r <- as.int(terr_num_r)

# aire pixel + min pixels
pix_area_ha <- (res(hdom)[1]*res(hdom)[2]) / 10000
min_pix <- ceiling(min_ha / pix_area_ha)

cat("Pixel area (ha):", pix_area_ha, " | min_pix for", min_ha, "ha:", min_pix, "\n")

# ============================================================
# 3) KEYS + ELIGIBILITY masks (plateau vs hors plateau)
# ============================================================
cat("3) Construction des clés...\n")

# keys (peuvent être NA si une couche est NA)
key_topo <- terr_num_r*10L  + topo        # terr×topo (hors plateau)
key_ess  <- terr_num_r*100L + compo       # terr×essence (plateau)

eligible_topo_base <- mask_bool & (topo != 2) & !is.na(key_topo) & !is.na(hdom)
eligible_ess_base  <- mask_bool & (topo == 2) & !is.na(key_ess)  & !is.na(hdom)

# Masques appliqués aux zones/valeurs pour zonal()

# --- IMPORTANT : convertir le masque logique en 1/NA
m_topo <- ifel(eligible_topo_base, 1, NA)
m_ess  <- ifel(eligible_ess_base,  1, NA)

hdom_topo <- mask(hdom, m_topo)
zon_topo  <- mask(key_topo, m_topo)

hdom_ess  <- mask(hdom, m_ess)
zon_ess   <- mask(key_ess, m_ess)
# ============================================================
# 4) COUNTS (pour min_ha) : nb pixels par key
# ============================================================
cat("4) Comptage pixels par groupe (min_ha)...\n")

cnt_topo <- zonal(!is.na(hdom_topo), zon_topo, fun="sum", na.rm=TRUE)
cnt_topo <- as.data.table(cnt_topo); setnames(cnt_topo, c("key","n_pix"))
cnt_topo <- cnt_topo[n_pix >= min_pix]

cnt_ess <- zonal(!is.na(hdom_ess), zon_ess, fun="sum", na.rm=TRUE)
cnt_ess <- as.data.table(cnt_ess); setnames(cnt_ess, c("key","n_pix"))
cnt_ess <- cnt_ess[n_pix >= min_pix]

cat("Groupes topo éligibles:", nrow(cnt_topo), "\n")
cat("Groupes essence éligibles:", nrow(cnt_ess), "\n")

# ============================================================
# 5) LOOP quantiles -> seuils + STATUS maps + contrôles
# ============================================================
cat("5) Calcul des quantiles + cartes...\n")

all_ctrl_topo <- list()
all_ctrl_ess  <- list()
all_ctrl_terr <- list()

for (qq in names(qs)) {
  
  p <- qs[[qq]]
  cat("\n---", qq, "(p =", p, ") ---\n")
  
  # ---------- 5A) Quantiles TOPO (terr×topo, hors plateau)
  q_topo <- zonal(
    hdom_topo, zon_topo,
    fun = function(x, ...) quantile(x, p, na.rm = TRUE, names = FALSE),
    na.rm = TRUE
  )
  q_topo <- as.data.table(q_topo); setnames(q_topo, c("key","seuil"))
  
  thr_topo <- merge(q_topo, cnt_topo, by="key", all=FALSE)
  thr_topo <- thr_topo[!is.na(seuil)]
  
  seuil_topo_r <- classify(key_topo, as.matrix(thr_topo[, .(key, seuil)]), others=NA)
  
  eligible_topo <- eligible_topo_base & !is.na(seuil_topo_r)
  status_topo <- ifel(eligible_topo, ifel(hdom > seuil_topo_r, 1, 2), NA)
  
  out_status_topo <- file.path(dir_status, paste0("STATUS_TOPO_", qq, ".tif"))
  writeRaster(status_topo, out_status_topo, overwrite=TRUE,
              gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  cat("✅", out_status_topo, "\n")
  
  # ---------- 5B) Quantiles ESSENCE (terr×essence, plateau)
  q_ess <- zonal(
    hdom_ess, zon_ess,
    fun = function(x, ...) quantile(x, p, na.rm = TRUE, names = FALSE),
    na.rm = TRUE
  )
  q_ess <- as.data.table(q_ess); setnames(q_ess, c("key","seuil"))
  
  thr_ess <- merge(q_ess, cnt_ess, by="key", all=FALSE)
  thr_ess <- thr_ess[!is.na(seuil)]
  
  seuil_ess_r <- classify(key_ess, as.matrix(thr_ess[, .(key, seuil)]), others=NA)
  
  eligible_ess <- eligible_ess_base & !is.na(seuil_ess_r)
  status_ess <- ifel(eligible_ess, ifel(hdom > seuil_ess_r, 1, 2), NA)
  
  out_status_ess <- file.path(dir_status, paste0("STATUS_ESS_", qq, ".tif"))
  writeRaster(status_ess, out_status_ess, overwrite=TRUE,
              gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  cat("✅", out_status_ess, "\n")
  
  # ---------- 5C) MIX
  status_mix <- ifel(topo == 2, status_ess, status_topo)
  out_status_mix <- file.path(dir_status, paste0("STATUS_MIX_", qq, ".tif"))
  writeRaster(status_mix, out_status_mix, overwrite=TRUE,
              gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  cat("✅", out_status_mix, "\n")
  
  # ---------- 5D) CONTROLES par groupe
  m_topo_ok   <- ifel(eligible_topo, 1, NA)
  zon_topo_ok <- mask(zon_topo, m_topo_ok)
  prop_topo <- zonal_mean01(status_topo == 1, zon_topo_ok)
  prop_topo <- as.data.table(prop_topo); setnames(prop_topo, c("key","prop_keep"))
  prop_topo[, `:=`(prop_keep_pct = 100*prop_keep, quantile = qq, p = p)]
  all_ctrl_topo[[qq]] <- prop_topo
  
  m_ess_ok   <- ifel(eligible_ess, 1, NA)
  zon_ess_ok <- mask(zon_ess, m_ess_ok)
  prop_ess <- zonal_mean01(status_ess == 1, zon_ess_ok)
  prop_ess <- as.data.table(prop_ess); setnames(prop_ess, c("key","prop_keep"))
  prop_ess[, `:=`(prop_keep_pct = 100*prop_keep, quantile = qq, p = p)]
  all_ctrl_ess[[qq]] <- prop_ess
  
  # ---------- 5E) CONTROLE par territoire (MIX)
  n_in_mask <- zonal(as.int(mask_bool), terr_id, fun="sum", na.rm=TRUE)
  n_in_mask <- as.data.table(n_in_mask); setnames(n_in_mask, c("terr_id","n_in_mask"))
  
  n_eligible <- zonal(as.int(!is.na(status_mix)), terr_id, fun="sum", na.rm=TRUE)
  n_eligible <- as.data.table(n_eligible); setnames(n_eligible, c("terr_id","n_eligible"))
  
  n_keep <- zonal(as.int(status_mix == 1), terr_id, fun="sum", na.rm=TRUE)
  n_keep <- as.data.table(n_keep); setnames(n_keep, c("terr_id","n_keep"))
  
  n_removed <- zonal(as.int(status_mix == 2), terr_id, fun="sum", na.rm=TRUE)
  n_removed <- as.data.table(n_removed); setnames(n_removed, c("terr_id","n_removed"))
  
  tab <- Reduce(function(x,y) merge(x,y, by="terr_id", all=TRUE),
                list(n_in_mask, n_eligible, n_keep, n_removed))
  
  for (cc in c("n_in_mask","n_eligible","n_keep","n_removed")) tab[is.na(get(cc)), (cc):=0L]
  
  tab[, `:=`(
    quantile = qq,
    area_in_mask_ha    = n_in_mask * pix_area_ha,
    area_eligible_ha   = n_eligible * pix_area_ha,
    area_keep_ha       = n_keep * pix_area_ha,
    area_removed_ha    = n_removed * pix_area_ha,
    prop_keep_vs_mask_pct      = fifelse(n_in_mask > 0, 100 * n_keep / n_in_mask, NA_real_),
    prop_keep_vs_eligible_pct  = fifelse(n_eligible > 0, 100 * n_keep / n_eligible, NA_real_),
    prop_eligible_vs_mask_pct  = fifelse(n_in_mask > 0, 100 * n_eligible / n_in_mask, NA_real_)
  )]
  all_ctrl_terr[[qq]] <- tab
  
  # exports seuils (à la fin de chaque quantile)
  fwrite(thr_topo, file.path(dir_tables, paste0("THRESH_TOPO_", qq, ".csv")), sep=";", dec=",")
  fwrite(thr_ess,  file.path(dir_tables, paste0("THRESH_ESS_",  qq, ".csv")), sep=";", dec=",")
}  
# ============================================================
# 6) EXPORT tables contrôles
# ============================================================
cat("\n6) Export tables contrôles...\n")

ctrl_topo <- rbindlist(all_ctrl_topo, fill=TRUE)
ctrl_ess  <- rbindlist(all_ctrl_ess,  fill=TRUE)
ctrl_terr <- rbindlist(all_ctrl_terr, fill=TRUE)

# pour interpréter les clés (optionnel): extraire terr_num & topo/compo depuis key
ctrl_topo[, `:=`(terr_num = key %/% 10L, topo = key %% 10L)]
ctrl_ess[,  `:=`(terr_num = key %/% 100L, compo = key %% 100L)]

setorder(ctrl_topo, quantile, terr_num, topo)
setorder(ctrl_ess,  quantile, terr_num, compo)
setorder(ctrl_terr, quantile, terr_id)

fwrite(ctrl_topo, file.path(dir_tables, "CTRL_propkeep_by_TERRxTOPO.csv"), sep=";", dec=",")
fwrite(ctrl_ess,  file.path(dir_tables, "CTRL_propkeep_by_TERRxESS.csv"),  sep=";", dec=",")
fwrite(ctrl_terr, file.path(dir_tables, "CTRL_propkeep_by_TERR_MIX.csv"),  sep=";", dec=",")

write.xlsx(list(
  CTRL_TERRxTOPO = ctrl_topo,
  CTRL_TERRxESS  = ctrl_ess,
  CTRL_TERR_MIX  = ctrl_terr
), file.path(dir_tables, "CTRL_ALL.xlsx"), overwrite=TRUE)

cat("✅ Tables écrites dans:", dir_tables, "\n")

# ============================================================
# 7) DECOUPES par territoire (ex: q50 MIX)
# ============================================================
cat("\n7) Découpes par territoire pour STATUS_MIX_q50...\n")

status_mix_q50 <- rast(file.path(dir_status, "STATUS_MIX_q50.tif"))
status_mix_q50 <- align_to(status_mix_q50, hdom, method="near")
terr_id2 <- align_to(terr_id, status_mix_q50, method="near")

terr_list <- unique(values(terr_id2))
terr_list <- terr_list[!is.na(terr_list)]
terr_list <- sort(terr_list)

out_cut_dir <- file.path(dir_cuts, "STATUS_MIX_q50_byTerritory")
dir.create(out_cut_dir, showWarnings = FALSE, recursive = TRUE)

for (tv in terr_list) {
  m <- terr_id2 == tv
  r_cut <- mask(status_mix_q50, m, maskvalues=0, updatevalue=NA)
  tv_safe <- gsub("[^0-9A-Za-z_-]", "_", as.character(tv))
  out_one <- file.path(out_cut_dir, paste0("STATUS_MIX_q50_terr_", tv_safe, ".tif"))
  writeRaster(r_cut, out_one, overwrite=TRUE, gdal=c("COMPRESS=DEFLATE","TILED=YES"))
}

cat("\n✅ FIN REBOOT\n",
    "Status maps:", dir_status, "\n",
    "Tables:", dir_tables, "\n",
    "Cuts:", out_cut_dir, "\n")

































library(terra)
library(data.table)
library(openxlsx)

qq <- "q50"
p  <- 0.50  # change selon qq si besoin

# clés
key_topo <- terr_num_r*10L  + topo
key_ess  <- terr_num_r*100L + compo

# --- masques de base
eligible_topo_base <- mask_bool & (topo != 2) & !is.na(key_topo) & !is.na(hdom)
eligible_ess_base  <- mask_bool & (topo == 2) & !is.na(key_ess)  & !is.na(hdom)

# --- (re)construire les rasters de seuils à partir des CSV exportés par le reboot
thr_topo <- fread(file.path(dir_tables, paste0("THRESH_TOPO_", qq, ".csv")), sep=";")
thr_ess  <- fread(file.path(dir_tables, paste0("THRESH_ESS_",  qq, ".csv")), sep=";")

seuil_topo_r <- classify(key_topo, as.matrix(thr_topo[, .(key, seuil)]), others=NA)
seuil_ess_r  <- classify(key_ess,  as.matrix(thr_ess[,  .(key, seuil)]), others=NA)

eligible_topo <- eligible_topo_base & !is.na(seuil_topo_r)
eligible_ess  <- eligible_ess_base  & !is.na(seuil_ess_r)

# --- statuts (si tu veux recalculer; sinon relis tes rasters STATUS_TOPO/ESS)
status_topo <- ifel(eligible_topo, ifel(hdom > seuil_topo_r, 1, 2), NA)
status_ess  <- ifel(eligible_ess,  ifel(hdom > seuil_ess_r,  1, 2), NA)

# --- zones "key" éligibles (pour zonal)
zon_topo_ok <- mask(key_topo, eligible_topo)
zon_ess_ok  <- mask(key_ess,  eligible_ess)

# ===== TOPO: n_before (éligibles), n_after (kept), prop =====
n_before_topo <- zonal(as.int(!is.na(status_topo)), zon_topo_ok, fun="sum", na.rm=TRUE)
n_after_topo  <- zonal(as.int(status_topo==1),     zon_topo_ok, fun="sum", na.rm=TRUE)

n_before_topo <- as.data.table(n_before_topo); setnames(n_before_topo, c("key","n_before"))
n_after_topo  <- as.data.table(n_after_topo);  setnames(n_after_topo,  c("key","n_after"))

tab_topo <- merge(thr_topo, n_before_topo, by="key", all.x=TRUE)
tab_topo <- merge(tab_topo, n_after_topo,  by="key", all.x=TRUE)
tab_topo[is.na(n_before), n_before := 0L]
tab_topo[is.na(n_after),  n_after  := 0L]

tab_topo[, `:=`(
  terr_num = key %/% 10L,
  topo     = key %% 10L,
  prop_keep = fifelse(n_before>0, 100*n_after/n_before, NA_real_)
)]
setorder(tab_topo, terr_num, topo)

# ===== ESSENCE: n_before, n_after, prop =====
n_before_ess <- zonal(as.int(!is.na(status_ess)), zon_ess_ok, fun="sum", na.rm=TRUE)
n_after_ess  <- zonal(as.int(status_ess==1),     zon_ess_ok, fun="sum", na.rm=TRUE)

n_before_ess <- as.data.table(n_before_ess); setnames(n_before_ess, c("key","n_before"))
n_after_ess  <- as.data.table(n_after_ess);  setnames(n_after_ess,  c("key","n_after"))

tab_ess <- merge(thr_ess, n_before_ess, by="key", all.x=TRUE)
tab_ess <- merge(tab_ess, n_after_ess,  by="key", all.x=TRUE)
tab_ess[is.na(n_before), n_before := 0L]
tab_ess[is.na(n_after),  n_after  := 0L]

tab_ess[, `:=`(
  terr_num = key %/% 100L,
  compo    = key %% 100L,
  prop_keep = fifelse(n_before>0, 100*n_after/n_before, NA_real_)
)]
setorder(tab_ess, terr_num, compo)

# ===== Impression console =====
cat("\n====================\n")
cat("SEUILS TOPO (", qq, ")\n", sep="")
cat("====================\n")
print(tab_topo[, .(terr_num, topo, seuil_hdom=seuil, n_before, n_after, prop_keep)])

cat("\n====================\n")
cat("SEUILS ESSENCE (", qq, ")\n", sep="")
cat("====================\n")
print(tab_ess[, .(terr_num, compo, seuil_hdom=seuil, n_before, n_after, prop_keep)])

# ===== Exports =====
fwrite(tab_topo, file.path(dir_tables, paste0("REPORT_THRESH_TOPO_", qq, ".csv")), sep=";", dec=",")
fwrite(tab_ess,  file.path(dir_tables, paste0("REPORT_THRESH_ESS_",  qq, ".csv")), sep=";", dec=",")

write.xlsx(list(
  TOPO = tab_topo[, .(terr_num, topo, seuil_hdom=seuil, n_before, n_after, prop_keep)],
  ESSENCE = tab_ess[, .(terr_num, compo, seuil_hdom=seuil, n_before, n_after, prop_keep)]
), file.path(dir_tables, paste0("REPORT_THRESH_", qq, ".xlsx")), overwrite=TRUE)

cat("\n✅ Reports écrits dans :", dir_tables, "\n")


library(terra)

# à faire après avoir construit terr_num_r (dans ton script reboot)
# (si terr_num_r n’existe pas dans l’environnement, dis-moi et je te donne le bloc minimal pour le recréer)

pix_area_ha <- (res(hdom)[1] * res(hdom)[2]) / 10000

ha_terr1_total <- global(as.int(terr_num_r == 1), "sum", na.rm=TRUE)[1,1] * pix_area_ha
ha_terr1_mask  <- global(as.int((terr_num_r == 1) & mask_bool), "sum", na.rm=TRUE)[1,1] * pix_area_ha

c(terr1_total_ha = ha_terr1_total, terr1_in_mask_ha = ha_terr1_mask)

terr_in_mask <- mask(terr_id, mask_bool)
freq(terr_in_mask)













library(terra)

# chemins
status_mix_q50_path <- file.path(dir_status, "STATUS_MIX_q50.tif")

# rasters
status_mix <- rast(status_mix_q50_path)
mask_1NA   <- rast(pth_mask)

# aligner le masque sur status_mix (important)
if (!same.crs(mask_1NA, status_mix)) mask_1NA <- project(mask_1NA, crs(status_mix))
mask_1NA <- resample(mask_1NA, status_mix, method="near") |> crop(status_mix) |> extend(status_mix)

mask_bool <- !is.na(mask_1NA)

# aire pixel
pix_area_ha <- (res(status_mix)[1] * res(status_mix)[2]) / 10000

# comptages pixels (dans le masque)
n_mask <- global(as.int(mask_bool), "sum", na.rm=TRUE)[1,1]
n_keep <- global(as.int((status_mix == 1) & mask_bool), "sum", na.rm=TRUE)[1,1]
n_elig <- global(as.int(!is.na(status_mix) & mask_bool), "sum", na.rm=TRUE)[1,1]

# surfaces
area_mask_ha <- n_mask * pix_area_ha
area_keep_ha <- n_keep * pix_area_ha

# pourcentages
pct_keep_vs_mask <- 100 * n_keep / n_mask
pct_keep_vs_eligible <- 100 * n_keep / n_elig
pct_eligible_vs_mask <- 100 * n_elig / n_mask

cat("Surface masque (ha):", area_mask_ha, "\n")
cat("Surface gardée q50 MIX (ha):", area_keep_ha, "\n")
cat("% gardée vs masque:", pct_keep_vs_mask, "\n")
cat("% gardée vs éligibles:", pct_keep_vs_eligible, "\n")
cat("% éligibles vs masque:", pct_eligible_vs_mask, "\n")







library(terra)
library(data.table)
library(openxlsx)

qq <- "q50"

status_mix <- rast(file.path(dir_status, paste0("STATUS_MIX_", qq, ".tif")))

# terr_id et mask_1NA existent déjà si tu as lancé le script reboot
# sinon, relis mask_1NA depuis pth_mask et rasterise terr_id comme dans ton script

align_to_ref <- function(r, ref, method="near") {
  if (!same.crs(r, ref)) r <- project(r, crs(ref))
  r <- resample(r, ref, method=method) |> crop(ref) |> extend(ref)
  r
}

terr_id  <- align_to_ref(terr_id, status_mix, "near")
mask_1NA <- align_to_ref(mask_1NA, status_mix, "near")
mask_bool <- !is.na(mask_1NA)

pix_area_ha <- (res(status_mix)[1] * res(status_mix)[2]) / 10000

n_in_mask <- zonal(as.int(mask_bool), terr_id, fun="sum", na.rm=TRUE)
n_keep    <- zonal(as.int((status_mix==1) & mask_bool), terr_id, fun="sum", na.rm=TRUE)
n_elig    <- zonal(as.int(!is.na(status_mix) & mask_bool), terr_id, fun="sum", na.rm=TRUE)

dt_mask <- as.data.table(n_in_mask); setnames(dt_mask, c("terr_id","n_in_mask"))
dt_keep <- as.data.table(n_keep);    setnames(dt_keep, c("terr_id","n_keep"))
dt_elig <- as.data.table(n_elig);    setnames(dt_elig, c("terr_id","n_eligible"))

tab <- Reduce(function(x,y) merge(x,y, by="terr_id", all=TRUE),
              list(dt_mask, dt_keep, dt_elig))

for (cc in c("n_in_mask","n_keep","n_eligible")) tab[is.na(get(cc)), (cc) := 0L]

tab[, `:=`(
  quantile = qq,
  area_in_mask_ha  = n_in_mask * pix_area_ha,
  area_keep_ha     = n_keep    * pix_area_ha,
  area_eligible_ha = n_eligible * pix_area_ha,
  pct_keep_vs_mask = fifelse(n_in_mask>0, 100*n_keep/n_in_mask, NA_real_),
  pct_keep_vs_eligible = fifelse(n_eligible>0, 100*n_keep/n_eligible, NA_real_),
  pct_eligible_vs_mask = fifelse(n_in_mask>0, 100*n_eligible/n_in_mask, NA_real_)
)]

setorder(tab, terr_id)

out_csv  <- file.path(dir_tables, paste0("KEEP_PCT_by_TERR_", qq, ".csv"))
out_xlsx <- file.path(dir_tables, paste0("KEEP_PCT_by_TERR_", qq, ".xlsx"))
fwrite(tab, out_csv, sep=";", dec=",")
write.xlsx(tab, out_xlsx, overwrite=TRUE)

tab