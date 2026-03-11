# =========================================================
# 0) PACKAGES
# =========================================================
# install.packages(c("readr", "dplyr", "sf", "terra", "openxlsx"))

library(readr)
library(dplyr)
library(sf)
library(terra)
library(openxlsx)

# =========================================================
# 1) FONCTIONS UTILES
# =========================================================

detect_encoding <- function(path) {
  x <- read_file_raw(path)
  if (length(x) >= 2 && x[1] == as.raw(0xFF) && x[2] == as.raw(0xFE)) return("UTF-16LE")
  if (length(x) >= 2 && x[1] == as.raw(0xFE) && x[2] == as.raw(0xFF)) return("UTF-16BE")
  "UTF-8"
}

read_any_csv <- function(path) {
  enc <- detect_encoding(path)
  first <- read_lines(path, n_max = 1, locale = locale(encoding = enc))
  delim <- if (grepl(";", first, fixed = TRUE)) ";" else ","
  
  read_delim(
    path,
    delim = delim,
    locale = locale(encoding = enc),
    show_col_types = FALSE,
    progress = FALSE
  )
}

se <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x) / sqrt(length(x))
}

to_utf8_df <- function(df) {
  names(df) <- iconv(names(df), from = "", to = "UTF-8", sub = "")
  df %>%
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "")
    ))
}

make_indic_global <- function(df) {
  df %>%
    summarise(
      n_placettes = n_distinct(plot_id),
      
      bois_mort_total_moy = mean(bois_mort_total, na.rm = TRUE),
      bois_mort_total_med = median(bois_mort_total, na.rm = TRUE),
      bois_mort_total_se  = se(bois_mort_total),
      
      bois_mort_sur_pied_moy = mean(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_med = median(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_se  = se(bmsp_vhatot),
      
      bois_mort_au_sol_moy = mean(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_med = median(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_se  = se(bmt_vha_tot),
      
      pct_placettes_sans_bois_mort = 100 * mean(bois_mort_total == 0, na.rm = TRUE)
    )
}

make_indic_global_no0 <- function(df) {
  df %>%
    filter(bois_mort_total > 0) %>%
    summarise(
      n_placettes = n_distinct(plot_id),
      
      bois_mort_total_moy = mean(bois_mort_total, na.rm = TRUE),
      bois_mort_total_med = median(bois_mort_total, na.rm = TRUE),
      bois_mort_total_se  = se(bois_mort_total),
      
      bois_mort_sur_pied_moy = mean(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_med = median(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_se  = se(bmsp_vhatot),
      
      bois_mort_au_sol_moy = mean(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_med = median(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_se  = se(bmt_vha_tot),
      
      pct_placettes_sans_bois_mort = NA_real_
    )
}

make_indic_peup <- function(df) {
  df %>%
    group_by(peup_pl) %>%
    summarise(
      n_placettes = n_distinct(plot_id),
      n_placettes_sans_bois_mort = n_distinct(plot_id[bois_mort_total == 0]),
      pct_placettes_sans_bois_mort = 100 * mean(bois_mort_total == 0, na.rm = TRUE),
      
      bois_mort_total_moy = mean(bois_mort_total, na.rm = TRUE),
      bois_mort_total_med = median(bois_mort_total, na.rm = TRUE),
      bois_mort_total_se  = se(bois_mort_total),
      
      bois_mort_sur_pied_moy = mean(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_med = median(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_se  = se(bmsp_vhatot),
      
      bois_mort_au_sol_moy = mean(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_med = median(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_se  = se(bmt_vha_tot),
      
      .groups = "drop"
    ) %>%
    arrange(peup_pl)
}

make_indic_peup_no0 <- function(df) {
  df %>%
    filter(bois_mort_total > 0) %>%
    group_by(peup_pl) %>%
    summarise(
      n_placettes_avec_bois_mort = n_distinct(plot_id),
      
      bois_mort_total_moy = mean(bois_mort_total, na.rm = TRUE),
      bois_mort_total_med = median(bois_mort_total, na.rm = TRUE),
      bois_mort_total_se  = se(bois_mort_total),
      
      bois_mort_sur_pied_moy = mean(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_med = median(bmsp_vhatot, na.rm = TRUE),
      bois_mort_sur_pied_se  = se(bmsp_vhatot),
      
      bois_mort_au_sol_moy = mean(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_med = median(bmt_vha_tot, na.rm = TRUE),
      bois_mort_au_sol_se  = se(bmt_vha_tot),
      
      .groups = "drop"
    ) %>%
    arrange(peup_pl)
}

extract_points_from_mask <- function(mask_path, plots_sf, out_gpkg, layer_name) {
  mask_rast <- rast(mask_path)
  
  pts_crs <- st_transform(plots_sf, crs(mask_rast))
  ext <- terra::extract(mask_rast, vect(pts_crs))
  val_col <- names(ext)[2]
  
  pts_sel <- pts_crs[
    !is.na(ext[[val_col]]) & ext[[val_col]] == 1,
  ]
  
  pts_sel <- st_transform(pts_sel, 4326)
  pts_df <- st_drop_geometry(pts_sel)
  
  st_write(
    pts_sel,
    out_gpkg,
    layer = layer_name,
    delete_layer = TRUE
  )
  
  list(sf = pts_sel, df = pts_df)
}

# =========================================================
# 2) FICHIERS
# =========================================================

f_bmsp  <- "C:/Old_Growth_Forest/traitement_donnees_iprfw/data/20260302_forets_matures_plots_BMSP.csv"
f_bmt   <- "C:/Old_Growth_Forest/traitement_donnees_iprfw/data/20260302_forets_matures_plots_BMT.csv"
f_plots <- "C:/Old_Growth_Forest/traitement_donnees_iprfw/data/20260202_requete_foret_mature_plots.csv"

f_mask_fa  <- "C:/Old_Growth_Forest/Carto_OGF/raw/FA_DLG.tif"
f_mask_q70 <- "C:/Old_Growth_Forest/Carto_OGF/outputs_REBOOT_DLG/status_maps_DLG/STATUS_MIX_q70_DLG.tif"
f_mask_q90 <- "C:/Old_Growth_Forest/Carto_OGF/outputs_REBOOT_DLG/status_maps_DLG/STATUS_MIX_q90_DLG.tif"

f_mask_bourd_q70 <- "C:/Old_Growth_Forest/Carto_OGF/outputs_BOURDOUXHE_TERRxHAB5_CLEAN_DLG/status_maps_DLG/STATUS_TERRxHAB5_q70_DLG.tif"
f_mask_bourd_q90 <- "C:/Old_Growth_Forest/Carto_OGF/outputs_BOURDOUXHE_TERRxHAB5_CLEAN_DLG/status_maps_DLG/STATUS_TERRxHAB5_q90_DLG.tif"

out_gpkg_fa  <- "C:/Old_Growth_Forest/Carto_OGF/raw/plots_FA_selected.gpkg"
out_gpkg_q70 <- "C:/Old_Growth_Forest/Carto_OGF/raw/plots_STATUS_MIX_q70_DLG_selected.gpkg"
out_gpkg_q90 <- "C:/Old_Growth_Forest/Carto_OGF/raw/plots_STATUS_MIX_q90_DLG_selected.gpkg"

out_gpkg_bourd_q70 <- "C:/Old_Growth_Forest/Carto_OGF/raw/plots_STATUS_TERRxHAB5_q70_DLG_selected.gpkg"
out_gpkg_bourd_q90 <- "C:/Old_Growth_Forest/Carto_OGF/raw/plots_STATUS_TERRxHAB5_q90_DLG_selected.gpkg"

f_out_all <- "C:/Old_Growth_Forest/traitement_donnees_iprfw/data/resultats_deadwood_iprfw_RW_FA_q70_q90_BOURD.xlsx"

# =========================================================
# 3) LECTURE DES DONNEES
# =========================================================

bmsp  <- read_any_csv(f_bmsp)
bmt   <- read_any_csv(f_bmt)
plots <- read_any_csv(f_plots)

# =========================================================
# 4) JOINTURE BOIS MORT SUR PLOTS
# =========================================================

bmsp_keep <- bmsp %>%
  group_by(plot_id) %>%
  summarise(
    bmsp_vhatot = sum(vhatot, na.rm = TRUE),
    .groups = "drop"
  )

bmt_keep <- bmt %>%
  group_by(plot_id) %>%
  summarise(
    bmt_vha_tot = sum(vha_tot, na.rm = TRUE),
    .groups = "drop"
  )

plots_dw <- plots %>%
  left_join(bmsp_keep, by = "plot_id") %>%
  left_join(bmt_keep, by = "plot_id") %>%
  mutate(
    bmsp_vhatot = coalesce(bmsp_vhatot, 0),
    bmt_vha_tot = coalesce(bmt_vha_tot, 0),
    bois_mort_total = bmsp_vhatot + bmt_vha_tot
  )

# =========================================================
# 5) CREATION DES POINTS
# =========================================================

plots_pts <- plots_dw %>%
  mutate(
    x_coord = coalesce(x_gps, longitude_theo),
    y_coord = coalesce(y_gps, latitude_theo)
  ) %>%
  filter(!is.na(x_coord), !is.na(y_coord))

plots_sf <- st_as_sf(
  plots_pts,
  coords = c("x_coord", "y_coord"),
  crs = 4326,
  remove = FALSE
)

# =========================================================
# 6) EXTRACTION DES POINTS
# =========================================================

res_fa <- extract_points_from_mask(
  mask_path = f_mask_fa,
  plots_sf = plots_sf,
  out_gpkg = out_gpkg_fa,
  layer_name = "plots_FA_selected"
)
plots_fa_sf <- res_fa$sf
plots_fa <- res_fa$df

res_q70 <- extract_points_from_mask(
  mask_path = f_mask_q70,
  plots_sf = plots_sf,
  out_gpkg = out_gpkg_q70,
  layer_name = "plots_STATUS_MIX_q70_DLG"
)
plots_q70_sf <- res_q70$sf
plots_q70 <- res_q70$df

res_q90 <- extract_points_from_mask(
  mask_path = f_mask_q90,
  plots_sf = plots_sf,
  out_gpkg = out_gpkg_q90,
  layer_name = "plots_STATUS_MIX_q90_DLG"
)
plots_q90_sf <- res_q90$sf
plots_q90 <- res_q90$df

res_bourd_q70 <- extract_points_from_mask(
  mask_path = f_mask_bourd_q70,
  plots_sf = plots_sf,
  out_gpkg = out_gpkg_bourd_q70,
  layer_name = "plots_STATUS_TERRxHAB5_q70_DLG"
)
plots_bourd_q70_sf <- res_bourd_q70$sf
plots_bourd_q70 <- res_bourd_q70$df

res_bourd_q90 <- extract_points_from_mask(
  mask_path = f_mask_bourd_q90,
  plots_sf = plots_sf,
  out_gpkg = out_gpkg_bourd_q90,
  layer_name = "plots_STATUS_TERRxHAB5_q90_DLG"
)
plots_bourd_q90_sf <- res_bourd_q90$sf
plots_bourd_q90 <- res_bourd_q90$df

# =========================================================
# 7) INDICATEURS
# =========================================================

# Wallonie
indicateurs_wallonie <- make_indic_global(plots_dw)
indicateurs_wallonie_sans_zero <- make_indic_global_no0(plots_dw)
indicateurs_wallonie_par_peuplement <- make_indic_peup(plots_dw)
indicateurs_wallonie_par_peuplement_sans_zero <- make_indic_peup_no0(plots_dw)

# FA
indicateurs_fa <- make_indic_global(plots_fa)
indicateurs_fa_sans_zero <- make_indic_global_no0(plots_fa)
indicateurs_fa_par_peuplement <- make_indic_peup(plots_fa)
indicateurs_fa_par_peuplement_sans_zero <- make_indic_peup_no0(plots_fa)

# FA q70 DLG
indicateurs_q70 <- make_indic_global(plots_q70)
indicateurs_q70_sans_zero <- make_indic_global_no0(plots_q70)
indicateurs_q70_par_peuplement <- make_indic_peup(plots_q70)
indicateurs_q70_par_peuplement_sans_zero <- make_indic_peup_no0(plots_q70)

# FA q90 DLG
indicateurs_q90 <- make_indic_global(plots_q90)
indicateurs_q90_sans_zero <- make_indic_global_no0(plots_q90)
indicateurs_q90_par_peuplement <- make_indic_peup(plots_q90)
indicateurs_q90_par_peuplement_sans_zero <- make_indic_peup_no0(plots_q90)

# Bourdouxhe q70
indicateurs_bourd_q70 <- make_indic_global(plots_bourd_q70)
indicateurs_bourd_q70_sans_zero <- make_indic_global_no0(plots_bourd_q70)
indicateurs_bourd_q70_par_peuplement <- make_indic_peup(plots_bourd_q70)
indicateurs_bourd_q70_par_peuplement_sans_zero <- make_indic_peup_no0(plots_bourd_q70)

# Bourdouxhe q90
indicateurs_bourd_q90 <- make_indic_global(plots_bourd_q90)
indicateurs_bourd_q90_sans_zero <- make_indic_global_no0(plots_bourd_q90)
indicateurs_bourd_q90_par_peuplement <- make_indic_peup(plots_bourd_q90)
indicateurs_bourd_q90_par_peuplement_sans_zero <- make_indic_peup_no0(plots_bourd_q90)

# =========================================================
# 8) CONTROLE EFFECTIFS
# =========================================================

comparaison_effectifs_all <- tibble(
  jeu = c(
    "Wallonie",
    "FA",
    "FA q70 DLG",
    "FA q90 DLG",
    "Bourdouxhe q70",
    "Bourdouxhe q90"
  ),
  n_placettes = c(
    n_distinct(plots_dw$plot_id),
    n_distinct(plots_fa$plot_id),
    n_distinct(plots_q70$plot_id),
    n_distinct(plots_q90$plot_id),
    n_distinct(plots_bourd_q70$plot_id),
    n_distinct(plots_bourd_q90$plot_id)
  )
)

comparaison_effectifs_all

# =========================================================
# 9) TABLEAU DE COMPARAISON GLOBAL
# =========================================================

comparaison_wallonie_all <- bind_rows(
  indicateurs_wallonie %>% mutate(zone = "Wallonie"),
  indicateurs_wallonie_sans_zero %>% mutate(zone = "Wallonie (BM > 0)"),
  
  indicateurs_fa %>% mutate(zone = "FA"),
  indicateurs_fa_sans_zero %>% mutate(zone = "FA (BM > 0)"),
  
  indicateurs_q70 %>% mutate(zone = "FA q70 DLG"),
  indicateurs_q70_sans_zero %>% mutate(zone = "FA q70 DLG (BM > 0)"),
  
  indicateurs_q90 %>% mutate(zone = "FA q90 DLG"),
  indicateurs_q90_sans_zero %>% mutate(zone = "FA q90 DLG (BM > 0)"),
  
  indicateurs_bourd_q70 %>% mutate(zone = "Bourdouxhe q70"),
  indicateurs_bourd_q70_sans_zero %>% mutate(zone = "Bourdouxhe q70 (BM > 0)"),
  
  indicateurs_bourd_q90 %>% mutate(zone = "Bourdouxhe q90"),
  indicateurs_bourd_q90_sans_zero %>% mutate(zone = "Bourdouxhe q90 (BM > 0)")
) %>%
  relocate(zone)

comparaison_wallonie_all

# =========================================================
# 10) NETTOYAGE UTF-8
# =========================================================

comparaison_effectifs_all <- to_utf8_df(comparaison_effectifs_all)
comparaison_wallonie_all <- to_utf8_df(comparaison_wallonie_all)

indicateurs_wallonie <- to_utf8_df(indicateurs_wallonie)
indicateurs_wallonie_sans_zero <- to_utf8_df(indicateurs_wallonie_sans_zero)
indicateurs_wallonie_par_peuplement <- to_utf8_df(indicateurs_wallonie_par_peuplement)
indicateurs_wallonie_par_peuplement_sans_zero <- to_utf8_df(indicateurs_wallonie_par_peuplement_sans_zero)

indicateurs_fa <- to_utf8_df(indicateurs_fa)
indicateurs_fa_sans_zero <- to_utf8_df(indicateurs_fa_sans_zero)
indicateurs_fa_par_peuplement <- to_utf8_df(indicateurs_fa_par_peuplement)
indicateurs_fa_par_peuplement_sans_zero <- to_utf8_df(indicateurs_fa_par_peuplement_sans_zero)

indicateurs_q70 <- to_utf8_df(indicateurs_q70)
indicateurs_q70_sans_zero <- to_utf8_df(indicateurs_q70_sans_zero)
indicateurs_q70_par_peuplement <- to_utf8_df(indicateurs_q70_par_peuplement)
indicateurs_q70_par_peuplement_sans_zero <- to_utf8_df(indicateurs_q70_par_peuplement_sans_zero)

indicateurs_q90 <- to_utf8_df(indicateurs_q90)
indicateurs_q90_sans_zero <- to_utf8_df(indicateurs_q90_sans_zero)
indicateurs_q90_par_peuplement <- to_utf8_df(indicateurs_q90_par_peuplement)
indicateurs_q90_par_peuplement_sans_zero <- to_utf8_df(indicateurs_q90_par_peuplement_sans_zero)

indicateurs_bourd_q70 <- to_utf8_df(indicateurs_bourd_q70)
indicateurs_bourd_q70_sans_zero <- to_utf8_df(indicateurs_bourd_q70_sans_zero)
indicateurs_bourd_q70_par_peuplement <- to_utf8_df(indicateurs_bourd_q70_par_peuplement)
indicateurs_bourd_q70_par_peuplement_sans_zero <- to_utf8_df(indicateurs_bourd_q70_par_peuplement_sans_zero)

indicateurs_bourd_q90 <- to_utf8_df(indicateurs_bourd_q90)
indicateurs_bourd_q90_sans_zero <- to_utf8_df(indicateurs_bourd_q90_sans_zero)
indicateurs_bourd_q90_par_peuplement <- to_utf8_df(indicateurs_bourd_q90_par_peuplement)
indicateurs_bourd_q90_par_peuplement_sans_zero <- to_utf8_df(indicateurs_bourd_q90_par_peuplement_sans_zero)

# =========================================================
# 11) EXPORT EXCEL
# =========================================================

wb_all <- createWorkbook()

addWorksheet(wb_all, "comp_all")
addWorksheet(wb_all, "effectifs")

addWorksheet(wb_all, "RW_global")
addWorksheet(wb_all, "RW_global_no0")
addWorksheet(wb_all, "RW_peup")
addWorksheet(wb_all, "RW_peup_no0")

addWorksheet(wb_all, "FA_global")
addWorksheet(wb_all, "FA_global_no0")
addWorksheet(wb_all, "FA_peup")
addWorksheet(wb_all, "FA_peup_no0")

addWorksheet(wb_all, "q70_global")
addWorksheet(wb_all, "q70_global_no0")
addWorksheet(wb_all, "q70_peup")
addWorksheet(wb_all, "q70_peup_no0")

addWorksheet(wb_all, "q90_global")
addWorksheet(wb_all, "q90_global_no0")
addWorksheet(wb_all, "q90_peup")
addWorksheet(wb_all, "q90_peup_no0")

addWorksheet(wb_all, "bourd_q70_glob")
addWorksheet(wb_all, "bourd_q70_no0")
addWorksheet(wb_all, "bourd_q70_peup")
addWorksheet(wb_all, "bourd_q70_p_no0")

addWorksheet(wb_all, "bourd_q90_glob")
addWorksheet(wb_all, "bourd_q90_no0")
addWorksheet(wb_all, "bourd_q90_peup")
addWorksheet(wb_all, "bourd_q90_p_no0")

writeData(wb_all, "comp_all", comparaison_wallonie_all)
writeData(wb_all, "effectifs", comparaison_effectifs_all)

writeData(wb_all, "RW_global", indicateurs_wallonie)
writeData(wb_all, "RW_global_no0", indicateurs_wallonie_sans_zero)
writeData(wb_all, "RW_peup", indicateurs_wallonie_par_peuplement)
writeData(wb_all, "RW_peup_no0", indicateurs_wallonie_par_peuplement_sans_zero)

writeData(wb_all, "FA_global", indicateurs_fa)
writeData(wb_all, "FA_global_no0", indicateurs_fa_sans_zero)
writeData(wb_all, "FA_peup", indicateurs_fa_par_peuplement)
writeData(wb_all, "FA_peup_no0", indicateurs_fa_par_peuplement_sans_zero)

writeData(wb_all, "q70_global", indicateurs_q70)
writeData(wb_all, "q70_global_no0", indicateurs_q70_sans_zero)
writeData(wb_all, "q70_peup", indicateurs_q70_par_peuplement)
writeData(wb_all, "q70_peup_no0", indicateurs_q70_par_peuplement_sans_zero)

writeData(wb_all, "q90_global", indicateurs_q90)
writeData(wb_all, "q90_global_no0", indicateurs_q90_sans_zero)
writeData(wb_all, "q90_peup", indicateurs_q90_par_peuplement)
writeData(wb_all, "q90_peup_no0", indicateurs_q90_par_peuplement_sans_zero)

writeData(wb_all, "bourd_q70_glob", indicateurs_bourd_q70)
writeData(wb_all, "bourd_q70_no0", indicateurs_bourd_q70_sans_zero)
writeData(wb_all, "bourd_q70_peup", indicateurs_bourd_q70_par_peuplement)
writeData(wb_all, "bourd_q70_p_no0", indicateurs_bourd_q70_par_peuplement_sans_zero)

writeData(wb_all, "bourd_q90_glob", indicateurs_bourd_q90)
writeData(wb_all, "bourd_q90_no0", indicateurs_bourd_q90_sans_zero)
writeData(wb_all, "bourd_q90_peup", indicateurs_bourd_q90_par_peuplement)
writeData(wb_all, "bourd_q90_p_no0", indicateurs_bourd_q90_par_peuplement_sans_zero)

saveWorkbook(wb_all, f_out_all, overwrite = TRUE)