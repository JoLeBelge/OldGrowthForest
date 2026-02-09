# =========================================================
# IPRFW -> CDOM + typologie_mature_simplifiee + SENSIBILITE AUX SEUILS
# - filtre "feuillus indigènes" via ess_max (plus gros arbre)
# - calcule CDOM (10 tiges/ha)
# - calcule essmaj_mature_40_code (top 40 tiges/ha, top2 %GHA)
# - typologie_mature_simplifiee (mêmes règles que terrain)
# - applique seuils terrain multi-quantiles (q05/q10/q25/q50/q75)
# - masque forêt ancienne (FA) appliqué à TOUTES les quantiles
# - sorties : CSV + PDF (dans FA)
# Proxy surface : 1 point = 50 ha
# =========================================================

library(sf)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(terra)

# -------------------------
# PARAMETRES (à adapter si besoin)
# -------------------------
HA_PER_POINT <- 50

gpkg_plots  <- "C:/Old_Growth_Forest/DATA/données_iprfw/ifw_plots.gpkg"
gpkg_trees  <- "C:/Old_Growth_Forest/DATA/données_iprfw/ifw_plots_trees_coppices.gpkg"

# seuils CDOM issus du terrain (script terrain multi-quantiles)
path_seuils <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/outputs/seuils_cdom_q05_q10_q25_q50_q75_q90_par_typologie_mature_simplifiee.csv"

# raster masque forêt ancienne (1 = FA, ailleurs NA/0)
mask_path <- "C:/Old_Growth_Forest/raster_couchesforestimator/raster/raster/FA_mask12_01.tif"

out_dir <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/outputs"

# quantiles à utiliser pour la sensibilité
quantiles_keep <- c("q05","q10","q25","q50","q75")

# exclusions essences (plus gros arbre)
codes_exclure <- c(2, 24) # chêne rouge, peuplier hybride

# paramètres CDOM / strate mature
n_tree_cdom <- 10
n_tree_mature <- 40

# -------------------------
# petites fonctions
# -------------------------
# (i) moyenne pondérée sur N tiges/ha (ici CDOM) : découpe par densité cumulée (NHA)
weighted_topN_by_plot <- function(df, N_target, value_col = "CIR", weight_col = "NHA") {
  df %>%
    filter(is.finite(.data[[value_col]]), .data[[value_col]] > 0,
           is.finite(.data[[weight_col]]), .data[[weight_col]] > 0) %>%
    group_by(IGN, NPL) %>%
    arrange(desc(.data[[value_col]]), .by_group = TRUE) %>%
    mutate(
      cumN = cumsum(.data[[weight_col]]),
      idd = row_number(),
      whup = cumN > N_target,
      whup2 = ifelse(any(whup), which(whup)[1], NA_integer_),
      cumN2 = ifelse(whup, N_target, cumN),
      diff = N_target - lag(cumN2, default = 0),
      w2 = case_when(
        !is.na(whup2) & idd < whup2  ~ .data[[weight_col]],
        !is.na(whup2) & idd == whup2 ~ diff,
        is.na(whup2)                ~ .data[[weight_col]],
        TRUE                        ~ 0
      ),
      vw = .data[[value_col]] * w2
    ) %>%
    summarise(val = sum(vw, na.rm = TRUE) / sum(w2, na.rm = TRUE), .groups = "drop")
}

# (ii) parser "code-pct%;code-pct%" en 2 codes + 2 %
parser_essmaj_num <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- str_replace_all(x, "\\s+", "")
  code1 <- suppressWarnings(as.integer(str_match(x, "^([0-9]+)-")[,2]))
  p1    <- suppressWarnings(as.numeric(str_match(x, "-([0-9]+)\\%")[,2]))
  code2 <- suppressWarnings(as.integer(str_match(x, "\\%;([0-9]+)-")[,2]))
  p2    <- suppressWarnings(as.numeric(str_match(x, "\\%;[0-9]+-([0-9]+)\\%")[,2]))
  p1[is.na(p1)] <- 0
  p2[is.na(p2)] <- 0
  tibble(code1 = code1, p1 = p1, code2 = code2, p2 = p2)
}

# =========================================================
# 1) LECTURE
# =========================================================
plots <- st_read(gpkg_plots, layer = "ifw_plots", quiet = TRUE)
trees <- st_read(gpkg_trees, layer = "ifw_plots_trees_coppices", quiet = TRUE) %>%
  st_drop_geometry()

# =========================================================
# 2) FILTRE "FEUILLUS INDIGENES" via ess_max (plus gros arbre)
# =========================================================
essence_max_tree <- trees %>%
  filter(!is.na(IGN), !is.na(NPL),
         !is.na(CIR), CIR > 0,
         !is.na(ESS), ESS != "",
         !is.na(ESS_D), ESS_D != "") %>%
  mutate(ESS_code = suppressWarnings(as.integer(ESS))) %>%
  filter(!is.na(ESS_code)) %>%
  group_by(IGN, NPL) %>%
  slice_max(order_by = CIR, n = 1, with_ties = FALSE) %>%
  transmute(IGN, NPL,
            ess_max_code = ESS_code,
            ess_max = ESS_D) %>%
  ungroup()

plots_exclus <- essence_max_tree %>%
  filter(ess_max_code > 40 | ess_max_code %in% codes_exclure) %>%
  distinct(IGN, NPL)

plots_ok <- plots %>% anti_join(plots_exclus, by = c("IGN","NPL"))
trees_ok <- trees %>% anti_join(plots_exclus, by = c("IGN","NPL"))

# =========================================================
# 3) CDOM (10 tiges/ha)
# =========================================================
cdom_by_plot <- weighted_topN_by_plot(
  trees_ok,
  N_target = n_tree_cdom,
  value_col = "CIR",
  weight_col = "NHA"
) %>%
  rename(cdom = val)

# =========================================================
# 4) ESSMAJ_MATURE_40_CODE (top40 tiges/ha, top2 %GHA)
# =========================================================
trees_top40 <- trees_ok %>%
  filter(!is.na(CIR), CIR > 0,
         !is.na(NHA), NHA > 0,
         !is.na(GHA), GHA > 0,
         !is.na(ESS), ESS != "",
         !is.na(ESS_D), ESS_D != "") %>%
  mutate(ESS_code = suppressWarnings(as.integer(ESS))) %>%
  filter(!is.na(ESS_code)) %>%
  group_by(IGN, NPL) %>%
  arrange(desc(CIR), .by_group = TRUE) %>%
  mutate(
    cumN = cumsum(NHA),
    idd = row_number(),
    whup = cumN > n_tree_mature,
    whup2 = ifelse(any(whup), which(whup)[1], NA_integer_),
    cumN2 = ifelse(whup, n_tree_mature, cumN),
    diff = n_tree_mature - lag(cumN2, default = 0),
    NHA2 = case_when(
      !is.na(whup2) & idd < whup2  ~ NHA,
      !is.na(whup2) & idd == whup2 ~ diff,
      is.na(whup2)                ~ NHA,
      TRUE                        ~ 0
    ),
    GHA2 = GHA * (NHA2 / NHA)
  ) %>%
  filter(NHA2 > 0, GHA2 > 0) %>%
  ungroup()

gha_ess_mature <- trees_top40 %>%
  group_by(IGN, NPL, ESS_code, ESS_D) %>%
  summarise(GHA_mature = sum(GHA2, na.rm = TRUE), .groups = "drop")

gha_tot_mature <- gha_ess_mature %>%
  group_by(IGN, NPL) %>%
  summarise(GHA_mature_tot = sum(GHA_mature, na.rm = TRUE), .groups = "drop")

essmaj_mature_40 <- gha_ess_mature %>%
  left_join(gha_tot_mature, by = c("IGN","NPL")) %>%
  mutate(pct = 100 * GHA_mature / GHA_mature_tot) %>%
  group_by(IGN, NPL) %>%
  arrange(desc(pct), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  mutate(pct_r = round(pct, 0)) %>%
  summarise(
    essmaj_mature_40_code = {
      keep <- which(pct_r > 0)
      if (length(keep) == 0) NA_character_
      else paste0(paste0(ESS_code[keep], "-", pct_r[keep], "%"), collapse = ";")
    },
    .groups = "drop"
  )

# =========================================================
# 5) TAB_FINAL + TYPOLOGIE MATURE SIMPLIFIEE
# =========================================================
tab_final <- plots_ok %>%
  select(IGN, NPL, geom) %>%
  left_join(cdom_by_plot, by = c("IGN","NPL")) %>%
  left_join(essmaj_mature_40, by = c("IGN","NPL")) %>%
  filter(!is.na(cdom), !is.na(essmaj_mature_40_code), essmaj_mature_40_code != "")

# règles typologie
seuil_pur <- 66.7
seuil_autre <- 50
seuil_dom <- 50

code_he <- 3
code_fr <- 5
codes_chene <- c(1)
codes_erable <- c(4,16,17)

codes_nobles <- c(code_he, codes_chene, code_fr, codes_erable, 7, 2)
codes_feuillus <- c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,
                    24,25,26,27,28,29,30,32,33,35)

typologie_mature_simplifiee_une_ligne <- function(code1, p1, code2, p2) {
  p1 <- ifelse(is.na(p1), 0, p1)
  p2 <- ifelse(is.na(p2), 0, p2)
  
  nobles <- ifelse(!is.na(code1) & code1 %in% codes_nobles, p1, 0) +
    ifelse(!is.na(code2) & code2 %in% codes_nobles, p2, 0)
  
  feuillus <- ifelse(!is.na(code1) & code1 %in% codes_feuillus, p1, 0) +
    ifelse(!is.na(code2) & code2 %in% codes_feuillus, p2, 0)
  
  if (p1 >= seuil_dom) {
    if (!is.na(code1) && code1 == code_he) return("Hetraie")
    if (!is.na(code1) && code1 %in% codes_chene) return("Chenaie")
    if (!is.na(code1) && code1 == code_fr) return("Frenaie")
    if (!is.na(code1) && code1 %in% codes_erable) return("Erabliere")
    if (!is.na(code1) && code1 %in% codes_nobles) return("Autres feuillus nobles")
    if (!is.na(code1) && code1 %in% codes_feuillus) return("Autres peuplements feuillus")
    return("Peuplement melange")
  }
  
  if (nobles >= seuil_pur) return("Autres feuillus nobles")
  if (feuillus >= seuil_autre) return("Autres peuplements feuillus")
  "Peuplement melange"
}

parsed <- parser_essmaj_num(tab_final$essmaj_mature_40_code)

tab_final <- tab_final %>%
  bind_cols(parsed) %>%
  mutate(
    typologie_mature_simplifiee = mapply(
      typologie_mature_simplifiee_une_ligne, code1, p1, code2, p2
    )
  ) %>%
  select(-code1, -p1, -code2, -p2) %>%
  filter(typologie_mature_simplifiee != "Peuplement melange")

# =========================================================
# 6) SEUILS MULTI-QUANTILES (on prépare seuils_long une seule fois)
# =========================================================
seuils_multi <- read.csv2(path_seuils, stringsAsFactors = FALSE) %>%
  rename(typologie_mature_simplifiee = typologie) %>%
  select(typologie_mature_simplifiee, any_of(quantiles_keep))

seuils_long <- seuils_multi %>%
  pivot_longer(cols = starts_with("q"), names_to = "quantile", values_to = "seuil_cdom") %>%
  mutate(q = as.numeric(sub("^q", "0.", quantile)))

# =========================================================
# 7) MASQUE FA APPLIQUE A TOUS LES POINTS + SENSIBILITE DANS FA
# =========================================================
out_csv_recap_multi_fa <- file.path(out_dir, "sensibilite_iprfw_dans_FA_par_typologie.csv")
out_csv_recap_glob_fa  <- file.path(out_dir, "sensibilite_iprfw_dans_FA_global.csv")
out_pdf_sens_fa        <- file.path(out_dir, "graph_sensibilite_iprfw_dans_FA.pdf")

mask_fa <- rast(mask_path)

# reprojection si besoin
if (!is.na(st_crs(tab_final)) && !is.na(crs(mask_fa))) {
  if (st_crs(tab_final)$wkt != crs(mask_fa)) {
    tab_final <- st_transform(tab_final, crs(mask_fa))
  }
}

val <- terra::extract(mask_fa, terra::vect(tab_final))
maskcol <- names(val)[2]
tab_final$fa_mask <- val[[maskcol]]

tab_fa <- tab_final %>% filter(!is.na(fa_mask) & fa_mask == 1)

tab_scored_fa <- tab_fa %>%
  left_join(seuils_long, by = "typologie_mature_simplifiee", relationship = "many-to-many") %>%
  mutate(pass = cdom >= seuil_cdom)

sens_by_typo_fa <- tab_scored_fa %>%
  group_by(typologie_mature_simplifiee, q, quantile) %>%
  summarise(
    n_avant = n(),
    n_apres = sum(pass, na.rm = TRUE),
    ha_avant = n_avant * HA_PER_POINT,
    ha_apres = n_apres * HA_PER_POINT,
    .groups = "drop"
  ) %>%
  mutate(pct_surface = 100 * ha_apres / ha_avant) %>%
  arrange(typologie_mature_simplifiee, q)

sens_global_fa <- sens_by_typo_fa %>%
  group_by(q, quantile) %>%
  summarise(
    n_avant = sum(n_avant),
    n_apres = sum(n_apres),
    ha_avant = sum(ha_avant),
    ha_apres = sum(ha_apres),
    .groups = "drop"
  ) %>%
  mutate(pct_surface = 100 * ha_apres / ha_avant) %>%
  arrange(q)

# graph FA (optionnel mais tu l'avais demandé : PDF FA seulement)
p_pct_fa <- ggplot(
  sens_by_typo_fa,
  aes(x = q, y = pct_surface,
      group = typologie_mature_simplifiee,
      color = typologie_mature_simplifiee)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(.05,.10,.25,.50,.75)) +
  theme_minimal() +
  labs(title = "Sensibilité aux seuils CDOM — dans forêt ancienne (FA)",
       x = "Quantile terrain utilisé comme seuil CDOM",
       y = "% surface retenue (proxy 50 ha/point)",
       color = "Typologie")

p_pct_glob_fa <- ggplot(sens_global_fa, aes(x = q, y = pct_surface)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(.05,.10,.25,.50,.75)) +
  theme_minimal() +
  labs(title = "Sensibilité globale — dans forêt ancienne (FA)",
       x = "Quantile terrain utilisé comme seuil CDOM",
       y = "% surface retenue (proxy 50 ha/point)")

# exports CSV lisibles (on drop la géométrie)

write.csv2(
  sens_by_typo_fa %>% sf::st_drop_geometry(),
  out_csv_recap_multi_fa,
  row.names = FALSE
)

write.csv2(
  sens_global_fa %>% sf::st_drop_geometry(),
  out_csv_recap_glob_fa,
  row.names = FALSE
)

pdf(out_pdf_sens_fa, width = 11, height = 8.5)
print(p_pct_fa)
print(p_pct_glob_fa)
dev.off()

cat("OK — exports FA écrits dans ", out_dir, "\n")




# =========================================================
# TABLEAU LARGE "COMME EXCEL" (tous quantiles)
# -> 1 ligne par typologie
# -> colonnes : qXX_cdom + n_avant + n_apres_qXX + pct_gardes_qXX
# =========================================================

# 1) table des seuils CDOM en large (q05_cdom, q10_cdom, ...)
seuils_wide <- seuils_long %>%
  select(typologie_mature_simplifiee, quantile, seuil_cdom) %>%
  mutate(quantile = paste0(quantile, "_cdom")) %>%
  pivot_wider(names_from = quantile, values_from = seuil_cdom)

# 2) n_avant (une seule fois par typologie)
n_avant_by_typo_fa <- tab_fa %>%
  st_drop_geometry() %>%
  count(typologie_mature_simplifiee, name = "n_avant")

# 3) n_apres + pct par quantile, en large
res_wide_fa <- sens_by_typo_fa %>%
  select(typologie_mature_simplifiee, quantile, n_apres, pct_surface) %>%
  mutate(
    n_apres_name = paste0("n_apres_", quantile),
    pct_name     = paste0("pct_gardes_", quantile)
  ) %>%
  select(typologie_mature_simplifiee, n_apres_name, n_apres, pct_name, pct_surface) %>%
  pivot_longer(cols = c(n_apres_name, pct_name), names_to = "tmp", values_to = "colname") %>%
  mutate(value = ifelse(tmp == "n_apres_name", n_apres, pct_surface)) %>%
  select(typologie_mature_simplifiee, colname, value) %>%
  pivot_wider(names_from = colname, values_from = value)

# 4) assemblage final
tableau_excel_fa <- n_avant_by_typo_fa %>%
  left_join(seuils_wide, by = "typologie_mature_simplifiee") %>%
  left_join(res_wide_fa, by = "typologie_mature_simplifiee") %>%
  arrange(typologie_mature_simplifiee)

# 5) export CSV (sans geom)
out_csv_tableau_excel_fa <- file.path(out_dir, "tableau_excel_sensibilite_FA.csv")

tableau_excel_fa_no_geom <- tableau_excel_fa %>%
  dplyr::select(-dplyr::any_of(c("geom", "geometry")))

write.csv2(tableau_excel_fa_no_geom, out_csv_tableau_excel_fa, row.names = FALSE)
cat("OK — tableau (sans geom) écrit :", out_csv_tableau_excel_fa, "\n")
