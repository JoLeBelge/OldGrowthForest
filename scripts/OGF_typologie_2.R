# =========================================================
# OGF_all.db : AJOUT typologie_mature_simplifiee (SAFE UPDATE par clés)
# + récupération de nha_tgb depuis dendro_stand
# - ne modifie PAS le nombre de lignes de dendro_plot
# - update par (ues_id_ogf, ues_id_ue), pas par rowid
# =========================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(openxlsx)

chemin_bd <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"

# ----------------------------
# 1) Seuils
# ----------------------------
seuil_pur   <- 66.7
seuil_autre <- 50
seuil_dom   <- 50

# ----------------------------
# 2) Codes
# ----------------------------
codes_chene  <- c("CH","CP","CS","CHs")
codes_erable <- c("ER","EP","ES")

codes_nobles <- c("HE", codes_chene, "FR", codes_erable, "MR", "CR")
codes_feuillus <- c("HE", codes_chene, "FR", codes_erable, "MR", "CR",
                    "CA","AUs","BOU","BOs","TIs","PPN","PG")

# ----------------------------
# 3) Parser essmaj
# ----------------------------
parser_essmaj <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- str_replace_all(x, "\\s+", "")
  
  code1 <- str_match(x, "^([A-Za-z]+)-")[,2]
  p1    <- suppressWarnings(as.numeric(str_match(x, "-([0-9]+)\\%")[,2]))
  
  code2 <- str_match(x, "\\%;([A-Za-z]+)-")[,2]
  p2    <- suppressWarnings(as.numeric(str_match(x, "\\%;[A-Za-z]+-([0-9]+)\\%")[,2]))
  
  code1[is.na(code1)] <- ""
  code2[is.na(code2)] <- ""
  p1[is.na(p1)] <- 0
  p2[is.na(p2)] <- 0
  
  tibble(code1 = code1, p1 = p1, code2 = code2, p2 = p2)
}

# ----------------------------
# 4) Typologie mature (ancienne)
# ----------------------------
typologie_une_ligne <- function(code1, p1, code2, p2) {
  code1 <- str_trim(ifelse(is.na(code1), "", code1))
  code2 <- str_trim(ifelse(is.na(code2), "", code2))
  p1 <- ifelse(is.na(p1), 0, p1)
  p2 <- ifelse(is.na(p2), 0, p2)
  
  if (code1 %in% codes_erable) return("Erabliere")
  
  total_chene  <- ifelse(code1 %in% codes_chene, p1, 0) + ifelse(code2 %in% codes_chene, p2, 0)
  total_erable <- ifelse(code1 %in% codes_erable, p1, 0) + ifelse(code2 %in% codes_erable, p2, 0)
  total_he     <- ifelse(code1 == "HE", p1, 0) + ifelse(code2 == "HE", p2, 0)
  total_fr     <- ifelse(code1 == "FR", p1, 0) + ifelse(code2 == "FR", p2, 0)
  total_mr     <- ifelse(code1 == "MR", p1, 0) + ifelse(code2 == "MR", p2, 0)
  total_cr     <- ifelse(code1 == "CR", p1, 0) + ifelse(code2 == "CR", p2, 0)
  
  if (total_chene  >= seuil_pur) return("Chenaie")
  if (total_he     >= seuil_pur) return("Hetraie")
  if (total_fr     >= seuil_pur) return("Frenaie")
  if (total_erable >= seuil_pur) return("Erabliere")
  if (total_mr     >= seuil_pur) return("Merisier")
  if (total_cr     >= seuil_pur) return("Chene rouge")
  
  if ((total_he + total_chene) >= seuil_pur && total_he < seuil_pur && total_chene < seuil_pur) return("Hetre - Chene")
  if ((total_fr + total_chene) >= seuil_pur && total_fr < seuil_pur && total_chene < seuil_pur) return("Chene - Frene")
  
  nobles_visibles <- ifelse(code1 %in% codes_nobles, p1, 0) + ifelse(code2 %in% codes_nobles, p2, 0)
  if (nobles_visibles >= seuil_pur) return("Autres feuillus nobles")
  
  feuillus_visibles <- ifelse(code1 %in% codes_feuillus, p1, 0) + ifelse(code2 %in% codes_feuillus, p2, 0)
  if (feuillus_visibles >= seuil_autre) return("Autres peuplements feuillus")
  
  "Peuplement melange"
}

# ----------------------------
# 4bis) Typologie mature simplifiée
# ----------------------------
typologie_mature_simplifiee_une_ligne <- function(code1, p1, code2, p2) {
  code1 <- str_trim(ifelse(is.na(code1), "", code1))
  code2 <- str_trim(ifelse(is.na(code2), "", code2))
  p1 <- ifelse(is.na(p1), 0, p1)
  p2 <- ifelse(is.na(p2), 0, p2)
  
  nobles_visibles   <- ifelse(code1 %in% codes_nobles, p1, 0) + ifelse(code2 %in% codes_nobles, p2, 0)
  feuillus_visibles <- ifelse(code1 %in% codes_feuillus, p1, 0) + ifelse(code2 %in% codes_feuillus, p2, 0)
  
  if (p1 >= seuil_dom) {
    if (code1 == "HE")           return("Hetraie")
    if (code1 %in% codes_chene)  return("Chenaie")
    if (code1 == "FR")           return("Frenaie")
    if (code1 %in% codes_erable) return("Erabliere")
    
    if (code1 %in% codes_nobles)   return("Autres feuillus nobles")
    if (code1 %in% codes_feuillus) return("Autres peuplements feuillus")
    return("Peuplement melange")
  }
  
  if (nobles_visibles >= seuil_pur)     return("Autres feuillus nobles")
  if (feuillus_visibles >= seuil_autre) return("Autres peuplements feuillus")
  
  "Peuplement melange"
}

# ----------------------------
# 5) Fonction SE
# ----------------------------
se <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x) / sqrt(length(x))
}

# =========================================================
# 6) Connexion + lecture
# =========================================================
con <- dbConnect(RSQLite::SQLite(), chemin_bd)

dendro_plot_db <- dbReadTable(con, "dendro_plot")
arbre          <- dbReadTable(con, "arbre")

# clé utilisée pour toute la logique dendro_plot / arbre
key <- c("ues_id_ogf", "ues_id_ue")

dup_key <- dendro_plot_db %>%
  count(across(all_of(key))) %>%
  filter(n > 1)

if (nrow(dup_key) > 0) {
  stop("dendro_plot contient des doublons sur (ues_id_ogf, ues_id_ue). Corrige avant update.")
}

# vérification présence nha_tgb_240 dans dendro_plot
if (!"nha_tgb_240" %in% names(dendro_plot_db)) {
  stop("La colonne nha_tgb_240 n'existe pas dans dendro_plot.")
}

# =========================================================
# 7) ESSMAJ mature (top 40 tiges/ha) + exclusions
# =========================================================
n_tree_essMatureMaj <- 40

trees_top <- arbre %>%
  filter(statut == 1,
         !is.na(circ), circ > 0,
         !is.na(ess), ess != "",
         !is.na(fe), fe > 0) %>%
  group_by(ues_id_ogf, ues_id_ue) %>%
  arrange(desc(circ), .by_group = TRUE) %>%
  mutate(
    fexthacumsum = cumsum(fe),
    idd = row_number(),
    whup = fexthacumsum > n_tree_essMatureMaj,
    whup2 = ifelse(any(whup), which(whup)[1], NA_integer_),
    fexthacumsum2 = ifelse(whup, n_tree_essMatureMaj, fexthacumsum),
    diff = n_tree_essMatureMaj - lag(fexthacumsum2, default = 0),
    fext_ha2 = case_when(
      !is.na(whup2) & idd < whup2 ~ fe,
      !is.na(whup2) & idd == whup2 ~ diff,
      is.na(whup2) ~ fe,
      TRUE ~ 0
    ),
    gha_dom = ((circ / 100)^2) * fext_ha2 / (4 * pi)
  ) %>%
  filter(gha_dom > 0) %>%
  ungroup()

codes_exclure <- c("CR", "MZ", "DO")

ue_exclues <- trees_top %>%
  filter(ess %in% codes_exclure) %>%
  distinct(ues_id_ogf, ues_id_ue)

trees_top_ok <- trees_top %>%
  anti_join(ue_exclues, by = key)

gha_ess_mature <- trees_top_ok %>%
  group_by(ues_id_ogf, ues_id_ue, ess) %>%
  summarise(gha_ess = sum(gha_dom, na.rm = TRUE), .groups = "drop")

gha_tot_mature <- gha_ess_mature %>%
  group_by(ues_id_ogf, ues_id_ue) %>%
  summarise(gha_essMature = sum(gha_ess, na.rm = TRUE), .groups = "drop")

essmaj_mature <- gha_ess_mature %>%
  left_join(gha_tot_mature, by = key) %>%
  mutate(pct = 100 * gha_ess / gha_essMature) %>%
  group_by(ues_id_ogf, ues_id_ue) %>%
  arrange(desc(pct), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  mutate(pct_r = round(pct, 0)) %>%
  summarise(
    essmaj_mature = {
      keep <- which(pct_r > 0)
      if (length(keep) == 0) NA_character_
      else paste0(paste0(ess[keep], "-", pct_r[keep], "%"), collapse = ";")
    },
    .groups = "drop"
  )

# =========================================================
# 8) Plus gros arbre vivant par UE
# =========================================================
max_tree <- arbre %>%
  filter(statut == 1, !is.na(circ), circ > 0, !is.na(ess), ess != "") %>%
  group_by(ues_id_ogf, ues_id_ue) %>%
  slice_max(order_by = circ, n = 1, with_ties = FALSE) %>%
  transmute(ues_id_ogf, ues_id_ue, CIR_max = circ, ess_max = ess) %>%
  ungroup()

# =========================================================
# 9) Table update
# =========================================================
update_tbl <- dendro_plot_db %>%
  select(all_of(key)) %>%
  left_join(essmaj_mature, by = key) %>%
  left_join(max_tree, by = key) %>%
  left_join(ue_exclues %>% mutate(exclu = 1L), by = key) %>%
  mutate(
    valid_mature = ifelse(is.na(exclu), 1L, 0L),
    essmaj_mature = ifelse(valid_mature == 0L, NA_character_, essmaj_mature)
  ) %>%
  select(-exclu)

parsed_m <- parser_essmaj(update_tbl$essmaj_mature)

update_tbl$typologie_mature <- mapply(
  typologie_une_ligne,
  parsed_m$code1, parsed_m$p1, parsed_m$code2, parsed_m$p2
)

update_tbl$typologie_mature_simplifiee <- mapply(
  typologie_mature_simplifiee_une_ligne,
  parsed_m$code1, parsed_m$p1, parsed_m$code2, parsed_m$p2
)

wh_na <- is.na(update_tbl$essmaj_mature) | update_tbl$essmaj_mature == ""
update_tbl$typologie_mature[wh_na] <- NA
update_tbl$typologie_mature_simplifiee[wh_na] <- NA

# =========================================================
# 10) ALTER TABLE + UPDATE
# =========================================================
cols <- dbGetQuery(con, "PRAGMA table_info(dendro_plot);")$name
if (!"essmaj_mature" %in% cols)               dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN essmaj_mature TEXT;")
if (!"typologie_mature" %in% cols)            dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN typologie_mature TEXT;")
if (!"typologie_mature_simplifiee" %in% cols) dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN typologie_mature_simplifiee TEXT;")
if (!"valid_mature" %in% cols)                dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN valid_mature INTEGER;")
if (!"CIR_max" %in% cols)                     dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN CIR_max REAL;")
if (!"ess_max" %in% cols)                     dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN ess_max TEXT;")

dbWriteTable(con, "tmp_update_mature", update_tbl, overwrite = TRUE)

dbExecute(con, "
  UPDATE dendro_plot
  SET
    essmaj_mature = (SELECT essmaj_mature FROM tmp_update_mature
                     WHERE tmp_update_mature.ues_id_ogf = dendro_plot.ues_id_ogf
                       AND tmp_update_mature.ues_id_ue  = dendro_plot.ues_id_ue),
    typologie_mature = (SELECT typologie_mature FROM tmp_update_mature
                        WHERE tmp_update_mature.ues_id_ogf = dendro_plot.ues_id_ogf
                          AND tmp_update_mature.ues_id_ue  = dendro_plot.ues_id_ue),
    typologie_mature_simplifiee = (SELECT typologie_mature_simplifiee FROM tmp_update_mature
                                   WHERE tmp_update_mature.ues_id_ogf = dendro_plot.ues_id_ogf
                                     AND tmp_update_mature.ues_id_ue  = dendro_plot.ues_id_ue),
    valid_mature = (SELECT valid_mature FROM tmp_update_mature
                    WHERE tmp_update_mature.ues_id_ogf = dendro_plot.ues_id_ogf
                      AND tmp_update_mature.ues_id_ue  = dendro_plot.ues_id_ue),
    CIR_max = (SELECT CIR_max FROM tmp_update_mature
               WHERE tmp_update_mature.ues_id_ogf = dendro_plot.ues_id_ogf
                 AND tmp_update_mature.ues_id_ue  = dendro_plot.ues_id_ue),
    ess_max = (SELECT ess_max FROM tmp_update_mature
               WHERE tmp_update_mature.ues_id_ogf = dendro_plot.ues_id_ogf
                 AND tmp_update_mature.ues_id_ue  = dendro_plot.ues_id_ue);
")

dbExecute(con, "DROP TABLE tmp_update_mature;")

# =========================================================
# 11) RELECTURE TABLE MISE A JOUR POUR ANALYSES
# =========================================================
plots_ogf <- dbReadTable(con, "dendro_plot") %>%
  filter(valid_mature == 1) %>%
  mutate(
    bm_sol_total = vol_wood_debris_FAS + vol_wood_debris_LIS
  )

dbDisconnect(con)

# =========================================================
# 12) CONTROLES RAPIDES
# =========================================================
update_tbl %>% count(valid_mature, sort = TRUE)
update_tbl %>% count(typologie_mature, sort = TRUE)
update_tbl %>% count(typologie_mature_simplifiee, sort = TRUE)
update_tbl %>% count(ess_max, sort = TRUE)

names(plots_ogf)[grepl("nha_tgb", names(plots_ogf))]
summary(plots_ogf$nha_tgb_240)

head(update_tbl)
head(plots_ogf)

# =========================================================
# 13) ANALYSES BOIS MORT
# =========================================================

# --- Toutes les placettes
indicateurs_ogf <- plots_ogf %>%
  summarise(
    n_plots = n(),
    
    nha_tgb_240_moy = mean(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_med = median(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_se  = se(nha_tgb_240),
    
    bm_total_moy = mean(vol_deadw, na.rm = TRUE),
    bm_total_med = median(vol_deadw, na.rm = TRUE),
    bm_total_se  = se(vol_deadw),
    
    bm_sur_pied_moy = mean(vol_dead_standing, na.rm = TRUE),
    bm_sur_pied_med = median(vol_dead_standing, na.rm = TRUE),
    bm_sur_pied_se  = se(vol_dead_standing),
    
    bm_sol_FAS_moy = mean(vol_wood_debris_FAS, na.rm = TRUE),
    bm_sol_FAS_med = median(vol_wood_debris_FAS, na.rm = TRUE),
    bm_sol_FAS_se  = se(vol_wood_debris_FAS),
    
    bm_sol_LIS_moy = mean(vol_wood_debris_LIS, na.rm = TRUE),
    bm_sol_LIS_med = median(vol_wood_debris_LIS, na.rm = TRUE),
    bm_sol_LIS_se  = se(vol_wood_debris_LIS),
    
    bm_sol_total_moy = mean(bm_sol_total, na.rm = TRUE),
    bm_sol_total_med = median(bm_sol_total, na.rm = TRUE),
    bm_sol_total_se  = se(bm_sol_total),
    
    pct_plots_sans_bois_mort = 100 * mean(vol_deadw == 0, na.rm = TRUE)
  )

indicateurs_ogf

# --- Sans placettes à 0
indicateurs_ogf_no0 <- plots_ogf %>%
  filter(vol_deadw > 0) %>%
  summarise(
    n_plots = n(),
    
    nha_tgb_240_moy = mean(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_med = median(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_se  = se(nha_tgb_240),
    
    bm_total_moy = mean(vol_deadw, na.rm = TRUE),
    bm_total_med = median(vol_deadw, na.rm = TRUE),
    bm_total_se  = se(vol_deadw),
    
    bm_sur_pied_moy = mean(vol_dead_standing, na.rm = TRUE),
    bm_sur_pied_med = median(vol_dead_standing, na.rm = TRUE),
    bm_sur_pied_se  = se(vol_dead_standing),
    
    bm_sol_FAS_moy = mean(vol_wood_debris_FAS, na.rm = TRUE),
    bm_sol_FAS_med = median(vol_wood_debris_FAS, na.rm = TRUE),
    bm_sol_FAS_se  = se(vol_wood_debris_FAS),
    
    bm_sol_LIS_moy = mean(vol_wood_debris_LIS, na.rm = TRUE),
    bm_sol_LIS_med = median(vol_wood_debris_LIS, na.rm = TRUE),
    bm_sol_LIS_se  = se(vol_wood_debris_LIS),
    
    bm_sol_total_moy = mean(bm_sol_total, na.rm = TRUE),
    bm_sol_total_med = median(bm_sol_total, na.rm = TRUE),
    bm_sol_total_se  = se(bm_sol_total)
  )

indicateurs_ogf_no0

# --- Par typologie, sans placettes à 0
indicateurs_ogf_par_typologie_no0 <- plots_ogf %>%
  filter(vol_deadw > 0) %>%
  group_by(typologie_mature_simplifiee) %>%
  summarise(
    n_plots = n(),
    
    nha_tgb_240_moy = mean(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_med = median(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_se  = se(nha_tgb_240),
    
    bm_total_moy = mean(vol_deadw, na.rm = TRUE),
    bm_total_med = median(vol_deadw, na.rm = TRUE),
    bm_total_se  = se(vol_deadw),
    
    bm_sur_pied_moy = mean(vol_dead_standing, na.rm = TRUE),
    bm_sur_pied_med = median(vol_dead_standing, na.rm = TRUE),
    bm_sur_pied_se  = se(vol_dead_standing),
    
    bm_sol_FAS_moy = mean(vol_wood_debris_FAS, na.rm = TRUE),
    bm_sol_FAS_med = median(vol_wood_debris_FAS, na.rm = TRUE),
    bm_sol_FAS_se  = se(vol_wood_debris_FAS),
    
    bm_sol_LIS_moy = mean(vol_wood_debris_LIS, na.rm = TRUE),
    bm_sol_LIS_med = median(vol_wood_debris_LIS, na.rm = TRUE),
    bm_sol_LIS_se  = se(vol_wood_debris_LIS),
    
    bm_sol_total_moy = mean(bm_sol_total, na.rm = TRUE),
    bm_sol_total_med = median(bm_sol_total, na.rm = TRUE),
    bm_sol_total_se  = se(bm_sol_total),
    
    .groups = "drop"
  ) %>%
  arrange(desc(bm_total_moy))

indicateurs_ogf_par_typologie_no0
# =========================================================
# 14) TABLEAUX "SLIDE READY"
# =========================================================
tableau_ogf_slide <- indicateurs_ogf %>%
  transmute(
    zone = "OGF terrain",
    n = n_plots,
    nha_tgb_240_moy = round(nha_tgb_240_moy, 1),
    nha_tgb_240_med = round(nha_tgb_240_med, 1),
    nha_tgb_240_se  = round(nha_tgb_240_se, 1),
    bm_total_moy = round(bm_total_moy, 1),
    mediane = round(bm_total_med, 1),
    se = round(bm_total_se, 1),
    bm_sur_pied = round(bm_sur_pied_moy, 1),
    bm_au_sol = round(bm_sol_total_moy, 1),
    pct_sans_bm = round(pct_plots_sans_bois_mort, 1)
  )

tableau_ogf_slide

tableau_ogf_slide_no0 <- indicateurs_ogf_no0 %>%
  transmute(
    zone = "OGF terrain (BM > 0)",
    n = n_plots,
    nha_tgb_240_moy = round(nha_tgb_240_moy, 1),
    nha_tgb_240_med = round(nha_tgb_240_med, 1),
    nha_tgb_240_se  = round(nha_tgb_240_se, 1),
    bm_total_moy = round(bm_total_moy, 1),
    mediane = round(bm_total_med, 1),
    se = round(bm_total_se, 1),
    bm_sur_pied = round(bm_sur_pied_moy, 1),
    bm_au_sol = round(bm_sol_total_moy, 1)
  )

tableau_ogf_slide_no0

tableau_ogf_typo_slide <- indicateurs_ogf_par_typologie_no0 %>%
  transmute(
    typologie = typologie_mature_simplifiee,
    n = n_plots,
    nha_tgb_240_moy = round(nha_tgb_240_moy, 1),
    nha_tgb_240_med = round(nha_tgb_240_med, 1),
    nha_tgb_240_se  = round(nha_tgb_240_se, 1),
    bm_total_moy = round(bm_total_moy, 1),
    mediane = round(bm_total_med, 1),
    se = round(bm_total_se, 1),
    bm_sur_pied = round(bm_sur_pied_moy, 1),
    bm_au_sol = round(bm_sol_total_moy, 1)
  )

tableau_ogf_typo_slide

# =========================================================
# 15) CORRELATION FAS vs LIS
# =========================================================
test_cor_fas_lis_pearson <- cor.test(
  plots_ogf$vol_wood_debris_FAS,
  plots_ogf$vol_wood_debris_LIS,
  use = "complete.obs",
  method = "pearson"
)

test_cor_fas_lis_spearman <- cor.test(
  plots_ogf$vol_wood_debris_FAS,
  plots_ogf$vol_wood_debris_LIS,
  use = "complete.obs",
  method = "spearman"
)

test_cor_fas_lis_pearson
test_cor_fas_lis_spearman

# =========================================================
# 16) DESCRIPTION DES DONNEES TERRAIN OGF
# =========================================================
plots_ogf <- plots_ogf %>%
  mutate(
    bm_sur_pied = vol_dead_standing,
    bm_sol_fas = vol_wood_debris_FAS,
    bm_sol_lis = vol_wood_debris_LIS,
    bm_sol_total = vol_wood_debris_FAS + vol_wood_debris_LIS,
    ratio_bm_vivant = ifelse(!is.na(vol_alive) & vol_alive > 0, vol_deadw / vol_alive, NA_real_),
    pct_bm_vivant = ifelse(!is.na(vol_alive) & vol_alive > 0, 100 * vol_deadw / vol_alive, NA_real_)
  )

# ---------------------------------------------------------
# 16.1 Tableau général complet
# ---------------------------------------------------------
description_ogf_globale <- plots_ogf %>%
  summarise(
    n_placettes = n(),
    
    cdom_moy = mean(cdom, na.rm = TRUE),
    cdom_med = median(cdom, na.rm = TRUE),
    cdom_se  = se(cdom),
    
    CIR_max_moy = mean(CIR_max, na.rm = TRUE),
    CIR_max_med = median(CIR_max, na.rm = TRUE),
    CIR_max_se  = se(CIR_max),
    
    nha_tgb_240_moy = mean(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_med = median(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_se  = se(nha_tgb_240),
    
    vol_vivant_moy = mean(vol_alive, na.rm = TRUE),
    vol_vivant_med = median(vol_alive, na.rm = TRUE),
    vol_vivant_se  = se(vol_alive),
    
    vol_mort_moy = mean(vol_deadw, na.rm = TRUE),
    vol_mort_med = median(vol_deadw, na.rm = TRUE),
    vol_mort_se  = se(vol_deadw),
    
    bm_sur_pied_moy = mean(bm_sur_pied, na.rm = TRUE),
    bm_sur_pied_med = median(bm_sur_pied, na.rm = TRUE),
    bm_sur_pied_se  = se(bm_sur_pied),
    
    bm_sol_fas_moy = mean(bm_sol_fas, na.rm = TRUE),
    bm_sol_fas_med = median(bm_sol_fas, na.rm = TRUE),
    bm_sol_fas_se  = se(bm_sol_fas),
    
    bm_sol_lis_moy = mean(bm_sol_lis, na.rm = TRUE),
    bm_sol_lis_med = median(bm_sol_lis, na.rm = TRUE),
    bm_sol_lis_se  = se(bm_sol_lis),
    
    bm_sol_total_moy = mean(bm_sol_total, na.rm = TRUE),
    bm_sol_total_med = median(bm_sol_total, na.rm = TRUE),
    bm_sol_total_se  = se(bm_sol_total),
    
    pct_bm_vivant_moy = mean(pct_bm_vivant, na.rm = TRUE),
    pct_bm_vivant_med = median(pct_bm_vivant, na.rm = TRUE),
    
    nb_arbres_moy = mean(number_of_trees, na.rm = TRUE),
    nb_arbres_med = median(number_of_trees, na.rm = TRUE),
    
    nb_gros_arbres_moy = mean(number_of_trees_thres120, na.rm = TRUE),
    nb_gros_arbres_med = median(number_of_trees_thres120, na.rm = TRUE)
  )

description_ogf_globale

# ---------------------------------------------------------
# 16.2 Tableau par typologie
# ---------------------------------------------------------
description_ogf_par_typologie <- plots_ogf %>%
  group_by(typologie_mature_simplifiee) %>%
  summarise(
    n_placettes = n(),
    
    cdom_moy = mean(cdom, na.rm = TRUE),
    cdom_med = median(cdom, na.rm = TRUE),
    cdom_se  = se(cdom),
    
    CIR_max_moy = mean(CIR_max, na.rm = TRUE),
    CIR_max_med = median(CIR_max, na.rm = TRUE),
    CIR_max_se  = se(CIR_max),
    
    nha_tgb_240_moy = mean(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_med = median(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_se  = se(nha_tgb_240),
    
    vol_vivant_moy = mean(vol_alive, na.rm = TRUE),
    vol_vivant_med = median(vol_alive, na.rm = TRUE),
    vol_vivant_se  = se(vol_alive),
    
    vol_mort_moy = mean(vol_deadw, na.rm = TRUE),
    vol_mort_med = median(vol_deadw, na.rm = TRUE),
    vol_mort_se  = se(vol_deadw),
    
    bm_sur_pied_moy = mean(bm_sur_pied, na.rm = TRUE),
    bm_sur_pied_med = median(bm_sur_pied, na.rm = TRUE),
    bm_sur_pied_se  = se(bm_sur_pied),
    
    bm_sol_fas_moy = mean(bm_sol_fas, na.rm = TRUE),
    bm_sol_fas_med = median(bm_sol_fas, na.rm = TRUE),
    bm_sol_fas_se  = se(bm_sol_fas),
    
    bm_sol_lis_moy = mean(bm_sol_lis, na.rm = TRUE),
    bm_sol_lis_med = median(bm_sol_lis, na.rm = TRUE),
    bm_sol_lis_se  = se(bm_sol_lis),
    
    bm_sol_total_moy = mean(bm_sol_total, na.rm = TRUE),
    bm_sol_total_med = median(bm_sol_total, na.rm = TRUE),
    bm_sol_total_se  = se(bm_sol_total),
    
    pct_bm_vivant_moy = mean(pct_bm_vivant, na.rm = TRUE),
    pct_bm_vivant_med = median(pct_bm_vivant, na.rm = TRUE),
    
    nb_gros_arbres_moy = mean(number_of_trees_thres120, na.rm = TRUE),
    nb_gros_arbres_med = median(number_of_trees_thres120, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(desc(vol_mort_moy))

description_ogf_par_typologie

# ---------------------------------------------------------
# 16.3 Version "slide-ready" : structure générale
# ---------------------------------------------------------
tableau_ogf_structure_slide <- description_ogf_globale %>%
  transmute(
    Jeu = "Placettes terrain OGF",
    n = n_placettes,
    
    `CDOM moyen` = round(cdom_moy, 1),
    `CDOM médian` = round(cdom_med, 1),
    
    `CIR max moyen` = round(CIR_max_moy, 1),
    `CIR max médian` = round(CIR_max_med, 1),
    
    `NHA TGB moyen` = round(nha_tgb_240_moy, 1),
    `NHA TGB médian` = round(nha_tgb_240_med, 1),
    `SE NHA TGB` = round(nha_tgb_240_se, 1),
    
    `Vol. vivant moyen` = round(vol_vivant_moy, 1),
    `SE vol. vivant` = round(vol_vivant_se, 1),
    
    `Vol. mort total moyen` = round(vol_mort_moy, 1),
    `Vol. mort total médian` = round(vol_mort_med, 1),
    `SE vol. mort total` = round(vol_mort_se, 1),
    
    `% BM / vivant` = round(pct_bm_vivant_moy, 1),
    
    `Nb gros arbres moyen` = round(nb_gros_arbres_moy, 1)
  )

tableau_ogf_structure_slide

# ---------------------------------------------------------
# 16.4 Version "slide-ready" : décomposition du bois mort
# ---------------------------------------------------------
tableau_ogf_bm_slide <- description_ogf_globale %>%
  transmute(
    Jeu = "Placettes terrain OGF",
    n = n_placettes,
    
    `NHA TGB moyen` = round(nha_tgb_240_moy, 1),
    `NHA TGB médian` = round(nha_tgb_240_med, 1),
    
    `BM total moyen` = round(vol_mort_moy, 1),
    `BM total médian` = round(vol_mort_med, 1),
    
    `BM sur pied moyen` = round(bm_sur_pied_moy, 1),
    `SE BM sur pied` = round(bm_sur_pied_se, 1),
    
    `BM au sol FAS moyen` = round(bm_sol_fas_moy, 1),
    `SE BM au sol FAS` = round(bm_sol_fas_se, 1),
    
    `BM au sol LIS moyen` = round(bm_sol_lis_moy, 1),
    `SE BM au sol LIS` = round(bm_sol_lis_se, 1),
    
    `BM au sol total moyen` = round(bm_sol_total_moy, 1),
    `BM au sol total médian` = round(bm_sol_total_med, 1),
    `SE BM au sol total` = round(bm_sol_total_se, 1)
  )

tableau_ogf_bm_slide

# ---------------------------------------------------------
# 16.5 Version "slide-ready" : par typologie
# ---------------------------------------------------------
tableau_ogf_typologie_slide <- description_ogf_par_typologie %>%
  transmute(
    Typologie = typologie_mature_simplifiee,
    n = n_placettes,
    
    `CDOM moy` = round(cdom_moy, 1),
    `CIR max moy` = round(CIR_max_moy, 1),
    
    `NHA TGB moy` = round(nha_tgb_240_moy, 1),
    `NHA TGB médian` = round(nha_tgb_240_med, 1),
    
    `Vol. vivant moy` = round(vol_vivant_moy, 1),
    `Vol. mort total moy` = round(vol_mort_moy, 1),
    `Vol. mort total médian` = round(vol_mort_med, 1),
    `SE vol. mort` = round(vol_mort_se, 1),
    
    `BM sur pied moy` = round(bm_sur_pied_moy, 1),
    `BM au sol total moy` = round(bm_sol_total_moy, 1),
    
    `% BM / vivant` = round(pct_bm_vivant_moy, 1),
    `Nb gros arbres moy` = round(nb_gros_arbres_moy, 1)
  )

tableau_ogf_typologie_slide

# ---------------------------------------------------------
# 16.6 Export Excel séparé "description terrain"
# ---------------------------------------------------------
f_out_ogf_desc <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/resultats_description_terrain_ogf.xlsx"

wb_ogf <- createWorkbook()

addWorksheet(wb_ogf, "ogf_global")
addWorksheet(wb_ogf, "ogf_par_typologie")
addWorksheet(wb_ogf, "ogf_slide_structure")
addWorksheet(wb_ogf, "ogf_slide_deadwood")
addWorksheet(wb_ogf, "ogf_slide_typologie")

writeData(wb_ogf, "ogf_global", description_ogf_globale)
writeData(wb_ogf, "ogf_par_typologie", description_ogf_par_typologie)
writeData(wb_ogf, "ogf_slide_structure", tableau_ogf_structure_slide)
writeData(wb_ogf, "ogf_slide_deadwood", tableau_ogf_bm_slide)
writeData(wb_ogf, "ogf_slide_typologie", tableau_ogf_typologie_slide)

saveWorkbook(wb_ogf, f_out_ogf_desc, overwrite = TRUE)

# ---------------------------------------------------------
# 16.7 Tableau par typologie - toutes les placettes
# ---------------------------------------------------------
description_ogf_par_typologie_all <- plots_ogf %>%
  group_by(typologie_mature_simplifiee) %>%
  summarise(
    n_placettes = n(),
    
    nha_tgb_240_moy = mean(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_med = median(nha_tgb_240, na.rm = TRUE),
    nha_tgb_240_se  = se(nha_tgb_240),
    
    vol_mort_moy = mean(vol_deadw, na.rm = TRUE),
    vol_mort_med = median(vol_deadw, na.rm = TRUE),
    vol_mort_se  = se(vol_deadw),
    
    bm_sur_pied_moy = mean(bm_sur_pied, na.rm = TRUE),
    bm_sur_pied_med = median(bm_sur_pied, na.rm = TRUE),
    bm_sur_pied_se  = se(bm_sur_pied),
    
    bm_sol_fas_moy = mean(bm_sol_fas, na.rm = TRUE),
    bm_sol_fas_med = median(bm_sol_fas, na.rm = TRUE),
    bm_sol_fas_se  = se(bm_sol_fas),
    
    bm_sol_lis_moy = mean(bm_sol_lis, na.rm = TRUE),
    bm_sol_lis_med = median(bm_sol_lis, na.rm = TRUE),
    bm_sol_lis_se  = se(bm_sol_lis),
    
    bm_sol_total_moy = mean(bm_sol_total, na.rm = TRUE),
    bm_sol_total_med = median(bm_sol_total, na.rm = TRUE),
    bm_sol_total_se  = se(bm_sol_total),
    
    .groups = "drop"
  ) %>%
  arrange(desc(vol_mort_moy))

description_ogf_par_typologie_all

# ---------------------------------------------------------
# Version lisible pour slides
# ---------------------------------------------------------
tableau_ogf_typologie_all_slide <- description_ogf_par_typologie_all %>%
  transmute(
    Typologie = typologie_mature_simplifiee,
    n = n_placettes,
    
    `NHA TGB moy` = round(nha_tgb_240_moy, 1),
    `NHA TGB médian` = round(nha_tgb_240_med, 1),
    `SE NHA TGB` = round(nha_tgb_240_se, 1),
    
    `BM total moyen` = round(vol_mort_moy, 1),
    `BM total médian` = round(vol_mort_med, 1),
    `SE BM total` = round(vol_mort_se, 1),
    
    `BM sur pied moy` = round(bm_sur_pied_moy, 1),
    
    `BM sol FAS moy` = round(bm_sol_fas_moy, 1),
    `BM sol LIS moy` = round(bm_sol_lis_moy, 1),
    
    `BM sol total moy` = round(bm_sol_total_moy, 1),
    `BM sol total médian` = round(bm_sol_total_med, 1),
    `SE BM sol total` = round(bm_sol_total_se, 1)
  )

tableau_ogf_typologie_all_slide