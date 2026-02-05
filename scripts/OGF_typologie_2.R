# =========================================================
# OGF_all.db : AJOUT typologie_mature_simplifiee (SAFE UPDATE par clés)
# - ne modifie PAS le nombre de lignes de dendro_plot
# - update par (ues_id_ogf, ues_id_ue), pas par rowid
# =========================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)

chemin_bd <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"

# ----------------------------
# 1) Seuils
# ----------------------------
seuil_pur   <- 66.7  # IPRFW
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
  
  tibble(code1=code1, p1=p1, code2=code2, p2=p2)
}

# ----------------------------
# 4) Typologie mature (ancienne) - inchangée
# ----------------------------
typologie_une_ligne <- function(code1, p1, code2, p2) {
  code1 <- str_trim(ifelse(is.na(code1), "", code1))
  code2 <- str_trim(ifelse(is.na(code2), "", code2))
  p1 <- ifelse(is.na(p1), 0, p1)
  p2 <- ifelse(is.na(p2), 0, p2)
  
  if (code1 %in% codes_erable) return("Erabliere")
  
  total_chene  <- ifelse(code1 %in% codes_chene, p1, 0) + ifelse(code2 %in% codes_chene, p2, 0)
  total_erable <- ifelse(code1 %in% codes_erable, p1, 0) + ifelse(code2 %in% codes_erable, p2, 0)
  total_he     <- ifelse(code1=="HE", p1, 0) + ifelse(code2=="HE", p2, 0)
  total_fr     <- ifelse(code1=="FR", p1, 0) + ifelse(code2=="FR", p2, 0)
  total_mr     <- ifelse(code1=="MR", p1, 0) + ifelse(code2=="MR", p2, 0)
  total_cr     <- ifelse(code1=="CR", p1, 0) + ifelse(code2=="CR", p2, 0)
  
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
# 4bis) Typologie mature simplifiée (nouvelle)
# ----------------------------
typologie_mature_simplifiee_une_ligne <- function(code1, p1, code2, p2) {
  code1 <- str_trim(ifelse(is.na(code1), "", code1))
  code2 <- str_trim(ifelse(is.na(code2), "", code2))
  p1 <- ifelse(is.na(p1), 0, p1)
  p2 <- ifelse(is.na(p2), 0, p2)
  
  nobles_visibles <- ifelse(code1 %in% codes_nobles, p1, 0) + ifelse(code2 %in% codes_nobles, p2, 0)
  feuillus_visibles <- ifelse(code1 %in% codes_feuillus, p1, 0) + ifelse(code2 %in% codes_feuillus, p2, 0)
  
  if (p1 >= seuil_dom) {
    if (code1 == "HE")            return("Hetraie")
    if (code1 %in% codes_chene)   return("Chenaie")
    if (code1 == "FR")            return("Frenaie")
    if (code1 %in% codes_erable)  return("Erabliere")
    
    if (code1 %in% codes_nobles)   return("Autres feuillus nobles")
    if (code1 %in% codes_feuillus) return("Autres peuplements feuillus")
    return("Peuplement melange")
  }
  
  if (nobles_visibles >= seuil_pur)     return("Autres feuillus nobles")
  if (feuillus_visibles >= seuil_autre) return("Autres peuplements feuillus")
  
  "Peuplement melange"
}

# =========================================================
# 5) Connexion + lecture
# =========================================================
con <- dbConnect(RSQLite::SQLite(), chemin_bd)

dendro_plot_db <- dbReadTable(con, "dendro_plot")
arbre <- dbReadTable(con, "arbre")

key <- c("ues_id_ogf", "ues_id_ue")

# --- sécurité : clé unique dans dendro_plot ---
dup_key <- dendro_plot_db %>% count(across(all_of(key))) %>% filter(n > 1)
if (nrow(dup_key) > 0) {
  stop("dendro_plot contient des doublons sur (ues_id_ogf, ues_id_ue). Corrige avant update.")
}

# =========================================================
# 6) ESSMAJ mature (top 40 tiges/ha) + exclusions
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
    gha_dom = ((circ/100)^2) * fext_ha2 / (4*pi)
  ) %>%
  filter(gha_dom > 0) %>%
  ungroup()

codes_exclure <- c("CR","MZ","DO")

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
# 7) Plus gros arbre (vivant) par UE : CIR_max + ess_max
# =========================================================
max_tree <- arbre %>%
  filter(statut == 1, !is.na(circ), circ > 0, !is.na(ess), ess != "") %>%
  group_by(ues_id_ogf, ues_id_ue) %>%
  slice_max(order_by = circ, n = 1, with_ties = FALSE) %>%
  transmute(ues_id_ogf, ues_id_ue, CIR_max = circ, ess_max = ess) %>%
  ungroup()

# =========================================================
# 8) Construire une table UPDATE (1 ligne par UE, clés uniques)
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

# typologies sur essmaj_mature
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
# 9) ALTER TABLE si besoin + UPDATE SQL via table temporaire (par clés)
# =========================================================
cols <- dbGetQuery(con, "PRAGMA table_info(dendro_plot);")$name
if (!"essmaj_mature" %in% cols)                 dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN essmaj_mature TEXT;")
if (!"typologie_mature" %in% cols)              dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN typologie_mature TEXT;")
if (!"typologie_mature_simplifiee" %in% cols)   dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN typologie_mature_simplifiee TEXT;")
if (!"valid_mature" %in% cols)                  dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN valid_mature INTEGER;")
if (!"CIR_max" %in% cols)                       dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN CIR_max REAL;")
if (!"ess_max" %in% cols)                       dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN ess_max TEXT;")

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
dbDisconnect(con)

# =========================================================
# 10) Contrôles rapides (sans relire la DB, on check update_tbl)
# =========================================================
update_tbl %>% count(valid_mature, sort = TRUE)
update_tbl %>% count(typologie_mature, sort = TRUE)
update_tbl %>% count(typologie_mature_simplifiee, sort = TRUE)
update_tbl %>% count(ess_max, sort = TRUE)

head(update_tbl)
