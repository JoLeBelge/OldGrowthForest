# =========================================================
# OGF_all.db : essmaj "mature" (gros arbres) + typologie_mature
#            + plus gros arbre (CIR_max, ess_max)
#            + EXCLUSION UE si présence de CR/MZ/DO -> NA + flag
#            + écriture dans dendro_plot (SQLite)
# =========================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)

chemin_bd <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"

# ----------------------------
# 1) Seuils
# ----------------------------
seuil_pur   <- 66.7
seuil_autre <- 50

# ----------------------------
# 2) Codes (regroupements)
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
# 4) Typologie
# ----------------------------
typologie_une_ligne <- function(code1, p1, code2, p2) {
  code1 <- str_trim(ifelse(is.na(code1), "", code1))
  code2 <- str_trim(ifelse(is.na(code2), "", code2))
  p1 <- ifelse(is.na(p1), 0, p1)
  p2 <- ifelse(is.na(p2), 0, p2)
  
  # règle spéciale : dominante = érable (sans seuil)
  if (code1 %in% codes_erable) return("Erabliere")
  
  total_chene  <- ifelse(code1 %in% codes_chene, p1, 0) + ifelse(code2 %in% codes_chene, p2, 0)
  total_erable <- ifelse(code1 %in% codes_erable, p1, 0) + ifelse(code2 %in% codes_erable, p2, 0)
  total_he     <- ifelse(code1=="HE", p1, 0) + ifelse(code2=="HE", p2, 0)
  total_fr     <- ifelse(code1=="FR", p1, 0) + ifelse(code2=="FR", p2, 0)
  total_mr     <- ifelse(code1=="MR", p1, 0) + ifelse(code2=="MR", p2, 0)
  total_cr     <- ifelse(code1=="CR", p1, 0) + ifelse(code2=="CR", p2, 0)
  
  # types purs
  if (total_chene  >= seuil_pur) return("Chenaie")
  if (total_he     >= seuil_pur) return("Hetraie")
  if (total_fr     >= seuil_pur) return("Frenaie")
  if (total_erable >= seuil_pur) return("Erabliere")
  if (total_mr     >= seuil_pur) return("Merisier")
  if (total_cr     >= seuil_pur) return("Chene rouge")
  
  # mélanges
  if ((total_he + total_chene) >= seuil_pur && total_he < seuil_pur && total_chene < seuil_pur) return("Hetre - Chene")
  if ((total_fr + total_chene) >= seuil_pur && total_fr < seuil_pur && total_chene < seuil_pur) return("Chene - Frene")
  
  # autres nobles (proxy top2)
  nobles_visibles <- ifelse(code1 %in% codes_nobles, p1, 0) + ifelse(code2 %in% codes_nobles, p2, 0)
  if (nobles_visibles >= seuil_pur) return("Autres feuillus nobles")
  
  # autres feuillus (proxy top2)
  feuillus_visibles <- ifelse(code1 %in% codes_feuillus, p1, 0) + ifelse(code2 %in% codes_feuillus, p2, 0)
  if (feuillus_visibles >= seuil_autre) return("Autres peuplements feuillus")
  
  "Peuplement melange"
}

# =========================================================
# 5) Connexion DB + lecture tables
# =========================================================
con <- dbConnect(RSQLite::SQLite(), chemin_bd)

dendro_plot <- dbReadTable(con, "dendro_plot")
arbre <- dbReadTable(con, "arbre")

key <- c("ues_id_ogf", "ues_id_ue")

# =========================================================
# 6) ESSMAJ "MATURE" : top 40 tiges/ha les plus grosses (vivantes)
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

# =========================================================
# 6bis) EXCLUSION UE si présence de CR/MZ/DO dans trees_top (on mettra NA + flag)
# =========================================================
codes_exclure <- c("CR","MZ","DO")

ue_exclues <- trees_top %>%
  filter(ess %in% codes_exclure) %>%
  distinct(ues_id_ogf, ues_id_ue)

trees_top_ok <- trees_top %>%
  anti_join(ue_exclues, by = key)

# =========================================================
# 6ter) Calcul essmaj_mature sur UE non exclues
# =========================================================
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
# 7) Construire dendro_plot2 proprement + flag valid_mature + NA exclues
# =========================================================
dendro_plot2 <- dendro_plot %>%
  select(-any_of(c("essmaj_mature","typologie_mature","valid_mature","CIR_max","ess_max"))) %>%
  left_join(essmaj_mature, by = key) %>%
  mutate(valid_mature = 1L) %>%
  left_join(ue_exclues %>% mutate(exclu = 1L), by = key) %>%
  mutate(
    valid_mature = ifelse(!is.na(exclu), 0L, valid_mature),
    essmaj_mature = ifelse(valid_mature == 0L, NA_character_, essmaj_mature)
  ) %>%
  select(-exclu)

# =========================================================
# 8) Typologie sur essmaj_mature
# =========================================================
parsed_m <- parser_essmaj(dendro_plot2$essmaj_mature)

dendro_plot2$typologie_mature <- mapply(
  typologie_une_ligne,
  parsed_m$code1, parsed_m$p1, parsed_m$code2, parsed_m$p2
)

dendro_plot2$typologie_mature[is.na(dendro_plot2$essmaj_mature) | dendro_plot2$essmaj_mature==""] <- NA

# =========================================================
# 9) Plus gros arbre (vivant) par UE : CIR_max + ess_max
# =========================================================
max_tree <- arbre %>%
  filter(statut == 1, !is.na(circ), circ > 0, !is.na(ess), ess != "") %>%
  group_by(ues_id_ogf, ues_id_ue) %>%
  slice_max(order_by = circ, n = 1, with_ties = FALSE) %>%
  transmute(
    ues_id_ogf, ues_id_ue,
    CIR_max = circ,
    ess_max = ess
  ) %>%
  ungroup()

dendro_plot2 <- dendro_plot2 %>%
  left_join(max_tree, by = key)

# =========================================================
# 10) Écriture dans SQLite (ALTER + UPDATE via rowid)
# =========================================================
cols <- dbGetQuery(con, "PRAGMA table_info(dendro_plot);")$name
if (!"essmaj_mature" %in% cols)    dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN essmaj_mature TEXT;")
if (!"typologie_mature" %in% cols) dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN typologie_mature TEXT;")
if (!"valid_mature" %in% cols)     dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN valid_mature INTEGER;")
if (!"CIR_max" %in% cols)          dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN CIR_max REAL;")
if (!"ess_max" %in% cols)          dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN ess_max TEXT;")

rowids <- dbGetQuery(con, "SELECT rowid FROM dendro_plot;")
stopifnot(nrow(rowids) == nrow(dendro_plot2))

tmp <- data.frame(
  rowid = rowids$rowid,
  essmaj_mature = dendro_plot2$essmaj_mature,
  typologie_mature = dendro_plot2$typologie_mature,
  valid_mature = dendro_plot2$valid_mature,
  CIR_max = dendro_plot2$CIR_max,
  ess_max = dendro_plot2$ess_max
)

dbWriteTable(con, "tmp_typo_mature", tmp, overwrite = TRUE)

dbExecute(con, "
  UPDATE dendro_plot
  SET
    essmaj_mature = (SELECT essmaj_mature FROM tmp_typo_mature WHERE tmp_typo_mature.rowid = dendro_plot.rowid),
    typologie_mature = (SELECT typologie_mature FROM tmp_typo_mature WHERE tmp_typo_mature.rowid = dendro_plot.rowid),
    valid_mature = (SELECT valid_mature FROM tmp_typo_mature WHERE tmp_typo_mature.rowid = dendro_plot.rowid),
    CIR_max = (SELECT CIR_max FROM tmp_typo_mature WHERE tmp_typo_mature.rowid = dendro_plot.rowid),
    ess_max = (SELECT ess_max FROM tmp_typo_mature WHERE tmp_typo_mature.rowid = dendro_plot.rowid);
")

dbExecute(con, "DROP TABLE tmp_typo_mature;")
dbDisconnect(con)

# =========================================================
# 11) Contrôles rapides
# =========================================================
dendro_plot2 %>% count(valid_mature, sort = TRUE)
dendro_plot2 %>% count(typologie_mature, sort = TRUE)
dendro_plot2 %>% count(ess_max, sort = TRUE)

head(dendro_plot2[, c("ues_id_ogf","ues_id_ue","valid_mature","essmaj_mature","typologie_mature","CIR_max","ess_max")])
