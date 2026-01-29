#code_typologie


library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)

chemin_bd <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"  

# ----------------------------
# 2) Seuils 
# ----------------------------
seuil_pur   <- 66.7  # feuillus "purs"
seuil_autre <- 50    # "autres peuplements feuillus" 

# ----------------------------
# 3) Codes (regroupements)
# ----------------------------
codes_chene  <- c("CH","CP","CS","CHs")   # tous = chêne
codes_erable <- c("ER","EP","ES")         # tous = érable

codes_nobles <- c("HE", codes_chene, "FR", codes_erable, "MR", "CR")

# feuillus (pour "autres peuplements feuillus")
codes_feuillus <- c("HE", codes_chene, "FR", codes_erable, "MR", "CR",
                    "CA","AUs","BOU","BOs","TIs","PPN","PG")

# ----------------------------
# 4) Parser essmaj (2 essences)
# ----------------------------
parser_essmaj <- function(x) {
  x <- str_replace_all(x, "\\s+", "")
  code1 <- str_match(x, "^([A-Za-z]+)-")[,2]
  p1    <- as.numeric(str_match(x, "-([0-9]+)\\%")[,2])
  code2 <- str_match(x, "\\%;([A-Za-z]+)-")[,2]
  p2    <- as.numeric(str_match(x, "\\%;[A-Za-z]+-([0-9]+)\\%")[,2])
  ifelse(is.na(code2), NA, code2) -> code2
  p2[is.na(p2)] <- 0
  tibble(code1=code1, p1=p1, code2=code2, p2=p2)
}

# ----------------------------
# 5) Classif typologie (règles)
#    + règle spéciale : si code1 = érable => Érablière (peu importe %)
# ----------------------------
typologie_une_ligne <- function(code1, p1, code2, p2) {
  code1 <- str_trim(code1)
  code2 <- ifelse(is.na(code2), NA_character_, str_trim(code2))
  p1 <- ifelse(is.na(p1), 0, p1)
  p2 <- ifelse(is.na(p2), 0, p2)
  
  # règle spéciale érablière : essence dominante = érable
  if (code1 %in% codes_erable) return("Erabliere")
  
  total_chene <- ifelse(code1 %in% codes_chene, p1, 0) + ifelse(!is.na(code2) & code2 %in% codes_chene, p2, 0)
  total_erable <- ifelse(code1 %in% codes_erable, p1, 0) + ifelse(!is.na(code2) & code2 %in% codes_erable, p2, 0)
  total_he <- ifelse(code1=="HE", p1, 0) + ifelse(!is.na(code2) & code2=="HE", p2, 0)
  total_fr <- ifelse(code1=="FR", p1, 0) + ifelse(!is.na(code2) & code2=="FR", p2, 0)
  total_mr <- ifelse(code1=="MR", p1, 0) + ifelse(!is.na(code2) & code2=="MR", p2, 0)
  total_cr <- ifelse(code1=="CR", p1, 0) + ifelse(!is.na(code2) & code2=="CR", p2, 0)
  
  # 1) types purs (>= 66.7)
  if (total_chene >= seuil_pur) return("Chenaie")
  if (total_he    >= seuil_pur) return("Hetraie")
  if (total_fr    >= seuil_pur) return("Frenaie")
  if (total_erable>= seuil_pur) return("Erabliere")
  if (total_mr    >= seuil_pur) return("Merisier")
  if (total_cr    >= seuil_pur) return("Chene rouge")
  
  # 2) mélanges spécifiques
  if ((total_he + total_chene) >= seuil_pur && total_he < seuil_pur && total_chene < seuil_pur) return("Hetre - Chene")
  if ((total_fr + total_chene) >= seuil_pur && total_fr < seuil_pur && total_chene < seuil_pur) return("Chene - Frene")
  
  # 3) autres feuillus nobles (proxy top2)
  nobles_visibles <- ifelse(code1 %in% codes_nobles, p1, 0) + ifelse(!is.na(code2) & code2 %in% codes_nobles, p2, 0)
  if (nobles_visibles >= seuil_pur) return("Autres feuillus nobles")
  
  # 4) autres peuplements feuillus (proxy top2)
  feuillus_visibles <- ifelse(code1 %in% codes_feuillus, p1, 0) + ifelse(!is.na(code2) & code2 %in% codes_feuillus, p2, 0)
  if (feuillus_visibles >= seuil_autre) return("Autres peuplements feuillus")
  
  # 5) sinon
  "Peuplement melange"
}

# ----------------------------
# 6) Lire table, calculer typologie, écrire dans SQLite
# ----------------------------
con <- dbConnect(RSQLite::SQLite(), chemin_bd)

dendro_plot <- dbReadTable(con, "dendro_plot")

parsed <- parser_essmaj(dendro_plot$essmaj)

dendro_plot$typologie <- mapply(
  typologie_une_ligne,
  parsed$code1, parsed$p1, parsed$code2, parsed$p2
)

# Ajouter colonne si besoin
colonnes <- dbGetQuery(con, "PRAGMA table_info(dendro_plot);")$name
if (!"typologie" %in% colonnes) {
  dbExecute(con, "ALTER TABLE dendro_plot ADD COLUMN typologie TEXT;")
}

# MAJ via rowid (simple)
rowids <- dbGetQuery(con, "SELECT rowid FROM dendro_plot;")
stopifnot(nrow(rowids) == nrow(dendro_plot))

tmp <- data.frame(rowid = rowids$rowid, typologie = dendro_plot$typologie)
dbWriteTable(con, "tmp_typo", tmp, overwrite = TRUE)

dbExecute(con, "
  UPDATE dendro_plot
  SET typologie = (SELECT typologie FROM tmp_typo WHERE tmp_typo.rowid = dendro_plot.rowid);
")

dbExecute(con, "DROP TABLE tmp_typo;")
dbDisconnect(con)


#nombre de parcelle par typo
dendro_plot %>%
  count(typologie, name = "n_lignes") %>%
  arrange(desc(n_lignes))


