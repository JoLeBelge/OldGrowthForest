library(sf)
library(dplyr)
library(tidyr)
# Chemins (utilise / ou bien double \\ sous Windows)
gpkg_plots <- "C:/Old_Growth_Forest/DATA/données_iprfw/ifw_plots.gpkg"
gpkg_trees <- "C:/Old_Growth_Forest/DATA/données_iprfw/ifw_plots_trees_coppices.gpkg"

# voir quelles couches existent dans chaque gpkg
st_layers(gpkg_plots)
st_layers(gpkg_trees)

plots <- st_read(gpkg_plots, layer = "ifw_plots")
trees_coppices <- st_read(gpkg_trees, layer = "ifw_plots_trees_coppices")

colnames(trees_coppices)

n_tree_cdom <- 10

# --- CDOM depuis les arbres ---
cdom_by_plot <- trees_coppices %>%
  st_drop_geometry() %>%
  filter(!is.na(CIR), CIR > 0,
         !is.na(NHA), NHA > 0) %>%
  group_by(IGN, NPL) %>%
  arrange(desc(CIR), .by_group = TRUE) %>%
  mutate(
    cumN = cumsum(NHA),
    idd = row_number(),
    whup = cumN > n_tree_cdom,
    whup2 = ifelse(any(whup), which(whup)[1], NA_integer_),
    
    cumN2 = ifelse(whup, n_tree_cdom, cumN),
    diff = n_tree_cdom - lag(cumN2, default = 0),
    
    NHA2 = case_when(
      !is.na(whup2) & idd < whup2 ~ NHA,
      !is.na(whup2) & idd == whup2 ~ diff,
      is.na(whup2) ~ NHA,  # si on n'atteint jamais 10 tiges/ha
      TRUE ~ 0
    ),
    Cha = CIR * NHA2
  ) %>%
  summarise(
    cdom = sum(Cha, na.rm = TRUE) / sum(NHA2, na.rm = TRUE),
    n_ha_used = sum(NHA2, na.rm = TRUE),
    .groups = "drop"
  )

# --- Jointure sur la couche placettes ---
plots_cdom <- plots %>%
  left_join(cdom_by_plot, by = c("IGN", "NPL"))




#essmaj


library(dplyr)
library(sf)

# % de GHA par essence et par placette
gha_pct <- trees_coppices %>%
  st_drop_geometry() %>%
  filter(!is.na(ESS_D), ESS_D != "",
         !is.na(GHA), GHA > 0) %>%
  group_by(IGN, NPL, ESS_D) %>%
  summarise(GHA_ess = sum(GHA, na.rm = TRUE), .groups = "drop") %>%
  group_by(IGN, NPL) %>%
  mutate(pct = 100 * GHA_ess / sum(GHA_ess, na.rm = TRUE)) %>%
  ungroup()

# format "ESS-xx%;ESS-yy%" mais si la 2e est 0% -> on n'affiche que la 1ère
essmaj_by_plot <- gha_pct %>%
  group_by(IGN, NPL) %>%
  arrange(desc(pct), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  mutate(pct_r = round(pct, 0)) %>%   # change ici si tu veux 1 décimale
  summarise(
    essmaj = {
      e <- ESS_D
      p <- pct_r
      # garder uniquement les parts > 0
      keep <- which(!is.na(e) & e != "" & p > 0)
      if (length(keep) == 0) NA_character_
      else paste0(paste0(e[keep], "-", p[keep], "%"), collapse = ";")
    },
    .groups = "drop"
  )

essmaj_by_plot

plots_out <- plots %>%
  left_join(cdom_by_plot,   by = c("IGN", "NPL")) %>%
  left_join(essmaj_by_plot, by = c("IGN", "NPL"))

plots_out




essD_vals <- trees_coppices %>%
  st_drop_geometry() %>%
  distinct(ESS_D) %>%
  arrange(ESS_D)

essD_vals





#typologie ; 

library(dplyr)
library(sf)
library(stringr)
library(tidyr)

# Dico explicite : ESS_D -> code utilisé par ta typologie
ess_map <- tibble::tribble(
  ~ESS_D,                 ~code,
  "Hêtre",                "HE",
  "Frêne",                "FR",
  "Merisier",             "MR",
  "Chêne rouge",          "CR",
  "Chênes indigènes",     "CH",
  
  # érables : on garde 3 codes comme dans tes règles
  "Erable sycomore",      "ER",
  "Erable plane",         "EP",
  "Erable champêtre",     "ES",
  
  # feuillus déjà présents dans ta liste feuillus (codes_feuillus)
  "Charme",               "CA",
  "Bouleau",              "BOU",
  "Tilleuls",             "TIs",
  "Peuplier hybride",     "PPN",
  "Peuplier tremble",     "PPN",
  "Alisiers",             "AUs",
  
  # tout le reste feuillus -> on met un code générique FD (feuillus divers)
  "Aubépines",            "FD",
  "Aulne blanc",          "FD",
  "Aulne glutineux",      "FD",
  "Autres saules",        "FD",
  "Cerisiers",            "FD",
  "Châtaignier",          "FD",
  "Feuillus divers",      "FD",
  "Marronnier",           "FD",
  "Noisetier",            "FD",
  "Noyers",               "FD",
  "Ormes",                "FD",
  "Pommier",              "FD",
  "Robinier",             "FD",
  "Saule marsault",       "FD",
  "Sorbier",              "FD",
  "Sureaux",              "FD",
  
  # résineux -> code générique RES (hors feuillus)
  "Autres sapins",        "RES",
  "Cyprès",               "RES",
  "Epicéa commun",        "RES",
  "Epicéa de Sitka",      "RES",
  "Mélèze",               "RES",
  "Pin noir d'Autriche",  "RES",
  "Pin noir de Corse",    "RES",
  "Pin sylvestre",        "RES",
  "Sapin de Douglas",     "RES",
  "Sapin de Vancouver",   "RES",
  "Sapin pectiné",        "RES",
  "Tsuga",                "RES"
)

# Check : valeurs non mappées (doit être vide)
missing_map <- trees_coppices %>%
  st_drop_geometry() %>%
  distinct(ESS_D) %>%
  anti_join(ess_map, by = "ESS_D")

missing_map


# =========================================================
# 0) Construire gha_pct_code (obligatoire avant le reste)
# =========================================================
gha_pct_code <- trees_coppices %>%
  st_drop_geometry() %>%
  filter(!is.na(ESS_D), ESS_D != "", !is.na(GHA), GHA > 0) %>%
  left_join(ess_map, by = "ESS_D") %>%
  mutate(code = ifelse(is.na(code), "FD", code)) %>%  # sécurité
  group_by(IGN, NPL, code) %>%
  summarise(GHA_ess = sum(GHA, na.rm = TRUE), .groups = "drop") %>%
  group_by(IGN, NPL) %>%
  mutate(pct = 100 * GHA_ess / sum(GHA_ess, na.rm = TRUE)) %>%
  ungroup()

# =========================================================
# 1) essmaj (affichage top2 arrondi, sans doublon 100/0)
# =========================================================
essmaj_by_plot <- gha_pct_code %>%
  group_by(IGN, NPL) %>%
  arrange(desc(pct), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  mutate(pct_r = round(pct, 0)) %>%
  summarise(
    essmaj = {
      keep <- which(pct_r > 0)
      paste0(paste0(code[keep], "-", pct_r[keep], "%"), collapse = ";")
    },
    .groups = "drop"
  )

# =========================================================
# 2) Code dominant (sur % NON arrondis) -> règle spéciale érable
# =========================================================
codes_chene  <- c("CH","CP","CS","CHs")
codes_erable <- c("ER","EP","ES")

dominant_code <- gha_pct_code %>%
  group_by(IGN, NPL) %>%
  slice_max(order_by = pct, n = 1, with_ties = FALSE) %>%
  transmute(IGN, NPL, code_dom = code)

# =========================================================
# 3) Totaux pour les seuils (sur % complets)
# =========================================================
seuil_pur   <- 66.7
seuil_autre <- 50

codes_nobles <- c("HE", codes_chene, "FR", codes_erable, "MR", "CR")
codes_feuillus <- c("HE", codes_chene, "FR", codes_erable, "MR", "CR",
                    "CA","AUs","BOU","BOs","TIs","PPN","PG","FD")

plot_sum <- gha_pct_code %>%
  group_by(IGN, NPL) %>%
  summarise(
    total_chene  = sum(pct[code %in% codes_chene],  na.rm = TRUE),
    total_erable = sum(pct[code %in% codes_erable], na.rm = TRUE),
    total_he     = sum(pct[code == "HE"], na.rm = TRUE),
    total_fr     = sum(pct[code == "FR"], na.rm = TRUE),
    total_mr     = sum(pct[code == "MR"], na.rm = TRUE),
    total_cr     = sum(pct[code == "CR"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(dominant_code, by = c("IGN","NPL"))

# =========================================================
# 4) Proxy "visibles dans les 2 dominantes" (comme ton code)
# =========================================================
parser_essmaj <- function(x) {
  x <- str_replace_all(x, "\\s+", "")
  code1 <- str_match(x, "^([A-Za-z]+)-")[,2]
  p1    <- as.numeric(str_match(x, "-([0-9]+)\\%")[,2])
  code2 <- str_match(x, "\\%;([A-Za-z]+)-")[,2]
  p2    <- as.numeric(str_match(x, "\\%;[A-Za-z]+-([0-9]+)\\%")[,2])
  p2[is.na(p2)] <- 0
  tibble(code1=code1, p1=p1, code2=code2, p2=p2)
}

parsed <- parser_essmaj(essmaj_by_plot$essmaj)

top2_proxy <- essmaj_by_plot %>%
  bind_cols(parsed) %>%
  transmute(
    IGN, NPL,
    nobles_visibles =
      ifelse(code1 %in% codes_nobles, p1, 0) +
      ifelse(!is.na(code2) & code2 %in% codes_nobles, p2, 0),
    feuillus_visibles =
      ifelse(code1 %in% codes_feuillus, p1, 0) +
      ifelse(!is.na(code2) & code2 %in% codes_feuillus, p2, 0)
  )

# =========================================================
# 5) Typologie finale (tes règles, dans l'ordre)
# =========================================================
typo_by_plot <- plot_sum %>%
  left_join(top2_proxy, by = c("IGN","NPL")) %>%
  mutate(
    nobles_visibles = ifelse(is.na(nobles_visibles), 0, nobles_visibles),
    feuillus_visibles = ifelse(is.na(feuillus_visibles), 0, feuillus_visibles),
    typologie = case_when(
      # règle spéciale : dominante = érable (sans seuil)
      code_dom %in% codes_erable ~ "Erabliere",
      
      # types purs
      total_chene  >= seuil_pur ~ "Chenaie",
      total_he     >= seuil_pur ~ "Hetraie",
      total_fr     >= seuil_pur ~ "Frenaie",
      total_erable >= seuil_pur ~ "Erabliere",
      total_mr     >= seuil_pur ~ "Merisier",
      total_cr     >= seuil_pur ~ "Chene rouge",
      
      # mélanges spécifiques
      (total_he + total_chene) >= seuil_pur & total_he < seuil_pur & total_chene < seuil_pur ~ "Hetre - Chene",
      (total_fr + total_chene) >= seuil_pur & total_fr < seuil_pur & total_chene < seuil_pur ~ "Chene - Frene",
      
      # autres nobles (proxy top2)
      nobles_visibles >= seuil_pur ~ "Autres feuillus nobles",
      
      # autres feuillus (proxy top2)
      feuillus_visibles >= seuil_autre ~ "Autres peuplements feuillus",
      
      TRUE ~ "Peuplement melange"
    )
  ) %>%
  select(IGN, NPL, typologie)

# =========================================================
# 6) Assembler + joindre à plots
# =========================================================
result_plot <- essmaj_by_plot %>%
  left_join(typo_by_plot, by = c("IGN","NPL"))

plots_typo <- plots %>%
  left_join(result_plot, by = c("IGN","NPL"))

# checks
result_plot %>% count(typologie, sort = TRUE)
head(result_plot)




# typologie --> cdom de L'UE (si 2 cdom ex equo --> on prend l'essence avec le + gros GHA)


essence_max_tree <- trees_coppices %>%
  st_drop_geometry() %>%
  filter(!is.na(CIR), CIR > 0, !is.na(ESS_D), ESS_D != "") %>%
  group_by(IGN, NPL) %>%
  slice_max(order_by = CIR, n = 1, with_ties = FALSE) %>%
  transmute(IGN, NPL, CIR_max = CIR, ess_max = ESS_D)



# =========================================================
# ESSMAJ "mature" (top 40 tiges/ha les plus grosses) - IPRFW GPKG
# -> calcule essmaj_mature_40 en codes (HE/CH/FR/...)
# =========================================================

n_tree_essMatureMaj <- 40

# 1) Base arbres avec code + variables utiles
trees_base <- trees_coppices %>%
  st_drop_geometry() %>%
  filter(!is.na(IGN), !is.na(NPL),
         !is.na(CIR), CIR > 0,
         !is.na(NHA), NHA > 0,
         !is.na(GHA), GHA > 0,
         !is.na(ESS_D), ESS_D != "") %>%
  left_join(ess_map, by = "ESS_D") %>%
  mutate(code = ifelse(is.na(code), "FD", code))  # sécurité

# 2) Sélection des "40 plus grosses tiges/ha" (pondération partielle sur la dernière)
trees_top40 <- trees_base %>%
  group_by(IGN, NPL) %>%
  arrange(desc(CIR), .by_group = TRUE) %>%
  mutate(
    cumN = cumsum(NHA),
    idd = row_number(),
    whup = cumN > n_tree_essMatureMaj,
    whup2 = ifelse(any(whup), which(whup)[1], NA_integer_),
    
    cumN2 = ifelse(whup, n_tree_essMatureMaj, cumN),
    diff = n_tree_essMatureMaj - lag(cumN2, default = 0),
    
    NHA2 = case_when(
      !is.na(whup2) & idd < whup2  ~ NHA,
      !is.na(whup2) & idd == whup2 ~ diff,
      is.na(whup2)                ~ NHA,   # si on n'atteint jamais 40 tiges/ha
      TRUE                        ~ 0
    ),
    GHA2 = GHA * (NHA2 / NHA)  # on garde la GHA au prorata si l'arbre est "partiellement" compté
  ) %>%
  filter(NHA2 > 0, GHA2 > 0) %>%
  ungroup()

# 3) Composition GHA par code sur ces top40 + format essmaj_mature_40
gha_ess_mature <- trees_top40 %>%
  group_by(IGN, NPL, code) %>%
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
    essmaj_mature_40 = {
      keep <- which(pct_r > 0)
      if (length(keep) == 0) NA_character_
      else paste0(paste0(code[keep], "-", pct_r[keep], "%"), collapse = ";")
    },
    .groups = "drop"
  )

# 4) Jointure dans tes sorties
plots_out <- plots_out %>%
  left_join(essmaj_mature_40, by = c("IGN","NPL"))

# check rapide
head(plots_out %>% st_drop_geometry() %>% select(IGN, NPL, essmaj, essmaj_mature_40))



tab_final <- plots %>%
  st_drop_geometry() %>%
  select(IGN, NPL, Peup_D) %>%
  left_join(essmaj_by_plot,      by = c("IGN","NPL")) %>%
  left_join(essmaj_mature_40,    by = c("IGN","NPL")) %>%   # <-- AJOUT ICI
  left_join(cdom_by_plot,        by = c("IGN","NPL")) %>%
  left_join(essence_max_tree,    by = c("IGN","NPL")) %>%
  select(IGN, NPL, Peup_D, essmaj, essmaj_mature_40, cdom, CIR_max, ess_max) %>%
  arrange(IGN, NPL)


# boxplot dendro en fct de ess_max

library(ggplot2)
library(dplyr)

min_n <- 15

tab_plot <- tab_final %>%
  filter(!is.na(cdom), !is.na(ess_max), ess_max != "") %>%
  add_count(ess_max, name = "n") %>%
  filter(n >= min_n) %>%
  mutate(ess_max = reorder(ess_max, cdom, FUN = median, na.rm = TRUE))

stats_ess <- tab_plot %>%
  group_by(ess_max) %>%
  summarise(
    mean = mean(cdom, na.rm = TRUE),
    sd = sd(cdom, na.rm = TRUE),
    med = median(cdom, na.rm = TRUE),
    y = max(cdom, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(tab_plot, aes(x = ess_max, y = cdom)) +
  geom_boxplot() +
  geom_text(
    data = stats_ess,
    aes(x = ess_max, y = y,
        label = paste0("med=", round(med, 1),
                       " | mean=", round(mean, 1),
                       " | sd=", round(sd, 1))),
    vjust = -0.3,
    size = 3
  ) +
  coord_flip() +
  labs(
    x = "ess_max",
    y = "cdom",
    title = paste0("CDOM par ess_max (n ≥ ", min_n, ")")
  ) +
  theme_minimal()

# --- Export PDF ---
out_pdf <- "C:/Old_Growth_Forest/DATA/outputs/boxplot_cdom_par_ess_max.pdf"
dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)

ggsave(out_pdf, plot = p, device = "pdf", width = 11, height = 8.5)  # format paysage

out_pdf





library(sf)
library(dplyr)

# (si pas déjà fait)
gpkg_trees <- "C:/Old_Growth_Forest/DATA/données_iprfw/ifw_plots_trees_coppices.gpkg"
trees_coppices <- st_read(gpkg_trees, layer = "ifw_plots_trees_coppices", quiet = TRUE)

tc <- trees_coppices %>% st_drop_geometry()

# 1) Toutes les modalités de ESS (code) + nb d'arbres
ess_vals <- tc %>%
  filter(!is.na(ESS), ESS != "") %>%
  count(ESS, sort = TRUE)

ess_vals

# 2) Toutes les modalités de ESS_D (nom FR) + nb d'arbres
essd_vals <- tc %>%
  filter(!is.na(ESS_D), ESS_D != "") %>%
  count(ESS_D, sort = TRUE)

essd_vals

# 3) Table de correspondance ESS <-> ESS_D (+ nb)
ess_map_vals <- tc %>%
  filter(!is.na(ESS), ESS != "", !is.na(ESS_D), ESS_D != "") %>%
  count(ESS, ESS_D, sort = TRUE)

ess_map_vals

# 4) Check : est-ce qu’un code ESS correspond à plusieurs ESS_D ?
ess_one_to_many <- ess_map_vals %>%
  group_by(ESS) %>%
  summarise(n_names = n_distinct(ESS_D), .groups = "drop") %>%
  filter(n_names > 1) %>%
  arrange(desc(n_names))

ess_one_to_many

