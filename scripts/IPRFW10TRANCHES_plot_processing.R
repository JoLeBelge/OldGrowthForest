# =========================================================
# IPRFW (CSV) -> CDOM + typologie_mature_simplifiee + seuils + masque FA + HDOM 18m
# - mêmes sorties que ton script initial (CSV + PDF + tableau large + HDOM)
# =========================================================

library(readr)
library(dplyr)
library(sf)
library(stringr)
library(stringi)
library(tidyr)
library(ggplot2)
library(terra)

# ---- chemins
plots_path <- "C:/Users/Lemans Léa/Documents/20260202_requete_foret_mature_plots.csv"
trees_path <- "C:/Users/Lemans Léa/Documents/20260202_requete_foret_mature_trees.csv"
dico_path  <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/dictionnaire_essences.csv"

path_seuils <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/outputs/seuils_cdom_q05_q10_q25_q50_q75_q90_par_typologie_mature_simplifiee.csv"
mask_path   <- "C:/Old_Growth_Forest/raster_couchesforestimator/raster/raster/FA_mask12_01.tif"
hdom_path   <- "C:/Old_Growth_Forest/raster_10m/dendro_hdom_10m.tif"
out_dir     <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/outputs"

# ---- paramètres
codes_exclure   <- c(2, 24)  # chêne rouge, peuplier hybride
n_tree_cdom     <- 10
n_tree_mature   <- 40
quantiles_keep  <- c("q05","q10","q25","q50","q75")
HA_PER_POINT    <- 50

# =========================================================
# 0) READ + DICO + SF + FILTRE CONDITION
# =========================================================

# ---- read
plots <- read_csv(plots_path, locale = locale(encoding = "Windows-1252"), show_col_types = FALSE)
trees <- read_csv(trees_path, locale = locale(encoding = "Windows-1252"), show_col_types = FALSE)
dico  <- read_csv(dico_path,  locale = locale(encoding = "UTF-8"),        show_col_types = FALSE)

# ---- accents (pour jointure stable)
trees <- trees %>% mutate(name = stri_trans_general(name, "Latin-ASCII"))

# ---- dico -> ajoute ESS (code) à trees (join sur name)
trees <- trees %>% mutate(name_key = str_to_lower(str_trim(name)))

dico2 <- dico %>%
  mutate(name_key = str_to_lower(str_trim(stri_trans_general(name, "Latin-ASCII")))) %>%
  transmute(name_key, ESS = as.integer(code), ESS_D = name) %>%
  distinct(name_key, .keep_all = TRUE)

trees <- trees %>%
  left_join(dico2, by = "name_key") %>%
  select(-name_key)

# ---- plots -> sf : GPS si dispo sinon lon/lat théorique (WGS84 -> Lambert 72)
plots_sf <- plots %>%
  mutate(
    lon = coalesce(x_gps, longitude_theo),
    lat = coalesce(y_gps, latitude_theo)
  ) %>%
  filter(is.finite(lon), is.finite(lat)) %>%
  transmute(IGN = ign, NPL = npl, lon, lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(31370)

# ---- mapping trees (nouvelles colonnes -> celles du script) + condition
trees <- trees %>%
  transmute(
    IGN = ign, NPL = npl,
    CIR = circumference,
    NHA = nha,
    GHA = gha,
    ESS = ESS,      # code (ajouté via dico)
    ESS_D = name,   # libellé (sans accents)
    condition = condition
  )

# ---- filtre statut : on garde "arbres vivants"
trees_ok <- trees %>%
  filter(!(condition %in% c("Absent","Chablis récent","Coupé","Hors du rayon réduit",
                            "Mort à terre","Mort sur pied"))) %>%
  filter(is.finite(CIR), CIR > 0,
         is.finite(NHA), NHA > 0,
         is.finite(GHA), GHA > 0,
         !is.na(ESS))

# =========================================================
# fonctions (CDOM + parser)
# =========================================================
weighted_topN_by_plot <- function(df, N_target, value_col="CIR", weight_col="NHA"){
  df %>%
    filter(is.finite(.data[[value_col]]), .data[[value_col]]>0,
           is.finite(.data[[weight_col]]), .data[[weight_col]]>0) %>%
    group_by(IGN, NPL) %>%
    arrange(desc(.data[[value_col]]), .by_group=TRUE) %>%
    mutate(
      cumN=cumsum(.data[[weight_col]]),
      idd=row_number(),
      whup=cumN>N_target,
      whup2=ifelse(any(whup), which(whup)[1], NA_integer_),
      cumN2=ifelse(whup, N_target, cumN),
      diff=N_target - lag(cumN2, default=0),
      w2=case_when(
        !is.na(whup2) & idd < whup2  ~ .data[[weight_col]],
        !is.na(whup2) & idd == whup2 ~ diff,
        is.na(whup2)                 ~ .data[[weight_col]],
        TRUE                         ~ 0
      ),
      vw=.data[[value_col]]*w2
    ) %>%
    summarise(val=sum(vw,na.rm=TRUE)/sum(w2,na.rm=TRUE), .groups="drop")
}

parser_essmaj_num <- function(x){
  x <- ifelse(is.na(x), "", x) %>% str_replace_all("\\s+","")
  tibble(
    code1 = suppressWarnings(as.integer(str_match(x, "^([0-9]+)-")[,2])),
    p1    = suppressWarnings(as.numeric(str_match(x, "-([0-9]+)\\%")[,2])),
    code2 = suppressWarnings(as.integer(str_match(x, "\\%;([0-9]+)-")[,2])),
    p2    = suppressWarnings(as.numeric(str_match(x, "\\%;[0-9]+-([0-9]+)\\%")[,2]))
  ) %>% mutate(p1=replace_na(p1,0), p2=replace_na(p2,0))
}

# =========================================================
# 1) filtre "feuillus indigènes" via ess_max (plus gros arbre)
# =========================================================
essence_max_tree <- trees_ok %>%
  group_by(IGN, NPL) %>%
  slice_max(order_by = CIR, n = 1, with_ties = FALSE) %>%
  transmute(IGN, NPL, ess_max_code = ESS, ess_max = ESS_D) %>%
  ungroup()

plots_exclus <- essence_max_tree %>%
  filter(ess_max_code > 40 | ess_max_code %in% codes_exclure) %>%
  distinct(IGN, NPL)

plots_ok  <- plots_sf %>% anti_join(plots_exclus, by=c("IGN","NPL"))
trees_ok2 <- trees_ok %>% anti_join(plots_exclus, by=c("IGN","NPL"))

# =========================================================
# 2) CDOM (10 tiges/ha)
# =========================================================
cdom_by_plot <- weighted_topN_by_plot(trees_ok2, n_tree_cdom, "CIR","NHA") %>%
  rename(cdom = val)

# =========================================================
# 3) ESSMAJ mature 40 (top40 tiges/ha, top2 essences %GHA)
# =========================================================
trees_top40 <- trees_ok2 %>%
  group_by(IGN, NPL) %>%
  arrange(desc(CIR), .by_group=TRUE) %>%
  mutate(
    cumN=cumsum(NHA),
    idd=row_number(),
    whup=cumN>n_tree_mature,
    whup2=ifelse(any(whup), which(whup)[1], NA_integer_),
    cumN2=ifelse(whup, n_tree_mature, cumN),
    diff=n_tree_mature - lag(cumN2, default=0),
    NHA2=case_when(
      !is.na(whup2) & idd < whup2  ~ NHA,
      !is.na(whup2) & idd == whup2 ~ diff,
      is.na(whup2)                 ~ NHA,
      TRUE                         ~ 0
    ),
    GHA2=GHA*(NHA2/NHA)
  ) %>%
  filter(NHA2>0, GHA2>0) %>%
  ungroup()

gha_ess_mature <- trees_top40 %>%
  group_by(IGN,NPL,ESS,ESS_D) %>%
  summarise(GHA_mature=sum(GHA2,na.rm=TRUE), .groups="drop")

gha_tot_mature <- gha_ess_mature %>%
  group_by(IGN,NPL) %>%
  summarise(GHA_mature_tot=sum(GHA_mature,na.rm=TRUE), .groups="drop")

essmaj_mature_40 <- gha_ess_mature %>%
  left_join(gha_tot_mature, by=c("IGN","NPL")) %>%
  mutate(pct=100*GHA_mature/GHA_mature_tot) %>%
  group_by(IGN,NPL) %>%
  arrange(desc(pct), .by_group=TRUE) %>%
  slice_head(n=2) %>%
  mutate(pct_r=round(pct,0)) %>%
  summarise(
    essmaj_mature_40_code = {
      keep <- which(pct_r>0)
      if(length(keep)==0) NA_character_
      else paste0(paste0(ESS[keep],"-",pct_r[keep],"%"), collapse=";")
    },
    .groups="drop"
  )

# =========================================================
# 4) TAB_FINAL + TYPOLOGIE
# =========================================================
tab_final <- plots_ok %>%
  left_join(cdom_by_plot, by=c("IGN","NPL")) %>%
  left_join(essmaj_mature_40, by=c("IGN","NPL")) %>%
  filter(!is.na(cdom), !is.na(essmaj_mature_40_code), essmaj_mature_40_code!="")

# règles typologie (identiques)
seuil_pur  <- 66.7
seuil_autre<- 50
seuil_dom  <- 50

code_he <- 3
code_fr <- 5
codes_chene <- c(1)
codes_erable <- c(4,16,17)

codes_nobles  <- c(code_he, codes_chene, code_fr, codes_erable, 7, 2)
codes_feuillus<- c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,
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
  select(-code1,-p1,-code2,-p2) %>%
  filter(typologie_mature_simplifiee != "Peuplement melange")

# =========================================================
# 5) SEUILS MULTI-QUANTILES
# =========================================================
seuils_multi <- read.csv2(path_seuils, stringsAsFactors = FALSE) %>%
  rename(typologie_mature_simplifiee = typologie) %>%
  select(typologie_mature_simplifiee, any_of(quantiles_keep))

seuils_long <- seuils_multi %>%
  pivot_longer(cols = starts_with("q"), names_to="quantile", values_to="seuil_cdom") %>%
  mutate(q = as.numeric(sub("^q","0.",quantile)))

# =========================================================
# 6) MASQUE FA + SENSIBILITE
# =========================================================
mask_fa <- rast(mask_path)

if (!is.na(st_crs(tab_final)) && !is.na(crs(mask_fa)) && st_crs(tab_final)$wkt != crs(mask_fa)) {
  tab_final <- st_transform(tab_final, crs(mask_fa))
}

val <- terra::extract(mask_fa, terra::vect(tab_final))
tab_final$fa_mask <- val[[2]]

tab_fa <- tab_final %>% filter(!is.na(fa_mask) & fa_mask == 1)

tab_scored_fa <- tab_fa %>%
  left_join(seuils_long, by="typologie_mature_simplifiee", relationship="many-to-many") %>%
  mutate(pass = cdom >= seuil_cdom)

sens_by_typo_fa <- tab_scored_fa %>%
  group_by(typologie_mature_simplifiee, q, quantile) %>%
  summarise(
    n_avant  = n(),
    n_apres  = sum(pass, na.rm=TRUE),
    ha_avant = n_avant * HA_PER_POINT,
    ha_apres = n_apres * HA_PER_POINT,
    .groups="drop"
  ) %>%
  mutate(pct_surface = 100 * ha_apres / ha_avant) %>%
  arrange(typologie_mature_simplifiee, q)

sens_global_fa <- sens_by_typo_fa %>%
  group_by(q, quantile) %>%
  summarise(
    n_avant  = sum(n_avant),
    n_apres  = sum(n_apres),
    ha_avant = sum(ha_avant),
    ha_apres = sum(ha_apres),
    .groups="drop"
  ) %>%
  mutate(pct_surface = 100 * ha_apres / ha_avant) %>%
  arrange(q)

tab_cdom_typo_fa <- tab_fa %>%
  st_drop_geometry() %>%
  group_by(typologie_mature_simplifiee) %>%
  summarise(
    n_plots    = sum(is.finite(cdom)),
    cdom_mean  = mean(cdom, na.rm = TRUE),
    cdom_median= median(cdom, na.rm = TRUE),
    cdom_sd    = sd(cdom, na.rm = TRUE),
    cdom_min   = min(cdom, na.rm = TRUE),
    cdom_max   = max(cdom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_plots))

write.csv2(tab_cdom_typo_fa,
           file.path(out_dir, "iprfw_CDOM_stats_par_typologie_FA.csv"),
           row.names = FALSE)


# ---- exports + graphs (comme avant)
out_csv_recap_multi_fa <- file.path(out_dir, "sensibilite_iprfw_dans_FA_par_typologie.csv")
out_csv_recap_glob_fa  <- file.path(out_dir, "sensibilite_iprfw_dans_FA_global.csv")
out_pdf_sens_fa        <- file.path(out_dir, "graph_sensibilite_iprfw_dans_FA.pdf")

write.csv2(sens_by_typo_fa, out_csv_recap_multi_fa, row.names=FALSE)
write.csv2(sens_global_fa, out_csv_recap_glob_fa, row.names=FALSE)

p_pct_fa <- ggplot(sens_by_typo_fa,
                   aes(x=q, y=pct_surface, group=typologie_mature_simplifiee, color=typologie_mature_simplifiee)) +
  geom_line(linewidth=0.8) + geom_point(size=2) +
  scale_x_continuous(breaks=c(.05,.10,.25,.50,.75)) +
  theme_minimal() +
  labs(title="Sensibilité aux seuils CDOM — dans forêt ancienne (FA)",
       x="Quantile terrain utilisé comme seuil CDOM",
       y="% surface retenue (proxy 50 ha/point)",
       color="Typologie")

p_pct_glob_fa <- ggplot(sens_global_fa, aes(x=q, y=pct_surface)) +
  geom_line(linewidth=0.9) + geom_point(size=2) +
  scale_x_continuous(breaks=c(.05,.10,.25,.50,.75)) +
  theme_minimal() +
  labs(title="Sensibilité globale — dans forêt ancienne (FA)",
       x="Quantile terrain utilisé comme seuil CDOM",
       y="% surface retenue (proxy 50 ha/point)")

pdf(out_pdf_sens_fa, width=11, height=8.5)
print(p_pct_fa)
print(p_pct_glob_fa)
dev.off()

# =========================================================
# 7) TABLEAU LARGE "COMME EXCEL"
# =========================================================
seuils_wide <- seuils_long %>%
  select(typologie_mature_simplifiee, quantile, seuil_cdom) %>%
  mutate(quantile = paste0(quantile, "_cdom")) %>%
  pivot_wider(names_from = quantile, values_from = seuil_cdom)

n_avant_by_typo_fa <- tab_fa %>%
  st_drop_geometry() %>%
  count(typologie_mature_simplifiee, name="n_avant")

res_wide_fa <- sens_by_typo_fa %>%
  select(typologie_mature_simplifiee, quantile, n_apres, pct_surface) %>%
  mutate(
    n_apres_name = paste0("n_apres_", quantile),
    pct_name     = paste0("pct_gardes_", quantile)
  ) %>%
  select(typologie_mature_simplifiee, n_apres_name, n_apres, pct_name, pct_surface) %>%
  pivot_longer(cols=c(n_apres_name, pct_name), names_to="tmp", values_to="colname") %>%
  mutate(value = ifelse(tmp=="n_apres_name", n_apres, pct_surface)) %>%
  select(typologie_mature_simplifiee, colname, value) %>%
  pivot_wider(names_from=colname, values_from=value)

tableau_excel_fa <- n_avant_by_typo_fa %>%
  left_join(seuils_wide, by="typologie_mature_simplifiee") %>%
  left_join(res_wide_fa, by="typologie_mature_simplifiee") %>%
  arrange(typologie_mature_simplifiee)

out_csv_tableau_excel_fa <- file.path(out_dir, "tableau_excel_sensibilite_FA.csv")
write.csv2(tableau_excel_fa, out_csv_tableau_excel_fa, row.names=FALSE)

# =========================================================
# 8) HDOM (buffer 18m) -> stats par typologie + exports
# =========================================================
r_hdom <- rast(hdom_path) / 100  # cm->m (si déjà m, enlève /100)

if (!is.na(st_crs(tab_fa)) && !is.na(crs(r_hdom)) && st_crs(tab_fa)$wkt != crs(r_hdom)) {
  tab_fa <- st_transform(tab_fa, crs(r_hdom))
}

buf18 <- st_buffer(tab_fa, 18)
vbuf  <- vect(buf18)

tab_fa$hdom_mean_m   <- extract(r_hdom, vbuf, fun=mean,   na.rm=TRUE, touches=TRUE)[,2]
tab_fa$hdom_median_m <- extract(r_hdom, vbuf, fun=median, na.rm=TRUE, touches=TRUE)[,2]
tab_fa$n_pix_hdom    <- extract(r_hdom, vbuf, fun=function(x) sum(!is.na(x)), touches=TRUE)[,2]


tab_hdom_typo <- tab_fa %>%
  st_drop_geometry() %>%
  filter(is.finite(hdom_mean_m)) %>%
  group_by(typologie_mature_simplifiee) %>%
  summarise(
    n_plots      = n(),
    hdom_mean    = mean(hdom_mean_m),
    hdom_median  = median(hdom_mean_m),
    hdom_sd      = sd(hdom_mean_m),
    hdom_se      = hdom_sd / sqrt(n_plots),
    hdom_min     = min(hdom_mean_m),
    hdom_max     = max(hdom_mean_m),
    .groups = "drop"
  ) %>%
  arrange(desc(n_plots))


write.csv2(st_drop_geometry(tab_fa),
           file.path(out_dir, "iprfw_tab_FA_avec_HDOM_buffer18m.csv"),
           row.names=FALSE)

write.csv2(tab_hdom_typo,
           file.path(out_dir, "iprfw_HDOM_stats_par_typologie_FA_buffer18m.csv"),
           row.names=FALSE)

cat("OK — sorties écrites dans :", out_dir, "\n")

j'ai une b'