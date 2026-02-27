library(DBI)
library(RSQLite)
library(dplyr)

db_path <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"
stopifnot(file.exists(db_path))

con <- dbConnect(RSQLite::SQLite(), db_path)
stopifnot(all(c("ogf","ues","dendro_stand") %in% dbListTables(con)))

ogf <- dbReadTable(con, "ogf")
ues <- dbReadTable(con, "ues")
dendro_stand <- dbReadTable(con, "dendro_stand")
dbDisconnect(con)

# -----------------------
# Helpers: colonnes robustes
# -----------------------
pick_col <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}
get_num <- function(df, colname) {
  if (is.na(colname)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[colname]]))
}

# Colonnes (moyennes + SE si dispo)
col_ba_mean <- pick_col(dendro_stand, c("basal_area_alive_mean", "ba_alive_mean", "gha_mean", "gha_alive_mean"))
col_ba_se   <- pick_col(dendro_stand, c("basal_area_alive_se", "basal_area_alive_sem", "ba_alive_se", "gha_se", "gha_mean_se"))

col_vol_alive_mean <- pick_col(dendro_stand, c("vol_alive_mean", "volume_alive_mean"))
col_vol_alive_se   <- pick_col(dendro_stand, c("vol_alive_se", "volume_alive_se"))

col_dead_st_mean <- pick_col(dendro_stand, c("vol_dead_standing_mean"))
col_dead_st_se   <- pick_col(dendro_stand, c("vol_dead_standing_se"))

col_fas_mean <- pick_col(dendro_stand, c("vol_wood_debris_FAS_mean"))
col_fas_se   <- pick_col(dendro_stand, c("vol_wood_debris_FAS_se"))

col_lis_mean <- pick_col(dendro_stand, c("vol_wood_debris_LIS_mean"))
col_lis_se   <- pick_col(dendro_stand, c("vol_wood_debris_LIS_se"))

col_deadw_mean <- pick_col(dendro_stand, c("vol_deadw_mean"))
col_deadw_se   <- pick_col(dendro_stand, c("vol_deadw_se"))

message("Colonnes détectées :")
message("- basal area mean = ", col_ba_mean, " | se = ", col_ba_se)

# -----------------------
# Parcelles VISITÉES
# -----------------------
ids_visites <- sort(unique(ues$id_ogf))

base_visites <- ues %>%
  filter(id_ogf %in% ids_visites) %>%
  group_by(id_ogf) %>%
  summarise(n_ue = n(), .groups = "drop")

data <- base_visites %>%
  left_join(
    ogf %>% transmute(
      id_ogf = as.numeric(id),
      parcelle = vosRef,
      surf_ha = surf,
      date = date,
      description = descr
    ),
    by = "id_ogf"
  ) %>%
  left_join(dendro_stand, by = c("id_ogf" = "ues_id_ogf")) %>%
  mutate(
    # GHA moyen = surface terrière vivante (basal area alive)
    gha_moy = get_num(cur_data(), col_ba_mean),
    gha_se  = get_num(cur_data(), col_ba_se),
    
    # Volume vivant
    vol_vivant = get_num(cur_data(), col_vol_alive_mean),
    vol_vivant_se = get_num(cur_data(), col_vol_alive_se),
    
    # Bois mort sur pied
    bm_sur_pied = get_num(cur_data(), col_dead_st_mean),
    bm_sur_pied_se = get_num(cur_data(), col_dead_st_se),
    
    # Bois mort au sol = FAS + LIS
    fas = get_num(cur_data(), col_fas_mean),
    lis = get_num(cur_data(), col_lis_mean),
    bm_sol = ifelse(is.na(fas), 0, fas) + ifelse(is.na(lis), 0, lis),
    
    # SE bois mort au sol (approx) si dispo
    fas_se = get_num(cur_data(), col_fas_se),
    lis_se = get_num(cur_data(), col_lis_se),
    bm_sol_se = ifelse(
      is.na(fas_se) & is.na(lis_se), NA_real_,
      sqrt((ifelse(is.na(fas_se), 0, fas_se)^2) + (ifelse(is.na(lis_se), 0, lis_se)^2))
    ),
    
    # Total bois mort
    bm_total = {
      tot <- get_num(cur_data(), col_deadw_mean)
      ifelse(is.na(tot), bm_sur_pied + bm_sol, tot)
    },
    bm_total_se = {
      totse <- get_num(cur_data(), col_deadw_se)
      ifelse(is.na(totse),
             ifelse(is.na(bm_sur_pied_se) & is.na(bm_sol_se), NA_real_,
                    sqrt((ifelse(is.na(bm_sur_pied_se), 0, bm_sur_pied_se)^2) +
                           (ifelse(is.na(bm_sol_se), 0, bm_sol_se)^2))),
             totse)
    }
  ) %>%
  select(
    id_ogf, parcelle, surf_ha, date, n_ue, description,
    gha_moy, gha_se,
    vol_vivant, vol_vivant_se,
    bm_sur_pied, bm_sur_pied_se,
    bm_sol, bm_sol_se,
    bm_total, bm_total_se
  ) %>%
  arrange(id_ogf)

write.csv(data, "rapport_parcelles_visitees.csv", row.names = FALSE)

# -----------------------
# Disclaimer demandé (sans "à l'hectare")
# -----------------------
disclaimer <- paste0(
  "Note méthodologique / prudence : Pour faciliter la lecture, nous présentons ici une synthèse ",
  "à l’échelle de la parcelle (peuplement). Cette synthèse est calculée à partir de mesures ponctuelles ",
  "effectuées sur un nombre limité de placettes (plots), puis moyennées pour fournir un ordre de grandeur. ",
  "Elle ne constitue ni un inventaire exhaustif, ni une estimation destinée à être représentative de l’ensemble ",
  "du peuplement. Dans notre démarche de projet, l’exploitation des données se fait uniquement à l’échelle du plot."
)

# -----------------------
# Génération HTML
# -----------------------
dir.create("rapports", showWarnings = FALSE)

fmt <- function(x, digits = 2) ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits))
fmt_pm <- function(mean, se, digits = 2) {
  if (is.na(mean)) return("NA")
  if (is.na(se)) return(fmt(mean, digits))
  paste0(fmt(mean, digits), " ± ", fmt(se, digits))
}
safe_txt <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\n", "<br>", x)
  x
}
safe_filename <- function(x) {
  x <- ifelse(is.na(x) | x == "", "parcelle", x)
  gsub("[^A-Za-z0-9_-]", "_", x)
}

for (i in seq_len(nrow(data))) {
  r <- data[i, ]
  
  html <- sprintf(
    '<!doctype html><html lang="fr"><head><meta charset="utf-8"/>
<title>Rapport parcelle</title>
<style>
body{font-family:Arial,sans-serif;max-width:900px;margin:24px auto;padding:0 12px;color:#111;}
h1{font-size:22px;margin:0 0 10px;} h2{font-size:16px;margin:18px 0 8px;}
.box{border:1px solid #ddd;border-radius:10px;padding:12px 14px;margin:10px 0;}
table{border-collapse:collapse;width:100%%;} th,td{border-bottom:1px solid #eee;padding:8px 6px;text-align:left;}
td.val{text-align:right;font-variant-numeric:tabular-nums;} .muted{color:#666;font-size:12px;}
.small{font-size:13px;line-height:1.35;}
</style></head><body>
<h1>Rapport de parcelle — %s</h1>
<div class="muted">ID base : %s — Généré le %s</div>

<div class="box"><h2>Identité</h2><table>
<tr><th>Parcelle</th><td class="val">%s</td></tr>
<tr><th>Surface</th><td class="val">%s ha</td></tr>
<tr><th>Date (base)</th><td class="val">%s</td></tr>
<tr><th>Nombre d’UE</th><td class="val">%s</td></tr>
</table></div>

<div class="box"><h2>Indicateurs</h2>
<div class="muted">Présentation : moyenne ± erreur standard (si disponible)</div>
<table>
<tr><th>GHA moyen (surface terrière vivante)</th><td class="val">%s</td></tr>
<tr><th>Volume vivant sur pied</th><td class="val">%s</td></tr>
<tr><th>Bois mort sur pied</th><td class="val">%s</td></tr>
<tr><th>Bois mort au sol</th><td class="val">%s</td></tr>
<tr><th>Bois mort total</th><td class="val">%s</td></tr>
</table></div>

<div class="box"><h2>Description / remarques</h2><div>%s</div></div>

<div class="box"><h2>Note méthodologique</h2>
<div class="small">%s</div>
</div>

<div class="muted">Imprimer en PDF : Ctrl+P → “Enregistrer en PDF”.</div>
</body></html>',
    safe_txt(r$parcelle), r$id_ogf, format(Sys.Date()),
    safe_txt(r$parcelle), fmt(r$surf_ha, 2), safe_txt(r$date), r$n_ue,
    fmt_pm(r$gha_moy, r$gha_se, 2),
    fmt_pm(r$vol_vivant, r$vol_vivant_se, 2),
    fmt_pm(r$bm_sur_pied, r$bm_sur_pied_se, 2),
    fmt_pm(r$bm_sol, r$bm_sol_se, 2),
    fmt_pm(r$bm_total, r$bm_total_se, 2),
    safe_txt(r$description),
    safe_txt(disclaimer)
  )
  
  out <- sprintf("rapports/rapport_%03d_%s.html", r$id_ogf, safe_filename(r$parcelle))
  writeLines(html, out)
}

cat("OK ✅\n- CSV: rapport_parcelles_visitees.csv\n- Rapports HTML dans: ", normalizePath("rapports"), "\n", sep="")