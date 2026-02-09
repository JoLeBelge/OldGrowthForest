# =========================================================
# ANALYSES OGF - typologie_mature_simplifiee (TERRAIN ONLY)
# - N plots par typologie
# - Deadwood : stats vol_deadw (mean/median/sd/se) par typologie
# - Deadwood : standing/FAS/LIS (moyennes + totaux) par typologie
# - Graphs deadwood (PDF)
# - CDOM : stats + QUANTILES q05/q10/q25/q50/q75 (+ q90 bonus) par typologie
# - Histogrammes CDOM + lignes quantiles (PDF)
# =========================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# -------------------------
# 0) Chemins
# -------------------------
db_path  <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"
data_dir <- dirname(db_path)
out_dir  <- file.path(data_dir, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# fermer tout device PDF resté ouvert
while (dev.cur() > 1) dev.off()

# -------------------------
# 1) Lecture DB
# -------------------------
con <- dbConnect(RSQLite::SQLite(), db_path)
dendro_plot <- dbReadTable(con, "dendro_plot")
dbDisconnect(con)

# -------------------------
# 2) Typologie de travail
# -------------------------
typo_col <- "typologie_mature_simplifiee"
stopifnot(typo_col %in% colnames(dendro_plot))

df <- dendro_plot %>%
  mutate(typologie = .data[[typo_col]])

# -------------------------
# 3) Nombre de plots par typologie
# -------------------------
tab_n_by_typo <- df %>%
  filter(!is.na(typologie), typologie != "") %>%
  count(typologie, name = "n_plots") %>%
  arrange(desc(n_plots))

out_csv_n <- file.path(out_dir, "n_plots_par_typologie_mature_simplifiee.csv")
write.csv2(tab_n_by_typo, out_csv_n, row.names = FALSE)

cat("CSV n plots :", out_csv_n, "\n")
print(tab_n_by_typo)

# =========================================================
# DEADWOOD
# =========================================================

standard_error <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x) / sqrt(length(x))
}

# -------------------------
# 4) Deadwood : stats vol_deadw (mean/median/sd/se) par typologie
# -------------------------
tab_deadw_stats <- df %>%
  filter(!is.na(typologie), typologie != "") %>%
  group_by(typologie) %>%
  summarise(
    n = sum(is.finite(vol_deadw)),
    vol_deadw_mean   = mean(vol_deadw, na.rm = TRUE),
    vol_deadw_median = median(vol_deadw, na.rm = TRUE),
    vol_deadw_sd     = sd(vol_deadw, na.rm = TRUE),
    vol_deadw_se     = standard_error(vol_deadw),
    .groups = "drop"
  ) %>%
  arrange(desc(vol_deadw_mean))

out_csv_deadw_stats <- file.path(out_dir, "deadwood_vol_deadw_stats_par_typologie_mature_simplifiee.csv")
write.csv2(tab_deadw_stats, out_csv_deadw_stats, row.names = FALSE)
cat("CSV deadwood stats :", out_csv_deadw_stats, "\n")

# -------------------------
# 5) Deadwood composantes : moyennes par UE + totaux + % composition
# -------------------------
tab_deadw_components_mean <- df %>%
  filter(!is.na(typologie), typologie != "") %>%
  group_by(typologie) %>%
  summarise(
    n = n(),
    standing_mean = mean(vol_dead_standing, na.rm = TRUE),
    FAS_mean      = mean(vol_wood_debris_FAS, na.rm = TRUE),
    LIS_mean      = mean(vol_wood_debris_LIS, na.rm = TRUE),
    vol_deadw_mean = mean(vol_deadw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(vol_deadw_mean))

out_csv_deadw_comp_mean <- file.path(out_dir, "deadwood_components_mean_par_typologie_mature_simplifiee.csv")
write.csv2(tab_deadw_components_mean, out_csv_deadw_comp_mean, row.names = FALSE)
cat("CSV deadwood composantes (moyennes) :", out_csv_deadw_comp_mean, "\n")

tab_deadw_components_total <- df %>%
  filter(!is.na(typologie), typologie != "") %>%
  group_by(typologie) %>%
  summarise(
    standing_sum = sum(vol_dead_standing, na.rm = TRUE),
    FAS_sum      = sum(vol_wood_debris_FAS, na.rm = TRUE),
    LIS_sum      = sum(vol_wood_debris_LIS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_sum = standing_sum + FAS_sum + LIS_sum,
    pct_standing = ifelse(total_sum > 0, 100 * standing_sum / total_sum, NA_real_),
    pct_FAS      = ifelse(total_sum > 0, 100 * FAS_sum      / total_sum, NA_real_),
    pct_LIS      = ifelse(total_sum > 0, 100 * LIS_sum      / total_sum, NA_real_)
  ) %>%
  arrange(desc(total_sum))

out_csv_deadw_comp_total <- file.path(out_dir, "deadwood_components_total_par_typologie_mature_simplifiee.csv")
write.csv2(tab_deadw_components_total, out_csv_deadw_comp_total, row.names = FALSE)
cat("CSV deadwood composantes (totaux + %) :", out_csv_deadw_comp_total, "\n")

# -------------------------
# 6) Graph deadwood : barres 100% + boxplots des % par UE
# -------------------------
plot_deadw_sum <- tab_deadw_components_total %>%
  select(typologie, pct_standing, pct_FAS, pct_LIS) %>%
  pivot_longer(starts_with("pct_"), names_to = "type", values_to = "pct") %>%
  mutate(type = recode(type,
                       pct_standing = "Bois mort debout",
                       pct_FAS      = "Gros bois couché (FAS)",
                       pct_LIS      = "Bois couché moyen (LIS)"))

p1 <- ggplot(plot_deadw_sum, aes(x = typologie, y = pct, fill = type)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Deadwood par typologie (totaux) : répartition (%)",
       x = "Typologie (mature simplifiée)", y = "% du deadwood", fill = "") +
  theme(axis.text.y = element_text(size = 9))

plot_deadw_ue <- df %>%
  filter(!is.na(typologie), typologie != "") %>%
  mutate(
    pct_standing = ifelse(vol_deadw > 0, 100 * vol_dead_standing / vol_deadw, NA_real_),
    pct_FAS      = ifelse(vol_deadw > 0, 100 * vol_wood_debris_FAS / vol_deadw, NA_real_),
    pct_LIS      = ifelse(vol_deadw > 0, 100 * vol_wood_debris_LIS / vol_deadw, NA_real_)
  ) %>%
  select(typologie, pct_standing, pct_FAS, pct_LIS) %>%
  pivot_longer(starts_with("pct_"), names_to = "type", values_to = "pct") %>%
  mutate(type = recode(type,
                       pct_standing = "Bois mort debout",
                       pct_FAS      = "Gros bois couché (FAS)",
                       pct_LIS      = "Bois couché moyen (LIS)")) %>%
  filter(is.finite(pct))

p2 <- ggplot(plot_deadw_ue, aes(x = typologie, y = pct)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  facet_wrap(~type, ncol = 1) +
  theme_minimal() +
  labs(title = "Dispersion des parts (%) de deadwood par UE",
       x = "Typologie (mature simplifiée)", y = "% par UE")

out_pdf_deadw <- file.path(out_dir, "graph_deadwood_typologie_mature_simplifiee.pdf")
pdf(out_pdf_deadw, width = 11, height = 8.5)
print(p1)
print(p2)
dev.off()
cat("PDF deadwood :", out_pdf_deadw, "\n")

# =========================================================
# CDOM : QUANTILES MULTIPLES + HISTOGRAMMES
# =========================================================

df_cdom <- df %>%
  filter(!is.na(typologie), typologie != "", is.finite(cdom))

# Quantiles à sortir (ceux discutés) + q90 bonus
tab_cdom_q <- df_cdom %>%
  group_by(typologie) %>%
  summarise(
    n = n(),
    cdom_mean = mean(cdom, na.rm = TRUE),
    cdom_sd   = sd(cdom, na.rm = TRUE),
    q05 = quantile(cdom, 0.05, na.rm = TRUE, names = FALSE),
    q10 = quantile(cdom, 0.10, na.rm = TRUE, names = FALSE),
    q25 = quantile(cdom, 0.25, na.rm = TRUE, names = FALSE),
    q50 = quantile(cdom, 0.50, na.rm = TRUE, names = FALSE),
    q75 = quantile(cdom, 0.75, na.rm = TRUE, names = FALSE),
    q90 = quantile(cdom, 0.90, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) %>%
  arrange(typologie)

out_csv_cdom_q <- file.path(out_dir, "seuils_cdom_q05_q10_q25_q50_q75_q90_par_typologie_mature_simplifiee.csv")
write.csv2(tab_cdom_q, out_csv_cdom_q, row.names = FALSE)
cat("CSV seuils CDOM multi-quantiles :", out_csv_cdom_q, "\n")

# Préparer les seuils en format long pour tracer les lignes
tab_cdom_q_long <- tab_cdom_q %>%
  select(typologie, q05, q10, q25, q50, q75, q90) %>%
  pivot_longer(cols = starts_with("q"),
               names_to = "quantile",
               values_to = "seuil")

# Histogrammes par typologie + lignes des quantiles
p_cdom_hist <- ggplot(df_cdom, aes(x = cdom)) +
  geom_histogram(bins = 25) +
  geom_vline(
    data = tab_cdom_q_long,
    aes(xintercept = seuil, linetype = quantile),
    linewidth = 0.6
  ) +
  facet_wrap(~typologie, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "CDOM par typologie : histogrammes + seuils q05/q10/q25/q50/q75 (+q90)",
    x = "CDOM",
    y = "Nombre d'UE",
    linetype = "Quantile"
  )

out_pdf_cdom <- file.path(out_dir, "hist_cdom_quantiles_typologie_mature_simplifiee.pdf")
pdf(out_pdf_cdom, width = 11, height = 8.5)
print(p_cdom_hist)
dev.off()
cat("PDF histogrammes CDOM :", out_pdf_cdom, "\n")

# -------------------------
# FIN
# -------------------------
cat("\n--- FIN ---\n",
    "Outputs dans : ", out_dir, "\n",
    "- ", basename(out_csv_n), "\n",
    "- ", basename(out_csv_deadw_stats), "\n",
    "- ", basename(out_csv_deadw_comp_mean), "\n",
    "- ", basename(out_csv_deadw_comp_total), "\n",
    "- ", basename(out_pdf_deadw), "\n",
    "- ", basename(out_csv_cdom_q), "\n",
    "- ", basename(out_pdf_cdom), "\n", sep = "")
