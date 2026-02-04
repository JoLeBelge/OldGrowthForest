library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# --- Chemins ---
db_path  <- "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db"
data_dir <- dirname(db_path)
out_dir  <- file.path(data_dir, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(RSQLite::SQLite(), db_path)

# Recharge la table (avec typologie déjà présente dans la DB)
dendro_plot <- dbReadTable(con, "dendro_plot")
dbDisconnect(con)

# --- Variables à analyser ---
vars <- c(
  "number_of_trees",
  "basal_area_alive",
  "basal_area_dead",
  "vol_alive",
  "vol_deadw",
  "cdom",
  "gha_relascope"
)

# =========================
# 1) PDF : histogrammes tous les plots + encart stats
# =========================
df_long <- dendro_plot %>%
  select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valeur") %>%
  filter(!is.na(valeur))

stats_var <- df_long %>%
  group_by(variable) %>%
  summarise(
    moyenne = mean(valeur),
    mediane = median(valeur),
    ecart_type = sd(valeur),
    .groups = "drop"
  ) %>%
  mutate(label = paste0("moy = ", round(moyenne, 2),
                        "\nmed = ", round(mediane, 2),
                        "\nsd = ", round(ecart_type, 2)))

p_global <- ggplot(df_long, aes(x = valeur)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free") +
  geom_text(
    data = stats_var,
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.1, vjust = 1.1,
    inherit.aes = FALSE,
    size = 3
  ) +
  theme_minimal() +
  labs(title = "Histogrammes (tous les plots)", x = NULL, y = "Nombre de plots")

out_pdf_global <- file.path(out_dir, "histogrammes_tous_plots.pdf")
ggsave(out_pdf_global, plot = p_global, width = 11, height = 8.5, units = "in")


# =========================
# 2) PDF multi-pages : histogrammes par typologie (1 page par variable)
# =========================
out_pdf_typo <- file.path(out_dir, "histos_par_typologie.pdf")
pdf(out_pdf_typo, width = 11, height = 8.5)

for (v in vars) {
  
  df_v <- dendro_plot %>%
    select(typologie, valeur = all_of(v)) %>%
    filter(!is.na(typologie), !is.na(valeur))
  
  stats_typo <- df_v %>%
    group_by(typologie) %>%
    summarise(
      moyenne = mean(valeur),
      mediane = median(valeur),
      ecart_type = sd(valeur),
      .groups = "drop"
    ) %>%
    mutate(label = paste0("moy = ", round(moyenne, 2),
                          "\nmed = ", round(mediane, 2),
                          "\nsd = ", round(ecart_type, 2)))
  
  p <- ggplot(df_v, aes(x = valeur)) +
    geom_histogram(bins = 30) +
    facet_wrap(~typologie, scales = "free") +
    geom_text(
      data = stats_typo,
      aes(x = Inf, y = Inf, label = label),
      hjust = 1.1, vjust = 1.1,
      inherit.aes = FALSE,
      size = 3
    ) +
    theme_minimal() +
    labs(
      title = paste("Histogramme de", v, "par typologie"),
      x = v, y = "Nombre de plots"
    )
  
  print(p)  # => 1 page par variable dans le PDF
}

dev.off()

cat("\nPDF global :", out_pdf_global, "\n")
cat("PDF par typologie :", out_pdf_typo, "\n")
cat("Dossier output :", out_dir, "\n")





# =========================
# 3) PDF multi-pages : boxplots par typologie (1 page par variable)
# =========================
out_pdf_box <- file.path(out_dir, "boxplots_par_typologie.pdf")
pdf(out_pdf_box, width = 11, height = 8.5)

for (v in vars) {
  
  df_v <- dendro_plot %>%
    select(typologie, valeur = all_of(v)) %>%
    filter(!is.na(typologie), !is.na(valeur))
  
  # stats optionnelles (n, moyenne, médiane, sd) pour affichage si tu veux
  stats_typo <- df_v %>%
    group_by(typologie) %>%
    summarise(
      n = n(),
      moyenne = mean(valeur),
      mediane = median(valeur),
      ecart_type = sd(valeur),
      .groups = "drop"
    )
  
  p <- ggplot(df_v, aes(x = typologie, y = valeur)) +
    geom_boxplot(outlier.alpha = 0.3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste("Boxplot de", v, "par typologie"),
      x = "Typologie", y = v
    )
  
  print(p)
}

dev.off()
cat("\nPDF boxplots :", out_pdf_box, "\n")





#deadwoood 

#moyenne globale sur le jeu de données
# standing      FAS      LIS    total pct_standing  pct_FAS  pct_LIS
#  1516.789 2677.632 2023.504 6217.926   24.39381 43.06311 32.54307


library(dplyr)

df <- dendro_plot %>%
  mutate(deadw_total = vol_dead_standing + vol_wood_debris_FAS + vol_wood_debris_LIS)

deadw_global_sum <- df %>%
  summarise(
    standing = sum(vol_dead_standing, na.rm = TRUE),
    FAS      = sum(vol_wood_debris_FAS, na.rm = TRUE),
    LIS      = sum(vol_wood_debris_LIS, na.rm = TRUE)
  ) %>%
  mutate(
    total = standing + FAS + LIS,
    pct_standing = 100 * standing / total,
    pct_FAS      = 100 * FAS / total,
    pct_LIS      = 100 * LIS / total
  )

deadw_global_sum


#moyenne par typologie 

deadw_bytypologie_sum <- df %>%
  group_by(typologie) %>%
  summarise(
    standing = sum(vol_dead_standing, na.rm = TRUE),
    FAS      = sum(vol_wood_debris_FAS, na.rm = TRUE),
    LIS      = sum(vol_wood_debris_LIS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total = standing + FAS + LIS,
    pct_standing = ifelse(total > 0, 100 * standing / total, NA_real_),
    pct_FAS      = ifelse(total > 0, 100 * FAS / total, NA_real_),
    pct_LIS      = ifelse(total > 0, 100 * LIS / total, NA_real_)
  ) %>%
  arrange(desc(total))

deadw_bytypologie_sum


# fermer tout device PDF resté ouvert
while (dev.cur() > 1) dev.off()


# -------------------------
# 1) p1 : barres empilées 100% (composition par typologie, sur sommes)
# -------------------------
plot_deadw_sum <- deadw_bytypologie_sum %>%
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
  labs(title = "Deadwood par typologie : répartition (%)",
       x = "Typologie", y = "% du deadwood (sur volumes totaux)", fill = "") +
  theme(axis.text.y = element_text(size = 9))

# -------------------------
# 2) p2 : boxplots (dispersion des % par UE)
# -------------------------
plot_deadw_ue <- df %>%
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
  filter(!is.na(typologie), is.finite(pct))

p2 <- ggplot(plot_deadw_ue, aes(x = typologie, y = pct)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  facet_wrap(~type, ncol = 1) +
  theme_minimal() +
  labs(title = "Dispersion des parts de deadwood par UE (boxplots)",
       x = "Typologie", y = "% par UE")

# -------------------------
# 3) Export PDF unique
# -------------------------
out_pdf_deadw <- file.path(out_dir, "graph_deadwood.pdf")
pdf(out_pdf_deadw, width = 11, height = 8.5)
print(p1)
print(p2)
dev.off()

cat("PDF deadwood créé :", out_pdf_deadw, "\n")
