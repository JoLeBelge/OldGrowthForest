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
