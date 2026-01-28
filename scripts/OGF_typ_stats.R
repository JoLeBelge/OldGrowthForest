# =========================
# DENDRO : synthèse + histogrammes (CDOM, GHA)
# Tables : dendro_stand + dendro_plot
# Sortie : PDF + stats console
# =========================

library(DBI)
library(dplyr)
library(dbplyr)
library(ggplot2)

con <- dbConnect(RSQLite::SQLite(), "C:/Users/Lemans Léa/Documents/GitHub/OldGrowthForest/data/OGF_all.db")


# ---- 2) Lire les tables ----
stand <- tbl(con, "dendro_stand") %>%
  select(ues_id_ogf, cdom_mean) %>%
  collect()

plot <- tbl(con, "dendro_plot") %>%
  select(ues_id_ogf, ues_id_ue, cdom, gha_relascope) %>%
  collect()

# ---- 3) Fonction stats rapide ----
quick_stats <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  c(
    n      = length(x),
    mean   = mean(x),
    median = median(x),
    sd     = sd(x),
    p10    = unname(quantile(x, 0.10)),
    p90    = unname(quantile(x, 0.90)),
    min    = min(x),
    max    = max(x)
  )
}

cat("\n--- Couverture ---\n")
cat("stand : lignes =", nrow(stand), " | parcelles =", dplyr::n_distinct(stand$ues_id_ogf), "\n")
cat("plot  : lignes =", nrow(plot),  " | parcelles =", dplyr::n_distinct(plot$ues_id_ogf), "\n")
cat("GHA non-NA =", sum(!is.na(plot$gha_relascope)), "\n")

cat("\n--- Stats CDOM (UE) : dendro_plot$cdom ---\n")
print(round(quick_stats(plot$cdom), 2))

cat("\n--- Stats GHA (UE) : dendro_plot$gha_relascope ---\n")
print(round(quick_stats(plot$gha_relascope), 2))

cat("\n--- Stats CDOM (parcelle) : dendro_stand$cdom_mean ---\n")
print(round(quick_stats(stand$cdom_mean), 2))

# ---- 4) Histogrammes ----
out_pdf <- "C:/Old_Growth_Forest/hist_dendro_cdom_gha.pdf"
pdf(out_pdf, width = 11, height = 8.5)

# CDOM (niveau UE)
ggplot(plot %>% filter(!is.na(cdom)), aes(x = cdom)) +
  geom_histogram(bins = 20) +
  theme_minimal(base_size = 12) +
  labs(title = "Histogramme CDOM (niveau UE - dendro_plot)",
       x = "CDOM", y = "Nombre d'UE") %>%
  print()

# GHA relascope (niveau UE)
ggplot(plot %>% filter(!is.na(gha_relascope)), aes(x = gha_relascope)) +
  geom_histogram(bins = 20) +
  theme_minimal(base_size = 12) +
  labs(title = "Histogramme GHA relascope (niveau UE - dendro_plot)",
       x = "GHA relascope", y = "Nombre d'UE") %>%
  print()

# (OPTIONNEL) CDOM moyen (niveau parcelle)
ggplot(stand %>% filter(!is.na(cdom_mean)), aes(x = cdom_mean)) +
  geom_histogram(bins = 15) +
  theme_minimal(base_size = 12) +
  labs(title = "Histogramme CDOM moyen (niveau parcelle - dendro_stand)",
       x = "CDOM mean", y = "Nombre de parcelles") %>%
  print()

dev.off()
cat("\nPDF créé :", out_pdf, "\n")

# ---- 5) Fermer DB ----
dbDisconnect(con)
