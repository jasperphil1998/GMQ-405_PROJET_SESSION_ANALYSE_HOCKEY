# ============================================================
# Installation des packages requis pour le projet Hockey
# À exécuter une seule fois (ou après une réinstallation de R).
# Dans VS Code : ouvrir ce fichier et faire "Source" (ou le
# lancer avec  Rscript install_packages.R  dans un terminal).
# ============================================================

# Packages disponibles sur le CRAN
packages_cran <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "ggplot2",
  "sf",
  "tmap",
  "rnaturalearth",
  "rnaturalearthdata",
  "tidygeocoder",
  "scales",
  "languageserver"   # requis par l'extension R de VS Code
)

manquants <- packages_cran[!(packages_cran %in% rownames(installed.packages()))]

if (length(manquants) > 0) {
  message("Installation depuis le CRAN : ", paste(manquants, collapse = ", "))
  install.packages(manquants, repos = "https://cloud.r-project.org")
} else {
  message("Tous les packages CRAN sont déjà installés.")
}

# rnaturalearthhires n'est PAS sur le CRAN : il vient du dépôt r-universe
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  message("Installation de rnaturalearthhires depuis r-universe...")
  install.packages(
    "rnaturalearthhires",
    repos = "https://ropensci.r-universe.dev"
  )
} else {
  message("rnaturalearthhires est déjà installé.")
}

message("\nInstallation terminée.")
