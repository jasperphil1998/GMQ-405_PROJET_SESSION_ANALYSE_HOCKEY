# =============================================================================
# install_packages.R — Installation des dépendances du projet
# =============================================================================
# À exécuter une seule fois (ou après une réinstallation de R).
# Dans VS Code / RStudio : ouvrir ce fichier et faire "Source".
# En terminal :  Rscript install_packages.R
# =============================================================================

# --- Socle commun : requis par run_all.R ------------------------------------
packages_requis <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "ggplot2",
  "scales",
  "sf",                 # données spatiales vectorielles
  "tmap",               # cartographie thématique (version 4 requise)
  "rnaturalearth",
  "rnaturalearthdata",
  "spatstat.geom",      # objets ppp / owin (module 08)
  "spatstat.explore",   # densité de noyau (module 08)
  "languageserver"      # requis par l'extension R de VS Code
)

# --- Optionnels : les modules concernés sont ignorés s'ils manquent ---------
packages_optionnels <- c(
  "tidygeocoder",   # géocodage de nouveaux lieux         (module 01)
  "ClustGeo",       # classification spatiale             (module 10)
  "sparr",          # densité spatio-temporelle (STKDE)   (module 09)
  "terra",          # rasters                             (module 09)
  "gifski",         # animation GIF                       (module 09)
  "classInt",       # discrétisation                      (module 09)
  "viridis"         # palettes                            (module 09)
)

installer <- function(liste, etiquette) {
  manquants <- liste[!(liste %in% rownames(installed.packages()))]
  if (length(manquants) > 0) {
    message("Installation (", etiquette, ") : ",
            paste(manquants, collapse = ", "))
    install.packages(manquants, repos = "https://cloud.r-project.org")
  } else {
    message("Tous les packages ", etiquette, " sont déjà installés.")
  }
}

installer(packages_requis, "requis")
installer(packages_optionnels, "optionnels")

# --- rnaturalearthhires : PAS sur le CRAN -----------------------------------
# Fournit les fonds de carte détaillés des provinces et des états, utilisés
# par les modules 04 et 06.
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  message("Installation de rnaturalearthhires depuis r-universe...")
  install.packages(
    "rnaturalearthhires",
    repos = "https://ropensci.r-universe.dev"
  )
} else {
  message("rnaturalearthhires est déjà installé.")
}

# --- Vérification de la version de tmap -------------------------------------
# Le projet utilise la syntaxe tmap 4 (tm_scale_*, fill / col séparés).
# Avec tmap 3, toutes les cartes échoueraient.
if (requireNamespace("tmap", quietly = TRUE)) {
  version_tmap <- packageVersion("tmap")
  if (version_tmap < "4.0.0") {
    warning(
      "tmap ", version_tmap, " est installé, mais le projet exige tmap 4.\n",
      "Mettre à jour avec : install.packages(\"tmap\")",
      call. = FALSE
    )
  } else {
    message("tmap ", version_tmap, " : version compatible.")
  }
}

message("\nInstallation terminée. Lancer ensuite :  Rscript run_all.R")
