# =============================================================================
# run_all.R — Lance toute la section ANALYSE SPATIALE
# =============================================================================
# UTILISATION
#   Ouvrir le DOSSIER du projet dans VS Code, puis dans la console R :
#       source("analyse_spatiale/run_all.R")
#   Ou en ligne de commande, depuis la racine du projet :
#       Rscript analyse_spatiale/run_all.R
#
# Les modules sont independants a une exception pres : 02 et 05 reutilisent le
# fichier unites_normalisees.rds produit par 01. Il faut donc lancer 01 avant
# ces deux-la (run_all.R respecte cet ordre).
#
# Toutes les sorties vont dans analyse_spatiale/sorties/.
# Rien n'est ecrit dans figures/ : le script principal du projet n'est pas
# touche, et ses sorties ne risquent pas d'etre ecrasees.
# =============================================================================

# Verification des dependances avant de lancer quoi que ce soit : mieux vaut un
# message clair tout de suite qu'une erreur au milieu du module 03.
paquets_requis <- c(
  "readr", "dplyr", "tidyr", "stringr", "lubridate", "ggplot2",
  "sf", "tmap", "rnaturalearth", "rnaturalearthdata",
  "spdep", "spatialreg", "spatstat.geom", "spatstat.explore"
)

paquets_optionnels <- c(
  "ggrepel",   # etiquettes non chevauchantes (sinon repli sur geom_text)
  "MASS",      # modele binomial negatif (controle de robustesse, module 05)
  "spgwr"      # regression geographiquement ponderee (module 05)
)

manquants <- paquets_requis[
  !vapply(paquets_requis, requireNamespace, logical(1), quietly = TRUE)
]

if (length(manquants) > 0) {
  stop(
    "Paquets requis manquants : ", paste(manquants, collapse = ", "), "\n",
    "Les installer avec :\n",
    "  install.packages(c(\"",
    paste(manquants, collapse = "\", \""), "\"))",
    call. = FALSE
  )
}

manquants_opt <- paquets_optionnels[
  !vapply(paquets_optionnels, requireNamespace, logical(1), quietly = TRUE)
]

if (length(manquants_opt) > 0) {
  message(
    "Paquets optionnels absents : ", paste(manquants_opt, collapse = ", "),
    "\nLes sections correspondantes seront ignorees. Pour les activer :\n",
    "  install.packages(c(\"",
    paste(manquants_opt, collapse = "\", \""), "\"))"
  )
}

debut <- Sys.time()

message("\n#############################################################")
message("#  GMQ-405 — SECTION ANALYSE SPATIALE                       #")
message("#############################################################")

source(file.path("analyse_spatiale", "R", "00_config.R"))

modules <- c(
  "01_normalisation.R",
  "02_autocorrelation.R",
  "03_semis_points.R",
  "04_centrographie.R",
  "05_modelisation.R"
)

for (module in modules) {
  chemin <- file.path("analyse_spatiale", "R", module)
  message("\n>>> ", module)
  # Chaque module est lance dans son propre environnement : cela evite qu'une
  # variable laissee par un module en masque une autre dans le suivant.
  resultat <- try(sys.source(chemin, envir = new.env(parent = globalenv())),
                  silent = FALSE)
  if (inherits(resultat, "try-error")) {
    message("!!! ECHEC de ", module, " — les modules suivants continuent.")
  }
}

duree <- round(as.numeric(difftime(Sys.time(), debut, units = "mins")), 1)

message("\n#############################################################")
message("#  TERMINE en ", duree, " minutes")
message("#  Sorties : analyse_spatiale/sorties/")
message("#############################################################")

fichiers <- list.files(file.path("analyse_spatiale", "sorties"))
message("\n", length(fichiers), " fichiers produits :")
message(paste(" -", sort(fichiers), collapse = "\n"))
