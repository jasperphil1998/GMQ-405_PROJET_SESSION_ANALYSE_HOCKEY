# =============================================================================
# run_all.R — Lance toute la chaîne d'analyse du projet
# =============================================================================
# UTILISATION
#   Ouvrir le DOSSIER du projet dans VS Code ou RStudio, puis dans la console R :
#       source("run_all.R")
#   Ou en ligne de commande, depuis la racine du projet :
#       Rscript run_all.R
#
#   Pour ne relancer qu'une partie :
#       source("run_all.R"); lancer_modules(c("07", "08"))
#
# ORDRE ET DÉPENDANCES
#   00  configuration        : sourcée une fois, obligatoire
#   01  géocodage            : avant 04, 07, 08 et 09
#   02-05 descriptif         : indépendants
#   06  normalisation        : avant 07 et 10 (produit unites_normalisees.rds)
#   07-10 analyse spatiale
#
# SORTIES
#   sorties/  tout : cartes, graphiques, tableaux et journaux de résultats
#
# BILAN DE FIN D'EXÉCUTION
#   ok      le module a tourné et produit ses sorties
#   IGNORÉ  le module s'est désactivé lui-même, faute d'un paquet optionnel.
#           IL N'A PRODUIT AUCUNE SORTIE. C'est le cas du module 09 tant que
#           sparr, gifski et viridis ne sont pas installés.
#   ÉCHEC   le module a planté ; les suivants ont continué
# =============================================================================


# --- Catalogue des modules --------------------------------------------------

MODULES <- list(
  list(id = "01", fichier = "01_geocodage.R",         nom = "Géocodage"),
  list(id = "02", fichier = "02_graphiques.R",        nom = "Graphiques descriptifs"),
  list(id = "03", fichier = "03_cartes_pays.R",       nom = "Cartes par pays"),
  list(id = "04", fichier = "04_cartes_villes.R",     nom = "Cartes par ville"),
  list(id = "05", fichier = "05_tableaux.R",          nom = "Tableaux du rapport"),
  list(id = "06", fichier = "06_normalisation.R",     nom = "Normalisation (taux, QL)"),
  list(id = "07", fichier = "07_grille_hexagonale.R", nom = "Grille hexagonale"),
  list(id = "08", fichier = "08_semis_points.R",      nom = "Semis de points"),
  list(id = "09", fichier = "09_stkde.R",             nom = "STKDE (Xavier L.)"),
  list(id = "10", fichier = "10_clustgeo.R",          nom = "ClustGeo (Xavier L.)")
)


# --- Vérification des dépendances -------------------------------------------
# Mieux vaut un message clair tout de suite qu'une erreur au milieu du
# module 08.

paquets_requis <- c(
  # socle commun
  "readr", "dplyr", "tidyr", "stringr", "lubridate", "ggplot2", "scales",
  "sf", "tmap", "rnaturalearth", "rnaturalearthdata",
  # semis de points (module 08)
  "spatstat.geom", "spatstat.explore"
)

paquets_optionnels <- c(
  "rnaturalearthhires", # fonds de carte provinces / états (modules 04, 06)
  "tidygeocoder",       # géocodage de nouveaux lieux (module 01)
  "ClustGeo",           # classification spatiale (module 10)
  "sparr", "terra", "gifski", "classInt", "viridis"   # STKDE (module 09)
)

manquants <- paquets_requis[
  !vapply(paquets_requis, requireNamespace, logical(1), quietly = TRUE)
]

if (length(manquants) > 0) {
  stop(
    "Paquets requis manquants : ", paste(manquants, collapse = ", "), "\n",
    "Les installer avec :\n",
    "  source(\"install_packages.R\")\n",
    "ou :\n",
    "  install.packages(c(\"", paste(manquants, collapse = "\", \""), "\"))",
    call. = FALSE
  )
}

manquants_opt <- paquets_optionnels[
  !vapply(paquets_optionnels, requireNamespace, logical(1), quietly = TRUE)
]

if (length(manquants_opt) > 0) {
  message(
    "Paquets optionnels absents : ", paste(manquants_opt, collapse = ", "),
    "\nLes sections correspondantes seront ignorées. Pour les activer :\n",
    "  source(\"install_packages.R\")"
  )
}


# --- Chargement de la configuration -----------------------------------------

source(file.path("R", "00_config.R"))


# --- Moteur d'exécution -----------------------------------------------------

lancer_modules <- function(ids = NULL) {

  a_lancer <- if (is.null(ids)) {
    MODULES
  } else {
    Filter(function(m) m$id %in% as.character(ids), MODULES)
  }

  if (length(a_lancer) == 0) {
    stop("Aucun module ne correspond à : ", paste(ids, collapse = ", "),
         call. = FALSE)
  }

  debut <- Sys.time()

  message("\n#############################################################")
  message("#  GMQ-405 — ANALYSE SPATIALE DU HOCKEY                     #")
  message("#############################################################")

  bilan <- lapply(a_lancer, function(module) {
    chemin <- file.path("R", module$fichier)
    message("\n>>> ", module$id, " — ", module$nom)

    t0  <- Sys.time()
    env <- new.env(parent = globalenv())
    # Chaque module tourne dans son propre environnement : cela évite qu'une
    # variable laissée par un module en masque une autre dans le suivant.
    resultat <- try(sys.source(chemin, envir = env), silent = FALSE)
    duree <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))

    # Un module qui dépend de paquets optionnels absents se désactive lui-même et
    # pose MODULE_IGNORE dans son environnement. Sans cette distinction, il
    # apparaîtrait "ok" alors qu'il n'a rien produit : c'est trompeur, et on
    # ne s'en aperçoit qu'en cherchant une sortie qui n'existe pas.
    etat <- if (inherits(resultat, "try-error")) {
      message("!!! ÉCHEC de ", module$fichier,
              " — les modules suivants continuent.")
      "ÉCHEC"
    } else if (isTRUE(mget("MODULE_IGNORE", envir = env,
                           ifnotfound = list(FALSE))[[1]])) {
      "IGNORÉ"
    } else {
      "ok"
    }

    data.frame(Module = module$id, Nom = module$nom,
               Etat = etat, Secondes = duree)
  })

  bilan <- do.call(rbind, bilan)
  duree_totale <- round(as.numeric(difftime(Sys.time(), debut, units = "mins")), 1)

  message("\n#############################################################")
  message("#  TERMINÉ en ", duree_totale, " minutes")
  message("#############################################################\n")
  print(bilan, row.names = FALSE)

  echecs <- bilan[bilan$Etat == "ÉCHEC", ]
  if (nrow(echecs) > 0) {
    message("\n", nrow(echecs), " module(s) en ÉCHEC : ",
            paste(echecs$Module, collapse = ", "))
  }

  ignores <- bilan[bilan$Etat == "IGNORÉ", ]
  if (nrow(ignores) > 0) {
    message("\n", nrow(ignores), " module(s) IGNORÉ(S) faute de paquets : ",
            paste(ignores$Module, collapse = ", "),
            "\nCes modules n'ont produit AUCUNE sortie. Voir le message",
            " ci-dessus pour la commande d'installation.")
  }

  message("\nToutes les sorties : sorties/")

  invisible(bilan)
}


# Lancement complet si le fichier est source directement.
if (!exists("RUN_ALL_NE_PAS_LANCER") || !RUN_ALL_NE_PAS_LANCER) {
  lancer_modules()
}
