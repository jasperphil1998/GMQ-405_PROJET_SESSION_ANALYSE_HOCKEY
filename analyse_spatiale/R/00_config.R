# =============================================================================
# 00_config.R — Configuration commune de la section ANALYSE SPATIALE
# =============================================================================
# Charge les librairies, localise la racine du projet, definit les systemes de
# coordonnees projetes et prepare les donnees de base (joueurs + lieux geocodes).
#
# Ce fichier est source par tous les autres scripts. Il ne produit aucune sortie.
# =============================================================================

# --- 1. Librairies ----------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)       # Import CSV
  library(dplyr)       # Manipulation de donnees
  library(tidyr)       # Restructuration
  library(stringr)     # Manipulation de texte
  library(lubridate)   # Dates
  library(ggplot2)     # Graphiques
  library(sf)          # Donnees spatiales vectorielles
  library(tmap)        # Cartographie thematique
  library(rnaturalearth)
  library(rnaturalearthdata)
})

# Librairies specifiques a l'analyse spatiale (chargees a la demande par
# chaque script pour que 00_config reste leger et tolerant).
# spdep            -> autocorrelation spatiale (Moran, LISA, Getis-Ord)
# spatstat.geom    -> objets ppp / owin (semis de points)
# spatstat.explore -> densite de noyau, fonction L de Ripley, relrisk
# spatialreg       -> modeles spatiaux SAR / SEM


# --- 2. Racine du projet ----------------------------------------------------

# Le script principal du projet suppose que le repertoire de travail est la
# racine du projet (data/... en chemin relatif). On applique la meme regle ici,
# mais on tolere un lancement depuis analyse_spatiale/ en remontant l'arbre.

trouver_racine_projet <- function(max_niveaux = 4) {
  fichier_temoin <- file.path(
    "data", "GMQ-405_Hockey_Players_complet_lieux_modernes.csv"
  )
  chemin <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_niveaux + 1)) {
    if (file.exists(file.path(chemin, fichier_temoin))) return(chemin)
    parent <- dirname(chemin)
    if (identical(parent, chemin)) break   # on a atteint la racine du disque
    chemin <- parent
  }
  stop(
    "Impossible de localiser la racine du projet.\n",
    "Ouvrir le DOSSIER du projet dans VS Code, ou faire setwd() vers\n",
    "GMQ-405_PROJET_SESSION_ANALYSE_HOCKEY avant de lancer le script.",
    call. = FALSE
  )
}

RACINE     <- trouver_racine_projet()
DOSSIER_AS <- file.path(RACINE, "analyse_spatiale")
SORTIES    <- file.path(DOSSIER_AS, "sorties")

dir.create(SORTIES, recursive = TRUE, showWarnings = FALSE)

# Raccourcis de chemins
chemin_donnees <- function(...) file.path(RACINE, "data", ...)
chemin_as      <- function(...) file.path(DOSSIER_AS, ...)
chemin_sortie  <- function(...) file.path(SORTIES, ...)

message("Racine du projet : ", RACINE)


# --- 3. Systemes de coordonnees ---------------------------------------------

# POURQUOI PROJETER ?
# Le script principal travaille entierement en EPSG:4326 (degres). C'est
# acceptable pour afficher des symboles proportionnels, mais invalide des que
# l'on calcule une DISTANCE, une AIRE ou une DENSITE : un degre de longitude
# vaut ~78 km a Toronto et ~52 km a Yellowknife. Toute analyse de semis de
# points, tout noyau de densite et toute matrice de voisinage doivent donc
# etre calcules dans une projection metrique.
#
# Choix retenus :
#  - CRS_NA : Albers equivalente Amerique du Nord. Projection EQUIVALENTE
#             (conserve les aires) -> correcte pour les densites.
#  - CRS_CA : Lambert conforme conique de Statistique Canada (EPSG:3347),
#             la projection officielle pour le Canada.
#  - CRS_EU : LAEA Europe (EPSG:3035), equivalente elle aussi.

CRS_NA <- paste(
  "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96",
  "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
)
CRS_CA <- 3347
CRS_EU <- 3035
CRS_GEO <- 4326


# --- 4. Donnees joueurs -----------------------------------------------------

# Meme preparation que le script principal (sections 3 a 5), pour que les
# resultats soient directement comparables.

charger_hockey <- function() {
  hockey <- read_csv(
    chemin_donnees("GMQ-405_Hockey_Players_complet_lieux_modernes.csv"),
    show_col_types = FALSE
  )

  hockey %>%
    mutate(
      Birthdate      = dmy(Birthdate),
      AnneeNaissance = year(Birthdate),
      Decennie       = floor(AnneeNaissance / 10) * 10,
      Elite1000      = ifelse(Pts >= 1000,
                              "1000 points et plus",
                              "Moins de 1000 points"),
      GroupeGeo = case_when(
        Country == "Canada" ~ "Canada",
        Country == "USA"    ~ "USA",
        Country %in% c(
          "Sweden", "Russia", "Finland", "Czech Republic", "Slovakia",
          "Switzerland", "Germany", "Latvia", "Denmark", "Norway",
          "Austria", "Belarus", "Ukraine", "Poland", "France",
          "England", "Scotland", "Wales", "Northern Ireland",
          "Ireland", "Italy", "Netherlands", "Belgium",
          "Croatia", "Slovenia", "Serbia", "Lithuania",
          "Bulgaria", "Estonia", "United Kingdom"
        ) ~ "Europe",
        TRUE ~ "Autres"
      )
    )
}


# --- 5. Lieux de naissance geocodes -----------------------------------------

# On lit le cache de geocodage deja constitue par le script principal.
# Aucun nouvel appel a un service de geocodage n'est fait ici.

charger_lieux_geocodes <- function() {
  fichier_cache <- chemin_donnees(
    "geocodage", "lieux_naissance_geocodes_lieux_modernes.csv"
  )
  if (!file.exists(fichier_cache)) {
    stop(
      "Cache de geocodage introuvable :\n  ", fichier_cache, "\n",
      "Lancer d'abord la section 3 du script principal.",
      call. = FALSE
    )
  }
  read_csv(fichier_cache, show_col_types = FALSE) %>%
    select(Birthplace, latitude, longitude) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    distinct(Birthplace, .keep_all = TRUE)
}


# --- 6. Villes de naissance en objet spatial --------------------------------

# Agregation par lieu de naissance : nombre de joueurs, production offensive,
# nombre de joueurs elite. C'est l'unite de base de l'analyse de semis.

construire_villes_sf <- function(hockey, lieux_geocodes) {
  villes <- hockey %>%
    group_by(Birthplace, Country) %>%
    summarise(
      NbJoueurs   = n(),
      TotalPts    = sum(Pts, na.rm = TRUE),
      TotalGP     = sum(GP, na.rm = TRUE),
      NbElite     = sum(Pts >= 1000, na.rm = TRUE),
      PtsMoyen    = mean(Pts, na.rm = TRUE),
      DecennieMed = median(Decennie, na.rm = TRUE),
      .groups     = "drop"
    )

  lieux_geocodes %>%
    right_join(villes, by = "Birthplace") %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = CRS_GEO, remove = FALSE) %>%
    # Extraction de la province / etat depuis la chaine "Ville, XX, Pays"
    mutate(
      CodeProv = case_when(
        Country == "Canada" ~ str_match(Birthplace, ",\\s*([A-Z]{2}),\\s*Canada$")[, 2],
        Country == "USA"    ~ str_match(Birthplace, ",\\s*([A-Z]{2}),\\s*USA$")[, 2],
        TRUE ~ NA_character_
      )
    )
}


# --- 7. Utilitaires de sortie -----------------------------------------------

# Sauvegarde d'une carte tmap avec les memes reglages que le script principal
# (300 dpi, format rapport).
sauver_carte <- function(tm, nom, largeur = 10, hauteur = 6) {
  fichier <- chemin_sortie(nom)
  suppressMessages(
    tmap_save(tm = tm, filename = fichier,
              width = largeur, height = hauteur, dpi = 300)
  )
  message("  -> ", basename(fichier))
  invisible(fichier)
}

sauver_graphique <- function(gg, nom, largeur = 10, hauteur = 6) {
  fichier <- chemin_sortie(nom)
  ggsave(fichier, plot = gg, width = largeur, height = hauteur, dpi = 300)
  message("  -> ", basename(fichier))
  invisible(fichier)
}

# Couche d'etiquettes pour les nuages de points. Utilise ggrepel (qui evite
# les chevauchements) s'il est installe, sinon un geom_text decale.
ggrepel_ou_texte <- function(donnees, colonne = "Code", taille = 3) {
  aes_etiq <- aes(label = .data[[colonne]])
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data = donnees, mapping = aes_etiq,
      size = taille, min.segment.length = 0,
      segment.size = 0.25, segment.colour = "grey60",
      max.overlaps = Inf, seed = 2026
    )
  } else {
    geom_text(data = donnees, mapping = aes_etiq,
              size = taille, hjust = -0.2, vjust = -0.4)
  }
}

sauver_tableau <- function(df, nom) {
  fichier <- chemin_sortie(nom)
  # On retire la geometrie avant export CSV le cas echeant
  if (inherits(df, "sf")) df <- st_drop_geometry(df)
  write_csv(df, fichier)
  message("  -> ", basename(fichier))
  invisible(fichier)
}

# Ecriture d'un bloc de resultats texte (sorties de tests statistiques).
# Les tests sont l'element central du rapport : on les archive en .txt pour
# pouvoir les citer sans avoir a relancer le script.
journal_resultats <- function(nom) {
  fichier <- chemin_sortie(nom)
  con <- file(fichier, open = "wt", encoding = "UTF-8")
  list(
    ecrire = function(...) {
      lignes <- paste0(...)
      writeLines(lignes, con)
      cat(lignes, sep = "\n")
      cat("\n")
    },
    capturer = function(objet, titre = NULL) {
      if (!is.null(titre)) {
        writeLines(c("", paste0("--- ", titre, " ---"), ""), con)
        cat("\n---", titre, "---\n\n")
      }
      txt <- capture.output(print(objet))
      writeLines(txt, con)
      cat(txt, sep = "\n")
      cat("\n")
    },
    fermer = function() {
      close(con)
      message("  -> ", basename(fichier))
    }
  )
}

tmap_mode("plot")

message("Configuration chargee (00_config.R).")
