# =============================================================================
# 00_config.R — Configuration commune a tous les modules
# =============================================================================
# Ce fichier est source par run_all.R, puis par chaque module. Il ne produit
# aucune sortie : il ne fait que charger les librairies, localiser la racine du
# projet, definir les projections, preparer les donnees et fournir les
# utilitaires de sauvegarde.
#
# CONVENTION DE SYNTAXE
# Tout le projet utilise le pipe natif de R (|>) plutot que celui de magrittr
# (%>%). C'est le choix fait par l'equipe (commit "Remplacer %>% par |>") et il
# evite une dependance a magrittr. Le pipe natif exige que la fonction de
# droite soit un APPEL : ecrire  x |> st_make_valid()  et non  x |> st_make_valid
#
# Les methodes et les packages suivent le manuel du cours :
#   Apparicio, P. et Gelb, J. (2026). Methodes d'analyse spatiale :
#   un grand bol d'R. Voir docs/REFERENCES.md.
# =============================================================================


# --- 1. Librairies ----------------------------------------------------------
# Les librairies de base sont chargees ici. Les librairies specialisees
# (spdep, spatstat, spatialreg, ClustGeo...) sont chargees par le module qui en
# a besoin, pour que 00_config reste leger et tolerant a une installation
# incomplete.

suppressPackageStartupMessages({
  library(readr)         # Import CSV
  library(dplyr)         # Manipulation de donnees
  library(tidyr)         # Restructuration
  library(stringr)       # Manipulation de texte
  library(lubridate)     # Dates
  library(ggplot2)       # Graphiques
  library(scales)        # Formatage des axes
  library(sf)            # Donnees spatiales vectorielles (chap. 1 du manuel)
  library(tmap)          # Cartographie thematique  (chap. 1.5 du manuel)
  library(rnaturalearth)
  library(rnaturalearthdata)
})


# --- 2. Racine du projet ----------------------------------------------------
# Aucun setwd() code en dur : les chemins sont relatifs a la racine du projet.
# Dans VS Code ou RStudio, ouvrir le DOSSIER du projet (et non un seul fichier)
# suffit. La fonction ci-dessous tolere aussi un lancement depuis un
# sous-dossier en remontant l'arborescence.

trouver_racine_projet <- function(max_niveaux = 4) {
  fichier_temoin <- file.path(
    "data", "GMQ-405_Hockey_Players_complet_lieux_modernes.csv"
  )
  chemin <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_niveaux + 1)) {
    if (file.exists(file.path(chemin, fichier_temoin))) return(chemin)
    parent <- dirname(chemin)
    if (identical(parent, chemin)) break   # racine du disque atteinte
    chemin <- parent
  }
  stop(
    "Impossible de localiser la racine du projet.\n",
    "Ouvrir le DOSSIER du projet dans VS Code / RStudio, ou faire setwd()\n",
    "vers GMQ-405_PROJET_SESSION_ANALYSE_HOCKEY avant de lancer le script.",
    call. = FALSE
  )
}

RACINE   <- trouver_racine_projet()
FIGURES  <- file.path(RACINE, "figures")   # sorties descriptives (non versionnees)
SORTIES  <- file.path(RACINE, "sorties")   # sorties statistiques (versionnees)

dir.create(FIGURES, recursive = TRUE, showWarnings = FALSE)
dir.create(SORTIES, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RACINE, "data", "geocodage"),
           recursive = TRUE, showWarnings = FALSE)

# Raccourcis de chemins
chemin_donnees <- function(...) file.path(RACINE, "data", ...)
chemin_figure  <- function(...) file.path(FIGURES, ...)
chemin_sortie  <- function(...) file.path(SORTIES, ...)
chemin_module  <- function(...) file.path(RACINE, "R", ...)

message("Racine du projet : ", RACINE)


# --- 3. Systemes de coordonnees ---------------------------------------------
#
# POURQUOI PROJETER ?
# Les cartes descriptives (modules 02 a 05) travaillent en EPSG:4326, en
# degres. C'est acceptable pour afficher des symboles proportionnels, mais
# invalide des que l'on calcule une DISTANCE, une AIRE ou une DENSITE : un
# degre de longitude vaut environ 78 km a Toronto et 52 km a Yellowknife.
# Toute analyse de semis de points, tout noyau de densite et toute matrice de
# voisinage doivent donc etre calcules dans une projection metrique
# (manuel, section 1.2.1).
#
#  - CRS_NA : Albers equivalente Amerique du Nord. Projection EQUIVALENTE
#             (conserve les aires) -> correcte pour les densites.
#  - CRS_CA : Lambert conforme conique de Statistique Canada (EPSG:3347),
#             la projection officielle pour le Canada.
#  - CRS_EU : LAEA Europe (EPSG:3035), equivalente elle aussi.
#  - CRS_ATL : LAEA centree sur l'Atlantique Nord : place le Canada et
#             l'Europe dans le meme champ.

CRS_GEO <- 4326
CRS_CA  <- 3347
CRS_EU  <- 3035
CRS_NA  <- paste(
  "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96",
  "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
)
CRS_ATL <- "+proj=laea +lat_0=55 +lon_0=-45 +datum=WGS84 +units=m +no_defs"

# Projection cylindrique equivalente mondiale, utilisee par le module 11
# (STKDE) : spatstat exige une projection metrique couvrant tout le globe.
CRS_MONDE <- paste(
  "+proj=cea +lon_0=0 +lat_ts=30 +datum=WGS84 +units=m +no_defs"
)


# --- 4. Habillage commun ----------------------------------------------------
# La mention de source et d'auteurs etait recopiee dans une quarantaine de
# figures du script d'origine. Une seule constante evite les divergences.

AUTEURS <- "Philippe Filion, Xavier Lafrance, Xavier St-Arnaud"
SOURCE  <- "Hockey DB / NHL player data"
CREDITS <- paste0("Source : ", SOURCE, "\nAuteur : ", AUTEURS)

# Palette des groupes geographiques, commune a tous les graphiques.
COULEURS_GEO <- c(
  "Autres" = "#F8766D",
  "Canada" = "#7CAE00",
  "Europe" = "#00BFC4",
  "USA"    = "#C77CFF"
)

# Seuil de "joueur d'elite" utilise par les modules descriptifs.
SEUIL_ELITE_PTS <- 1000

tmap_mode("plot")


# --- 5. Cache de session ----------------------------------------------------
# run_all.R execute chaque module dans son propre environnement. Sans cache,
# le fichier de joueurs serait relu et repare une dizaine de fois. Le cache vit
# dans l'environnement global, donc il survit d'un module a l'autre.

.cache_projet <- new.env(parent = emptyenv())

en_cache <- function(cle, expression) {
  if (!exists(cle, envir = .cache_projet, inherits = FALSE)) {
    assign(cle, expression, envir = .cache_projet)
  }
  get(cle, envir = .cache_projet, inherits = FALSE)
}


# --- 6. Donnees joueurs -----------------------------------------------------
# Reprend a l'identique la preparation des sections 3 a 5 du script d'origine
# (archive/Projet_Hockey_script_ORIGINAL.R), pour que tous les resultats
# restent comparables a ceux deja produits.

# Pays regroupes sous l'etiquette "Europe".
PAYS_EUROPE <- c(
  "Sweden", "Russia", "Finland", "Czech Republic", "Slovakia",
  "Switzerland", "Germany", "Latvia", "Denmark", "Norway",
  "Austria", "Belarus", "Ukraine", "Poland", "France",
  "England", "Scotland", "Wales", "Northern Ireland",
  "Ireland", "Italy", "Netherlands", "Belgium",
  "Croatia", "Slovenia", "Serbia", "Lithuania",
  "Bulgaria", "Estonia", "United Kingdom"
)

charger_hockey <- function() {
  en_cache("hockey", {
    read_csv(
      chemin_donnees("GMQ-405_Hockey_Players_complet_lieux_modernes.csv"),
      show_col_types = FALSE
    ) |>
      mutate(
        Birthdate      = dmy(Birthdate),
        AnneeNaissance = year(Birthdate),
        Decennie       = floor(AnneeNaissance / 10) * 10,
        Elite1000      = ifelse(Pts >= SEUIL_ELITE_PTS,
                                "1000 points et plus",
                                "Moins de 1000 points"),
        GroupeGeo = case_when(
          Country == "Canada"       ~ "Canada",
          Country == "USA"          ~ "USA",
          Country %in% PAYS_EUROPE  ~ "Europe",
          TRUE                      ~ "Autres"
        ),
        # Position nettoyee et traduite, utilisee par les graphiques 6 et 11.
        Position = str_trim(.data[["Pos."]]),
        PositionLabel = recode(
          Position,
          "C"  = "Centre",
          "D"  = "Defenseur",
          "G"  = "Gardien",
          "LW" = "Ailier gauche",
          "RW" = "Ailier droit",
          .default = Position
        ),
        PIM = as.numeric(PIM),
        Pts = as.numeric(Pts)
      )
  })
}


# --- 7. Lieux de naissance geocodes -----------------------------------------
# Le cache de geocodage est constitue par le module 01. On se contente ici de
# le relire : aucun appel a un service de geocodage n'est fait.

FICHIER_CACHE_GEO <- function() {
  chemin_donnees("geocodage", "lieux_naissance_geocodes_lieux_modernes.csv")
}

charger_lieux_geocodes <- function() {
  en_cache("lieux_geocodes", {
    fichier <- FICHIER_CACHE_GEO()
    if (!file.exists(fichier)) {
      stop(
        "Cache de geocodage introuvable :\n  ", fichier, "\n",
        "Lancer d'abord R/01_geocodage.R.",
        call. = FALSE
      )
    }
    read_csv(fichier, show_col_types = FALSE) |>
      select(Birthplace, latitude, longitude) |>
      filter(!is.na(latitude), !is.na(longitude)) |>
      distinct(Birthplace, .keep_all = TRUE)
  })
}


# --- 8. Villes de naissance en objet spatial --------------------------------
# Agregation par lieu de naissance : nombre de joueurs, production offensive,
# nombre de joueurs elite. C'est l'unite de base des cartes par ville
# (module 04) et de l'analyse de semis (module 08).
#
# Le script d'origine recalculait ce meme tableau SIX fois, une fois par carte
# regionale. Il est desormais calcule une seule fois et mis en cache.

construire_villes_sf <- function() {
  en_cache("villes_sf", {
    hockey         <- charger_hockey()
    lieux_geocodes <- charger_lieux_geocodes()

    villes <- hockey |>
      group_by(Birthplace, Country) |>
      summarise(
        NbJoueurs   = n(),
        TotalPts    = sum(Pts, na.rm = TRUE),
        TotalGP     = sum(GP, na.rm = TRUE),
        NbElite     = sum(Pts >= SEUIL_ELITE_PTS, na.rm = TRUE),
        PtsMoyen    = mean(Pts, na.rm = TRUE),
        DecennieMed = median(Decennie, na.rm = TRUE),
        .groups     = "drop"
      )

    lieux_geocodes |>
      right_join(villes, by = "Birthplace") |>
      filter(!is.na(latitude), !is.na(longitude)) |>
      st_as_sf(coords = c("longitude", "latitude"),
               crs = CRS_GEO, remove = FALSE) |>
      # Extraction de la province / etat depuis la chaine "Ville, XX, Pays"
      mutate(
        CodeProv = case_when(
          Country == "Canada" ~ str_match(Birthplace,
                                          ",\\s*([A-Z]{2}),\\s*Canada$")[, 2],
          Country == "USA"    ~ str_match(Birthplace,
                                          ",\\s*([A-Z]{2}),\\s*USA$")[, 2],
          TRUE ~ NA_character_
        )
      )
  })
}


# --- 9. Fonds de carte ------------------------------------------------------
# rnaturalearth retelecharge (ou relit) la couche a chaque appel. Le script
# d'origine appelait ne_countries() une douzaine de fois. Ici : une fois.

charger_monde <- function(echelle = "medium") {
  en_cache(paste0("monde_", echelle),
           ne_countries(scale = echelle, returnclass = "sf"))
}

charger_provinces_canada <- function() {
  en_cache("provinces_ca",
           ne_states(country = "Canada", returnclass = "sf"))
}

charger_etats_us <- function() {
  en_cache("etats_us",
           ne_states(country = "United States of America",
                     returnclass = "sf"))
}


# --- 10. Utilitaires de sortie ----------------------------------------------
# Deux destinations distinctes, heritees des deux volets du projet :
#   figures/  -> sorties DESCRIPTIVES (modules 02 a 05, 11). Non versionnees,
#                elles se regenerent en quelques minutes.
#   sorties/  -> sorties STATISTIQUES (modules 06 a 10). Versionnees, pour
#                pouvoir citer les resultats des tests sans relancer R.

sauver_carte <- function(tm, nom, largeur = 10, hauteur = 6, dossier = SORTIES) {
  fichier <- file.path(dossier, nom)
  suppressMessages(
    tmap_save(tm = tm, filename = fichier,
              width = largeur, height = hauteur, dpi = 300)
  )
  message("  -> ", basename(fichier))
  invisible(fichier)
}

sauver_graphique <- function(gg, nom, largeur = 10, hauteur = 6,
                             dossier = SORTIES) {
  fichier <- file.path(dossier, nom)
  ggsave(fichier, plot = gg, width = largeur, height = hauteur, dpi = 300)
  message("  -> ", basename(fichier))
  invisible(fichier)
}

sauver_tableau <- function(df, nom, dossier = SORTIES) {
  fichier <- file.path(dossier, nom)
  # On retire la geometrie avant export CSV le cas echeant
  if (inherits(df, "sf")) df <- st_drop_geometry(df)
  write_csv(df, fichier)
  message("  -> ", basename(fichier))
  invisible(fichier)
}

# Raccourcis pour les modules descriptifs, qui ecrivent dans figures/
sauver_carte_fig     <- function(tm, nom, ...) sauver_carte(tm, nom, ..., dossier = FIGURES)
sauver_graphique_fig <- function(gg, nom, ...) sauver_graphique(gg, nom, ..., dossier = FIGURES)
sauver_tableau_fig   <- function(df, nom)      sauver_tableau(df, nom, dossier = FIGURES)

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


# --- 11. Carte regionale a cercles proportionnels ---------------------------
# Le script d'origine repetait quatorze fois le meme bloc de vingt lignes pour
# produire ses cartes regionales (Quebec, Ontario, Alberta, C.-B., provinces
# atlantiques, six regions americaines, Europe, pays nordiques, Russie).
# Seuls le sous-ensemble de villes, l'emprise, le facteur d'echelle et le titre
# changeaient. Cette fonction factorise le bloc : ajouter une region demande
# desormais quatre lignes.
#
# Syntaxe tmap 4 (tm_shape / tm_polygons / tm_symbols / tm_title / tm_credits),
# conforme au chapitre 1.5 du manuel.

carte_villes_region <- function(villes, fond, emprise, titre, fichier,
                                variable = "TotalPts",
                                titre_legende = "Points totaux",
                                echelle = 1.4,
                                couleur = "blue",
                                largeur = 10, hauteur = 6,
                                note = NULL) {

  if (nrow(villes) == 0) {
    message("  (aucune ville pour : ", titre, ") — carte non produite")
    return(invisible(NULL))
  }

  credits <- if (is.null(note)) CREDITS else paste0(note, "\n", CREDITS)

  carte <- tm_shape(fond, bbox = emprise) +
    tm_polygons(fill = "grey95", col = "grey70", lwd = 0.3) +
    tm_shape(villes) +
    tm_symbols(
      size = variable,
      # tmap 4 : le facteur d'agrandissement des symboles passe par
      # values.scale de l'echelle, et non plus par un argument "scale" nu
      # comme en tmap 3 (ou il etait silencieusement ignore).
      size.scale = tm_scale_continuous(values.scale = echelle),
      size.legend = tm_legend(title = titre_legende),
      fill = couleur,
      col = "grey20",
      lwd = 0.4,
      fill_alpha = 0.6
    ) +
    tm_title(titre) +
    tm_credits(credits, position = tm_pos_in("left", "bottom"), size = 0.7)

  sauver_carte_fig(carte, fichier, largeur = largeur, hauteur = hauteur)
  invisible(carte)
}

# Raccourci pour construire une emprise en degres.
emprise_geo <- function(xmin, xmax, ymin, ymax) {
  st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          crs = st_crs(CRS_GEO))
}


message("Configuration chargee (00_config.R).")
