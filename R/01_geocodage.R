# =============================================================================
# 01_geocodage.R — Géocodage incrémental des lieux de naissance
# =============================================================================
# ORIGINE : section 3.2 de archive/Projet_Hockey_script_ORIGINAL.R.
#
# Le fichier cache data/geocodage/lieux_naissance_geocodes_lieux_modernes.csv
# contient DÉJÀ les coordonnées de tous les lieux de naissance présents dans le
# jeu de données. Ce module ne relance donc PAS un géocodage complet : il ne
# traite que les lieux absents du cache. Aujourd'hui : aucun, donc il s'exécute
# en une seconde.
#
# Il faut le lancer avant les modules 04, 07, 08 et 09, qui ont tous besoin de
# coordonnées. Les modules 02, 03, 05, 06 et 10 n'en ont pas besoin.
#
# SORTIE : mise à jour du cache CSV, aucun fichier de résultat.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 01 — GÉOCODAGE DES LIEUX DE NAISSANCE ===")

hockey <- charger_hockey()

# Lieux uniques présents dans les données
lieux_naissance <- hockey |>
  filter(!is.na(Birthplace)) |>
  count(Birthplace, Country, sort = TRUE, name = "NbJoueurs")

message("Lieux de naissance distincts : ", nrow(lieux_naissance))

fichier_cache <- FICHIER_CACHE_GEO()

if (file.exists(fichier_cache)) {
  cache_geo <- read_csv(fichier_cache, show_col_types = FALSE)
  message("Cache existant : ", nrow(cache_geo), " lieux déjà géocodés")
} else {
  cache_geo <- tibble::tibble(
    Birthplace = character(),
    latitude   = double(),
    longitude  = double()
  )
  message("Aucun cache : un géocodage complet va être lancé.")
}

# Lieux présents dans les données mais pas encore dans le cache
lieux_a_geocoder <- lieux_naissance |>
  filter(!(Birthplace %in% cache_geo$Birthplace))

if (nrow(lieux_a_geocoder) > 0) {

  if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
    stop(
      nrow(lieux_a_geocoder), " lieu(x) restent à géocoder, mais le package\n",
      "tidygeocoder n'est pas installé. Lancer install_packages.R.",
      call. = FALSE
    )
  }

  message("Géocodage de ", nrow(lieux_a_geocoder), " nouveau(x) lieu(x)...")

  nouveaux_geo <- lieux_a_geocoder |>
    tidygeocoder::geocode(
      address = Birthplace,
      # arcgis : rapide, sans limite d'une requête par seconde et sans clé
      # API. OSM/Nominatim demanderait environ 40 minutes pour 2300 lieux.
      method  = "arcgis",
      lat     = latitude,
      long    = longitude
    )

  cache_geo <- bind_rows(cache_geo, nouveaux_geo)
  write_csv(cache_geo, fichier_cache)
  message("Cache mis à jour : ", nrow(cache_geo), " lieux")

} else {
  message("Aucun nouveau lieu à géocoder : le cache est complet.")
}

# Contrôle de qualité : lieux restés sans coordonnées
sans_coord <- cache_geo |>
  filter(is.na(latitude) | is.na(longitude))

if (nrow(sans_coord) > 0) {
  message("ATTENTION : ", nrow(sans_coord),
          " lieu(x) sans coordonnées après géocodage.")
  print(utils::head(sans_coord$Birthplace, 10))
}

# Le cache est vide du cache de session pour que les modules suivants
# relisent la version à jour.
if (exists("lieux_geocodes", envir = .cache_projet, inherits = FALSE)) {
  rm("lieux_geocodes", envir = .cache_projet)
}
if (exists("villes_sf", envir = .cache_projet, inherits = FALSE)) {
  rm("villes_sf", envir = .cache_projet)
}

message("=== 01 terminé ===")
