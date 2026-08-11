# *****************************************************************************
# 09_stkde.R — Densité spatio-temporelle par noyau (STKDE)
# *****************************************************************************
# MÉTHODE : estimation de densité de noyau spatio-temporelle dans une maille
# régulière. Permet de voir l'évolution de la provenance des joueurs au fil du 
# temps.
#
# SORTIES : sorties/09_stkde_joueurs.gif (+ un graphique de densité temporelle).
# *****************************************************************************

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 09 — DENSITÉ SPATIO-TEMPORELLE (STKDE) ===")


# --- Paquets spécifiques ----------------------------------------------------
# sparr, gifski et viridis ne font pas partie du socle du projet. Plutôt que
# de planter au milieu du calcul, on vérifie tout de suite.

paquets_stkde <- c("spatstat.geom", "spatstat.explore", "sparr",
                   "terra", "gifski", "classInt", "viridis")
manquants <- paquets_stkde[
  !vapply(paquets_stkde, requireNamespace, logical(1), quietly = TRUE)
]

if (length(manquants) > 0) {
  # MODULE_IGNORE est lu par run_all.R : sans lui, le bilan afficherait "ok"
  # pour un module qui n'a rien produit du tout.
  MODULE_IGNORE <- TRUE
  message(
    "Module 09 IGNORÉ : paquets manquants -> ",
    paste(manquants, collapse = ", "), "\n",
    "  install.packages(c(\"", paste(manquants, collapse = "\", \""), "\"))\n",
    "  AUCUNE sortie STKDE ne sera produite."
  )
} else {

suppressPackageStartupMessages({
  library(spatstat.geom)
  library(spatstat.explore)
  library(sparr)
  library(terra)
  library(gifski)
  library(classInt)
  library(viridis)
})


# --- Paramètres -------------------------------------------------------------
# Regroupés ici pour qu'on puisse les changer sans fouiller dans le code.

H_SPATIAL   <- 150000   # largeur de bande spatiale, en mètres
LAMBDA_TEMP <- 8       # largeur de bande temporelle, en années
RESOLUTION_SPATIALE   <- 500   # ne pas baisser (voir ci-dessus)
RESOLUTION_TEMPORELLE <- 150   # -> 64 : deux fois plus rapide, sans perte
ESTIMER_BANDWIDTH <- FALSE   # Mettre TRUE si on souhaite ré-optimiser


# *****************************************************************************
# 1. PRÉPARATION DES DONNÉES ----
# *****************************************************************************
# Le script 01_geocodage.R alimente le fichier cache pour garder ce géocodage en
# mémoire. Une fois qu'il est roulé, on fait juste le lire.

hockey         <- charger_hockey()
lieux_geocodes <- charger_lieux_geocodes()
monde          <- charger_monde("medium")

# Jeu de données géocodé, au niveau de chaque joueur
prov_joueur_stkde <- hockey |>
  left_join(lieux_geocodes, by = "Birthplace")

# Conversion en objet spatial sf
prov_joueur_stkde_sf <- prov_joueur_stkde |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(AnneeNaissance)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO)

message("Joueurs géolocalisés et dates : ", nrow(prov_joueur_stkde_sf))


# *****************************************************************************
# 2. VISUALISATION DE LA DENSITÉ TEMPORELLE ----
# *****************************************************************************
# Note : Il aurait été plus intéressant de faire le STKDE avec la date d'entrée
# dans la LNH du joueur plutôt que sa date de naissance, mais nous ne possédons 
# pas cette information. On aurait pu la déduire en ajoutant, par exemple, 20 
# ans à l'année de naissance, mais cela n'est pas nécessairement vrai pour 
# chaque joueur et aurait ajouté un biais sur la donnée.

prov_joueur_stkde_sf$dt <- prov_joueur_stkde_sf$AnneeNaissance
prov_joueur_stkde_sf$dt_num <- as.numeric(
  prov_joueur_stkde_sf$dt - min(prov_joueur_stkde_sf$dt)
)

graph_densite_temporelle <- ggplot(prov_joueur_stkde_sf, aes(x = dt)) +
  geom_density(bw = "sj", color = "blue", lwd = 1) +
  labs(
    title = "Densité temporelle des naissances de joueurs de la LNH",
    y = "Densité", x = "Année de naissance",
    caption = CREDITS
  ) +
  theme_bw()

sauver_graphique(graph_densite_temporelle,
                 "09_stkde_densite_temporelle.png",
                     largeur = 9, hauteur = 5)


# *****************************************************************************
# 3. FENÊTRE D'OBSERVATION ET SEMIS ----
# *****************************************************************************
# Projection cylindrique équivalente mondiale : obligatoire, spatstat travaille
# en unités planes. CRS_MONDE est défini dans 00_config.R 

monde_sf <- st_transform(monde, crs = CRS_MONDE)
prov_joueur_stkde_sf <- st_transform(prov_joueur_stkde_sf, crs = CRS_MONDE)

pays_union   <- st_union(monde_sf)
fenetre_pays <- as.owin(st_buffer(pays_union, 10000))

XY <- st_coordinates(prov_joueur_stkde_sf)
prov_joueur_stkde_sf.ppp <- ppp(
  x = XY[, 1],
  y = XY[, 2],
  window = fenetre_pays, check = TRUE
)

## --- Estimation des deux meilleures largeurs de bande -----------------------
# On applique un jittering (micro-déplacement) puisque de nombreux joueurs
# partagent exactement la même coordonnée (même ville de naissance).
ppp_jittered <- rjitter(
  prov_joueur_stkde_sf.ppp,
  radius = 25,     # perturbation maximale, en mètres
  retry  = TRUE
)

# VÉRIFICATION CRITIQUE : ces deux valeurs doivent être strictement égales.
message("Points dans le semis jittéré : ", npoints(ppp_jittered))
message("Longueur du vecteur temps  : ", length(prov_joueur_stkde_sf$dt_num))
stopifnot(npoints(ppp_jittered) == length(prov_joueur_stkde_sf$dt_num))

# Estimation des deux meilleures bandwidths :
if (ESTIMER_BANDWIDTH) {
  message("Estimation des largeurs de bande (long : plusieurs dizaines de minutes)...")
  scores_bw <- LIK.spattemp(
    ppp_jittered,
    tt = prov_joueur_stkde_sf$dt_num,
    tlim = range(prov_joueur_stkde_sf$dt_num),
    parallelise = NA,
    verbose = TRUE
  )
  print(scores_bw)
}


# *****************************************************************************
# 4. CALCUL DES DENSITÉS SPATIO-TEMPORELLES ----
# *****************************************************************************

message("Calcul de la densité spatio-temporelle (",
        RESOLUTION_SPATIALE, "x", RESOLUTION_SPATIALE, "x",
        RESOLUTION_TEMPORELLE, " = ",
        format(RESOLUTION_SPATIALE^2 * RESOLUTION_TEMPORELLE / 1e6,
               digits = 3), " M cellules)...")

# Trouve l'étendue temporelle des données
tlim_observe <- range(prov_joueur_stkde_sf$dt_num)

# Chronométrage des deux étapes coûteuses :
# Calcul des valeurs de densités
t_densite <- system.time(
  dens_vals <- spattemp.density(
    ppp_jittered,
    h      = H_SPATIAL,
    lambda = LAMBDA_TEMP,
    tt     = prov_joueur_stkde_sf$dt_num,
    tlim   = tlim_observe,
    sres   = RESOLUTION_SPATIALE,
    tres   = RESOLUTION_TEMPORELLE,
    verbose = FALSE
  )
)["elapsed"]

message("  densité calculée en ", round(t_densite), " s")

## Extraction des rasters à chaque période
all_rasts <- lapply(dens_vals$z, function(x) {
  my_rast <- terra::rast(x) * 10000   # petit ajustement pour la carto
  vals <- terra::values(my_rast)
  vals[vals <= 0] <- NA
  terra::values(my_rast) <- vals
  terra::crs(my_rast) <- CRS_MONDE
  my_rast
})

## Extraction des valeurs pour créer une échelle commune de couleur
set.seed(2026)   # ajout : l'échantillonnage était aléatoire, donc la palette
                 # changeait à chaque exécution
all_densities <- do.call(c, lapply(all_rasts, function(x) {
  sample(terra::values(x), size = 100, replace = FALSE)
}))
color_breaks <- classIntervals(all_densities, n = 10, style = "kmeans")

## Préparation des dates
timestamps  <- round(as.numeric(names(dens_vals$z)))
time_frames <- min(prov_joueur_stkde_sf$dt) + timestamps


# *****************************************************************************
# 5. CRÉATION DE L'ANIMATION ----
# *****************************************************************************

message("Compilation de ", length(time_frames), " cartes...")

all_maps <- lapply(seq_along(time_frames), function(i) {
  tm_shape(all_rasts[[i]]) +
    tm_raster(
      col.scale = tm_scale_intervals(
        breaks = color_breaks$brks,
        values = viridis(10)
      ),
      col.legend = tm_legend(show = FALSE)
    ) +
    tm_shape(monde) + tm_borders(col = "gray", lwd = 0.05) +
    tm_title(text = as.character(time_frames[[i]]), color = "black", size = 2)
})

fichier_gif <- chemin_sortie("09_stkde_joueurs.gif")

t_rendu <- system.time(
  tmap_animation(
    all_maps, filename = fichier_gif,
    width = 750, height = 750, dpi = 150, delay = 25
  )
)["elapsed"]

message("  -> ", basename(fichier_gif))
message("  rendu des ", length(time_frames), " images en ",
        round(t_rendu), " s")
message("  RÉPARTITION : densité ", round(t_densite), " s | rendu ",
        round(t_rendu), " s")

}   # fin du bloc conditionnel sur les paquets

message("=== 09 terminé ===")
