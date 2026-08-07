# =============================================================================
# 11_stkde.R — Densite spatio-temporelle par noyau (STKDE)
# =============================================================================
# >>> MODULE DE XAVIER LAFRANCE — CHANTIER EN COURS <<<
#
# ORIGINE : SECTION 5 de archive/Projet_Hockey_script_ORIGINAL.R, branche
# XavierL, commit 4a28724 "Ajout du STKDE".
#
# Le code est repris TEL QUEL : memes noms de variables (prov_joueur_stkde,
# dens_vals, all_rasts, all_maps, color_breaks, time_frames), meme
# enchainement, memes valeurs de parametres. Rien n'a ete "ameliore" en
# silence. Trois choses seulement ont change, et elles sont signalees ligne a
# ligne plus bas :
#   1. le bloc de geocodage a ete retire (il fait doublon avec le module 01) ;
#   2. les chemins et le fond de carte passent par 00_config.R ;
#   3. les paquets manquants font sortir proprement au lieu de planter.
#
# Les points restes ouverts sont marques  # TODO XAVIER  — ils ne sont pas
# corriges, c'est a toi de decider.
#
# METHODE : estimation de densite de noyau spatio-temporelle (manuel du cours,
# chapitre 4, package sparr). Permet de voir l'evolution de la provenance des
# joueurs au fil du temps.
#
# SORTIES : figures/stkde_joueurs.gif (+ un graphique de densite temporelle).
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 11 — DENSITE SPATIO-TEMPORELLE (STKDE) ===")


# --- Paquets specifiques ----------------------------------------------------
# sparr, gifski et viridis ne font pas partie du socle du projet. Plutot que
# de planter au milieu du calcul, on verifie tout de suite.

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
    "Module 11 IGNORE : paquets manquants -> ",
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


# --- Parametres -------------------------------------------------------------
# Regroupes ici pour que tu puisses les changer sans fouiller le code.

H_SPATIAL   <- 100000   # largeur de bande spatiale, en metres
LAMBDA_TEMP <- 10       # largeur de bande temporelle, en annees
RESOLUTION_SPATIALE <- 500
RESOLUTION_TEMPORELLE <- 150
ESTIMER_BANDWIDTH <- FALSE   # TODO XAVIER : voir la section 3 ci-dessous


# =============================================================================
# 1. PREPARATION DES DONNEES
# =============================================================================
# CHANGEMENT 1 : le script d'origine relancait ici un geocodage
# (lieux_uniques + tidygeocoder::geocode). C'est desormais le travail du
# module 01, qui alimente le meme fichier cache. On se contente de le lire.

hockey         <- charger_hockey()
lieux_geocodes <- charger_lieux_geocodes()
monde          <- charger_monde("medium")

# Jeu de donnees geocode, au niveau du JOUEUR
prov_joueur_stkde <- hockey |>
  left_join(lieux_geocodes, by = "Birthplace")

# Conversion en objet spatial sf
prov_joueur_stkde_sf <- prov_joueur_stkde |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(AnneeNaissance)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO)

message("Joueurs geolocalises et dates : ", nrow(prov_joueur_stkde_sf))


# =============================================================================
# 2. VISUALISATION DE LA DENSITE TEMPORELLE
# =============================================================================
# TODO XAVIER (note laissee dans ton commit d'origine) :
#   "Vaudrait plus la peine que la variable temporelle soit la premiere annee
#    de jeu dans la LNH plutot que l'annee de naissance."
#   -> le jeu de donnees ne contient pas la premiere saison. Il faudrait
#      l'ajouter au CSV source, ou l'approximer par AnneeNaissance + 20.
#      Une approximation biaiserait les debuts et fins de periode : a discuter
#      dans le rapport si tu retiens cette piste.

prov_joueur_stkde_sf$dt <- prov_joueur_stkde_sf$AnneeNaissance
prov_joueur_stkde_sf$dt_num <- as.numeric(
  prov_joueur_stkde_sf$dt - min(prov_joueur_stkde_sf$dt)
)

graph_densite_temporelle <- ggplot(prov_joueur_stkde_sf, aes(x = dt)) +
  geom_density(bw = "sj", color = "blue", lwd = 1) +
  labs(
    title = "Densite temporelle des naissances de joueurs de la LNH",
    y = "Densite", x = "Annee de naissance",
    caption = CREDITS
  ) +
  theme_bw()

sauver_graphique_fig(graph_densite_temporelle,
                     "stkde_densite_temporelle.png",
                     largeur = 9, hauteur = 5)


# =============================================================================
# 3. FENETRE D'OBSERVATION ET SEMIS
# =============================================================================
# Projection cylindrique equivalente mondiale : obligatoire, spatstat travaille
# en unites planes. CRS_MONDE est defini dans 00_config.R (c'est exactement la
# chaine proj que tu utilisais).

monde_sf <- st_transform(monde, crs = CRS_MONDE)
prov_joueur_stkde_sf <- st_transform(prov_joueur_stkde_sf, crs = CRS_MONDE)

pays_union   <- st_union(monde_sf)
fenetre_pays <- as.owin(st_buffer(pays_union, 10000))

joueurs_union      <- st_union(prov_joueur_stkde_sf)
fenetre_joueurs_sf <- st_buffer(joueurs_union, 100000)
fenetre_joueurs    <- as.owin(fenetre_joueurs_sf)

XY <- st_coordinates(prov_joueur_stkde_sf)
prov_joueur_stkde_sf.ppp <- ppp(
  x = XY[, 1],
  y = XY[, 2],
  window = fenetre_pays, check = TRUE
)

# --- Estimation des deux meilleures largeurs de bande -----------------------
# On applique un jittering (micro-deplacement) puisque de nombreux joueurs
# partagent exactement la meme coordonnee (meme ville de naissance).
ppp_jittered <- rjitter(
  prov_joueur_stkde_sf.ppp,
  radius = 25,     # perturbation maximale, en metres
  retry  = TRUE
)

# VERIFICATION CRITIQUE : ces deux valeurs doivent etre STRICTEMENT EGALES.
message("Points dans le ppp jittere : ", npoints(ppp_jittered))
message("Longueur du vecteur temps  : ", length(prov_joueur_stkde_sf$dt_num))
stopifnot(npoints(ppp_jittered) == length(prov_joueur_stkde_sf$dt_num))

# TODO XAVIER : LIK.spattemp met plusieurs dizaines de minutes sur ce jeu de
# donnees, ce qui bloque run_all.R. Il est donc desactive par defaut
# (ESTIMER_BANDWIDTH <- FALSE en haut du fichier). Deux choses restent a faire :
#   a) le lancer UNE fois, a la main, et noter les valeurs obtenues ;
#   b) reporter ces valeurs dans H_SPATIAL et LAMBDA_TEMP, plutot que de
#      garder les valeurs rondes actuelles (100 km / 10 ans) qui ont ete
#      choisies a la main.
# Tant que ce n'est pas fait, il faut ecrire dans le rapport que les largeurs
# de bande sont fixees a priori et non optimisees.
if (ESTIMER_BANDWIDTH) {
  message("Estimation des largeurs de bande (long : plusieurs dizaines de minutes)...")
  scores_bw <- LIK.spattemp(
    ppp_jittered,
    tt = prov_joueur_stkde_sf$dt_num,
    tlim = range(prov_joueur_stkde_sf$dt_num),
    # start = c(100000, 5),
    parallelise = NA,
    verbose = TRUE
  )
  print(scores_bw)
}


# =============================================================================
# 4. CALCUL DES DENSITES SPATIO-TEMPORELLES
# =============================================================================
# TODO XAVIER : le semis passe ici est prov_joueur_stkde_sf.ppp (NON jittere),
# alors que l'estimation des largeurs de bande ci-dessus utilise ppp_jittered.
# Les deux devraient etre coherents. A trancher : soit tout en jittere, soit
# tout en brut. Le jittering est necessaire des qu'un calcul depend des
# distances entre paires ; pour une densite de noyau il est moins critique.

message("Calcul de la densite spatio-temporelle (",
        RESOLUTION_SPATIALE, "x", RESOLUTION_SPATIALE, "x",
        RESOLUTION_TEMPORELLE, " = ",
        format(RESOLUTION_SPATIALE^2 * RESOLUTION_TEMPORELLE / 1e6,
               digits = 3), " M cellules)...")

# tlim couvre toute l'etendue temporelle observee. La valeur c(0, 128) etait
# codee en dur : elle est maintenant deduite des donnees, ce qui evite de
# tronquer silencieusement les dernieres decennies si le CSV est mis a jour.
tlim_observe <- range(prov_joueur_stkde_sf$dt_num)

# Chronometrage des deux etapes couteuses. Sans ca, on optimise a l'aveugle :
# selon que le temps part dans le calcul de densite ou dans le rendu des
# images, la marche a suivre n'est pas du tout la meme.
t_densite <- system.time(
  dens_vals <- spattemp.density(
    prov_joueur_stkde_sf.ppp,
    h      = H_SPATIAL,
    lambda = LAMBDA_TEMP,
    tt     = prov_joueur_stkde_sf$dt_num,
    tlim   = tlim_observe,
    sres   = RESOLUTION_SPATIALE,
    tres   = RESOLUTION_TEMPORELLE,
    verbose = FALSE
  )
)["elapsed"]

message("  densite calculee en ", round(t_densite), " s")

## Extraction des rasters a chaque periode
all_rasts <- lapply(dens_vals$z, function(x) {
  my_rast <- terra::rast(x) * 10000   # petit ajustement pour la carto
  vals <- terra::values(my_rast)
  vals <- ifelse(is.na(vals), 0, vals)
  terra::values(my_rast) <- vals
  terra::crs(my_rast) <- CRS_MONDE
  my_rast
})

## Extraction des valeurs pour creer une echelle commune de couleur
set.seed(2026)   # ajout : l'echantillonnage etait aleatoire, donc la palette
                 # changeait a chaque execution
all_densities <- do.call(c, lapply(all_rasts, function(x) {
  sample(terra::values(x), size = 100, replace = FALSE)
}))
color_breaks <- classIntervals(all_densities, n = 10, style = "kmeans")

## Preparation des dates
timestamps  <- round(as.numeric(names(dens_vals$z)))
time_frames <- min(prov_joueur_stkde_sf$dt) + timestamps


# =============================================================================
# 5. ANIMATION
# =============================================================================

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
    tm_shape(monde) + tm_borders(col = "black", lwd = 0.05) +
    tm_title(text = as.character(time_frames[[i]]), color = "black", size = 8)
})

fichier_gif <- chemin_figure("stkde_joueurs.gif")

t_rendu <- system.time(
  tmap_animation(
    all_maps, filename = fichier_gif,
    width = 500, height = 500, dpi = 150, delay = 25
  )
)["elapsed"]

message("  -> ", basename(fichier_gif))
message("  rendu des ", length(time_frames), " images en ",
        round(t_rendu), " s")
message("  REPARTITION : densite ", round(t_densite), " s | rendu ",
        round(t_rendu), " s")

}   # fin du bloc conditionnel sur les paquets

message("=== 11 termine ===")
