# *****************************************************************************
# 08_semis_points.R — Probabilité qu'un joueur devienne un joueur d'élite
# *****************************************************************************
#
# Répond à la question : un joueur né à un endroit donné a-t-il plus de chances de devenir un
# joueur d'élite ? L'indicateur est un RAPPORT de deux densités estimées avec
# le même noyau, si bien que l'effet de la population s'annule.
#
# CADRE D'ÉTUDE : le Canada. C'est le pays qui fournit le plus de joueurs
# (5598)..
#
# SORTIES : 1 figure, 1 journal de résultats.
# *****************************************************************************

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

suppressPackageStartupMessages({
  library(spatstat.geom)
  library(spatstat.explore)
})

message("\n=== 08 — ANALYSE DE SEMIS DE POINTS ===")

# --- Paramètres de l'analyse ------------------------------------------------

PAYS_ETUDE   <- "Canada"
SIGMA        <- 100000   # rayon du noyau gaussien, en mètres (100 km)
SEUIL_ELITE  <- 500      # seuil de "joueur d'élite" en points en carrière
RESOLUTION   <- 300      # côtés de la grille de calcul

jrn <- journal_resultats("08_resultats_semis_points.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 08. SEMIS DE POINTS")
jrn$ecrire("=====================================================")
jrn$ecrire("Pays étudié      : ", PAYS_ETUDE)
jrn$ecrire("Noyau gaussien   : sigma = ", SIGMA / 1000, " km")
jrn$ecrire("Seuil d'élite    : ", SEUIL_ELITE, " points en carrière")
jrn$ecrire("Projection       : EPSG:", CRS_CA,
           " (Lambert conforme, Statistique Canada)")


# *****************************************************************************
# 1. PRÉPARATION : FENÊTRE D'OBSERVATION ET SEMIS ----
# *****************************************************************************

hockey    <- charger_hockey()
villes_sf <- construire_villes_sf()

# --- Fenêtre d'observation (owin) -------------------------------------------.

contour_pays <- ne_countries(country = PAYS_ETUDE, scale = "medium",
                            returnclass = "sf") |>
  st_transform(CRS_CA) |>
  st_geometry() |>
  # Simplification : le contour détaillé du Canada (milliers d'îles) rend les
  # calculs très lents sans rien changer à une analyse à l'échelle de 100 km.
  st_simplify(dTolerance = 2000) |>
  st_buffer(15000) |>
  st_make_valid()

fenetre <- as.owin(contour_pays)
# Conversion en masque matriciel : contrôle explicite de la résolution de
# calcul, et gain de vitesse important sur les noyaux de densité.
fenetre <- as.mask(fenetre, dimyx = RESOLUTION)

jrn$ecrire("Superficie de la fenêtre : ",
           format(round(area.owin(fenetre) / 1e6), big.mark = " "), " km2")


# Chargement et projection des provinces canadiennes
provinces_pays <- ne_states(country = PAYS_ETUDE, returnclass = "sf") |>
  st_transform(CRS_CA) |>
  st_simplify(dTolerance = 2000)           

# --- Semis de points --------------------------------------------------------
# IMPORTANT : on travaille au niveau des LOCALITÉS (une ville = un point), et
# non des joueurs. Empiler 5598 joueurs sur 997 coordonnées créerait des
# milliers de paires à distance nulle.. Le nombre de joueurs devient un POIDS, 
# pas une répétition du point.

villes_pays <- villes_sf |>
  filter(Country == PAYS_ETUDE) |>
  st_transform(CRS_CA) |>
  mutate(NbElite = as.numeric(NbElite))

# Recalcul du nombre de joueurs d'élite selon le seuil choisi ici
elite_par_ville <- hockey |>
  filter(Country == PAYS_ETUDE) |>
  group_by(Birthplace) |>
  summarise(NbEliteSeuil = sum(Pts >= SEUIL_ELITE, na.rm = TRUE),
            .groups = "drop")

villes_pays <- villes_pays |>
  left_join(elite_par_ville, by = "Birthplace") |>
  mutate(NbEliteSeuil = coalesce(NbEliteSeuil, 0))

# Les points doivent tomber DANS la fenêtre. Le géocodage place parfois une
# localité côtière légèrement en mer : on les écarte explicitement plutôt que
# de laisser spatstat les supprimer en silence.
coords_villes <- st_coordinates(villes_pays)
dans_fenetre  <- inside.owin(coords_villes[, 1], coords_villes[, 2], fenetre)

jrn$ecrire("")
jrn$ecrire("Localités du pays : ", nrow(villes_pays))
jrn$ecrire("Retenues (dans la fenêtre) : ", sum(dans_fenetre))
jrn$ecrire("Écartées (hors fenêtre, géocodage côtier) : ", sum(!dans_fenetre))

villes_ok <- villes_pays[dans_fenetre, ]
coords_ok <- coords_villes[dans_fenetre, ]

semis <- ppp(
  x = coords_ok[, 1], y = coords_ok[, 2],
  window = fenetre, check = FALSE
)

jrn$ecrire("Joueurs représentés : ", sum(villes_ok$NbJoueurs))
jrn$ecrire("Joueurs d'élite (", SEUIL_ELITE, "+ pts) : ",
           sum(villes_ok$NbEliteSeuil))


# --- Utilitaire de cartographie des surfaces --------------------------------
# Les objets "im" de spatstat sont convertis en data.frame pour être tracés
# avec ggplot2, ce qui donne des figures homogènes avec le reste du projet.

# EMPRISE D'AFFICHAGE
MARGE_AFFICHAGE <- 250000   # 250 km

emprise <- st_bbox(villes_ok)
limites_x <- c(emprise[["xmin"]] - MARGE_AFFICHAGE,
               emprise[["xmax"]] + MARGE_AFFICHAGE)
limites_y <- c(emprise[["ymin"]] - MARGE_AFFICHAGE,
               emprise[["ymax"]] + MARGE_AFFICHAGE)

carte_surface <- function(image, titre, sous_titre, legende,
                          palette = "viridis", transformation = "identity",
                          limites_admin = provinces_pays) {
  df <- as.data.frame(image)
  names(df) <- c("x", "y", "valeur")

  gg <- ggplot() +
    geom_sf(data = contour_pays, fill = "grey92", colour = NA) +
    geom_raster(data = df, aes(x = x, y = y, fill = valeur)) +
    geom_sf(data = limites_admin, fill = NA, colour = "grey40", 
            linewidth = 0.35, linetype = "solid") +
    geom_sf(data = contour_pays, fill = NA, colour = "grey20", 
            linewidth = 0.5) +
    coord_sf(xlim = limites_x, ylim = limites_y, expand = FALSE) +
    labs(title = titre, subtitle = sous_titre, fill = legende,
         x = NULL, y = NULL,
         caption = CREDITS) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )

  if (palette == "viridis") {
    gg <- gg + scale_fill_viridis_c(option = "magma", na.value = "transparent",
                                    trans = transformation)
  } else {
    gg <- gg + scale_fill_distiller(palette = palette, direction = 1,
                                    na.value = "transparent",
                                    trans = transformation)
  }
  gg
}


# *****************************************************************************
# 2. SURFACE DE RISQUE RELATIF ----
# *****************************************************************************
# QUESTION : un joueur né dans telle région a-t-il plus de chances de devenir
# un joueur d'élite ?
#
# MÉTHODE : rapport de deux densités estimées avec le MÊME noyau,
#     p(s) = densité(joueurs élite) / densité(tous les joueurs)
# C'est l'estimateur de risque relatif de Kelsall et Diggle. Son intérêt
# majeur ici : comme le numérateur et le dénominateur subissent la même
# distribution de population sous-jacente, l'effet de la population S'ANNULE.
# Aucune donnée démographique externe n'est nécessaire.

message("  Calcul des surfaces de densité...")

# Dénominateur : densité de TOUS les joueurs.
densite_joueurs <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$NbJoueurs,
  edge = TRUE   # correction de bordure : sans elle, la densité est
                # systématiquement sous-estimée près des frontières
)

# Numérateur : densité des seuls joueurs d'élite, MÊME noyau, MÊME correction.
densite_elite <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$NbEliteSeuil, edge = TRUE
)

# On masque les zones où le dénominateur est trop faible.

# CRITÈRE DE FIABILITÉ : plutôt qu'un percentile arbitraire, on exige un
# EFFECTIF MINIMAL de joueurs dans le voisinage du noyau. Pour un noyau
# gaussien d'écart-type sigma, la masse effective couvre 2*pi*sigma^2 ; le
# nombre attendu de joueurs sous le noyau vaut donc densité * 2*pi*sigma^2.
# En exigeant au moins MIN_JOUEURS_NOYAU joueurs, on garantit que chaque pixel
# affiche repose sur un échantillon interprétable.
MIN_JOUEURS_NOYAU <- 30
masse_noyau <- 2 * pi * SIGMA^2          # en m2
seuil_fiabilite <- MIN_JOUEURS_NOYAU / masse_noyau   # en joueurs par m2

risque_relatif <- eval.im(
  ifelse(densite_joueurs >= seuil_fiabilite,
         densite_elite / densite_joueurs, NA)
)

n_pixels_retenus <- sum(!is.na(risque_relatif$v))
n_pixels_total   <- sum(!is.na(densite_joueurs$v))
part_pixels_retenus <- n_pixels_retenus / n_pixels_total

carte_risque <- carte_surface(
  risque_relatif,
  titre = paste0("Probabilité qu'un joueur devienne un joueur d'élite — ",
                 PAYS_ETUDE),
  sous_titre = paste0("Rapport de densités (Kelsall-Diggle) : joueurs de ",
                      SEUIL_ELITE, "+ points / tous les joueurs.",
                      "\nZones grises : trop peu de joueurs pour un rapport fiable."),
  legende = paste0("Part de joueurs\nde ", SEUIL_ELITE, "+ pts"),
  palette = "YlGnBu"
)

sauver_graphique(carte_risque, "08_carte_risque_relatif_elite.png",
                 largeur = 10, hauteur = 7)

part_globale <- sum(villes_ok$NbEliteSeuil) / sum(villes_ok$NbJoueurs)

jrn$ecrire("")
jrn$ecrire("--- Surface de risque relatif ---")
jrn$ecrire("Part globale de joueurs d'élite : ",
           round(part_globale * 100, 2), " %")
jrn$ecrire("Critère de fiabilité : au moins ", MIN_JOUEURS_NOYAU,
           " joueurs sous le noyau")
jrn$ecrire("Part du territoire cartographiée : ",
           round(part_pixels_retenus * 100, 1), " %")
jrn$ecrire("Étendue de la surface (zones fiables) : ",
           round(min(risque_relatif, na.rm = TRUE) * 100, 2), " % à ",
           round(max(risque_relatif, na.rm = TRUE) * 100, 2), " %")
jrn$ecrire("(à comparer à la moyenne nationale de ",
           round(part_globale * 100, 2), " %)")
jrn$ecrire("")
jrn$ecrire("LECTURE : cette carte répond à une question que le module")
jrn$ecrire("04 ne peut pas poser. Ses cartes de points totaux mélangent")
jrn$ecrire("VOLUME et CALIBRE : une région apparaît productive parce qu'elle")
jrn$ecrire("envoie beaucoup de joueurs, pas nécessairement de meilleurs.")
jrn$ecrire("Ici, le volume est au dénominateur : il est neutralisé.")


jrn$fermer()
message("=== 08 terminé ===")
