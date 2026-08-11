# =============================================================================
# 08_semis_points.R — Analyse de semis de points (densité de noyau, Ripley)
# =============================================================================
# PROBLÈME TRAITÉ
# Le module 04 représente les villes par des cercles proportionnels. Ce
# mode de représentation sature dès que les villes sont nombreuses et proches
# (sud de l'Ontario, corridor Windsor-Québec) : les cercles se superposent et
# l'information devient illisible. Une SURFACE DE DENSITÉ résout ce problème et
# transforme un semis de points en champ continu interprétable.
#
# TROIS ANALYSES
#  1. Densité de noyau pondérée : où se concentre la production de joueurs, et
#     où se concentre la production de POINTS ?
#  2. Surface de risque relatif : quelle est la probabilité qu'un joueur né à
#     un endroit donné devienne un joueur d'élite ? Cet indicateur est un
#     RAPPORT de deux densités, donc l'effet de la population s'annule.
#  3. Fonction L de Ripley : les localités productrices sont-elles plus
#     regroupées que le hasard, et à quelle distance ?
#
# CADRE D'ÉTUDE : le Canada. C'est le pays qui fournit le plus de joueurs
# (5598), ce qui donne une fenêtre d'observation bien remplie. La démarche est
# transposable telle quelle à un autre pays en changeant PAYS_ETUDE.
#
# SORTIES : 4 figures, 1 tableau, 1 journal de résultats.
# =============================================================================

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
NB_SIM       <- 99       # simulations pour l'enveloppe de Ripley
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


# =============================================================================
# 1. PRÉPARATION : FENÊTRE D'OBSERVATION ET SEMIS
# =============================================================================

hockey    <- charger_hockey()
villes_sf <- construire_villes_sf()

# --- Fenêtre d'observation (owin) -------------------------------------------
# Toute analyse de semis exige une FENÊTRE : le domaine dans lequel les points
# auraient PU se trouver. Sans elle, la densité et la fonction L n'ont aucun
# sens (on ne saurait pas distinguer "absence de joueurs" de "hors du pays").

contour_pays <- ne_countries(country = PAYS_ETUDE, scale = "medium",
                            returnclass = "sf") |>
  st_transform(CRS_CA) |>
  st_geometry() |>
  # Simplification : le contour détaillé du Canada (milliers d'îles) rend les
  # calculs très lents sans rien changer à une analyse à l'échelle de 100 km.
  st_simplify(dTolerance = 2000) |>
  # Dilatation de 15 km : la simplification rogne les côtes et rejetait 76
  # localités côtières pourtant valides (Vancouver, Halifax, villes du
  # Saint-Laurent). La fenêtre dépasse ainsi légèrement le trait de côte, ce
  # qui est sans conséquence à l'échelle d'un noyau de 100 km, alors que
  # perdre 8 % des localités biaiserait toutes les densités.
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
# milliers de paires à distance nulle, ce qui invaliderait complètement la
# fonction de Ripley. Le nombre de joueurs devient un POIDS, pas une
# répétition du point.

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
# Les densités sont calculées sur la fenêtre COMPLÈTE du pays (c'est nécessaire
# pour que la correction de bordure soit juste). En revanche, l'affichage est
# cadré sur l'emprise des données, élargie d'une marge : l'Extrême-Arctique ne
# contient aucun lieu de naissance et n'occuperait que de la place, en plus de
# faire apparaître des artefacts de tramage liés à la rasterisation des
# milliers d'îles du masque.
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

  # ORDRE DES COUCHES :
  # 1. Le pays en gris pâle (fond)
  # 2. La surface calculée (raster)
  # 3. Les frontières administratives (provinces) par-dessus
  # 4. Le contour national extérieur (un peu plus épais)
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


# =============================================================================
# 2. DENSITÉ DE NOYAU PONDÉRÉE
# =============================================================================
# density.ppp estime l'intensité locale du semis. En passant des POIDS, on
# estime non pas la densité de localités, mais la densité de JOUEURS (ou de
# points marqués) par unité de surface.
#
# Le choix de sigma est un arbitrage : trop petit, la surface se réduit à des
# taches autour des grandes villes ; trop grand, tout est lisse. 100 km
# correspond à l'échelle d'une région de recrutement.

message("  Calcul des surfaces de densité...")

densite_joueurs <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$NbJoueurs,
  edge = TRUE   # correction de bordure : sans elle, la densité est
                # systématiquement sous-estimée près des frontières
)

densite_points <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$TotalPts, edge = TRUE
)

# Conversion en unités lisibles : joueurs par 10 000 km2
# (l'unité native est "joueurs par m2", illisible)
densite_joueurs_lis <- eval.im(densite_joueurs * 1e10)
densite_points_lis  <- eval.im(densite_points  * 1e10)

carte_dens_joueurs <- carte_surface(
  densite_joueurs_lis,
  titre = paste0("Densité de production de joueurs de la LNH — ", PAYS_ETUDE),
  sous_titre = paste0("Noyau gaussien, sigma = ", SIGMA / 1000,
                      " km, correction de bordure"),
  legende = "Joueurs par\n10 000 km2",
  transformation = "sqrt"   # écrase la domination du corridor Windsor-Québec
)

sauver_graphique(carte_dens_joueurs, "08_carte_densite_joueurs.png",
                 largeur = 10, hauteur = 7)

carte_dens_points <- carte_surface(
  densite_points_lis,
  titre = paste0("Densité de production offensive — ", PAYS_ETUDE),
  sous_titre = paste0("Points en carrière pondérés, sigma = ",
                      SIGMA / 1000, " km"),
  legende = "Points par\n10 000 km2",
  transformation = "sqrt"
)

sauver_graphique(carte_dens_points, "08_carte_densite_points.png",
                 largeur = 10, hauteur = 7)

jrn$ecrire("")
jrn$ecrire("--- Surfaces de densité ---")
jrn$ecrire("Densité max de joueurs  : ",
           round(max(densite_joueurs_lis), 1), " par 10 000 km2")
jrn$ecrire("Densité max de points   : ",
           round(max(densite_points_lis), 1), " par 10 000 km2")
jrn$ecrire("L'échelle de couleur est en racine carrée : sans cela, le sud de")
jrn$ecrire("l'Ontario écrase visuellement tout le reste du pays.")


# =============================================================================
# 3. SURFACE DE RISQUE RELATIF
# =============================================================================
# QUESTION : un joueur né dans telle région a-t-il plus de chances de devenir
# un joueur d'élite ?
#
# MÉTHODE : rapport de deux densités estimées avec le MÊME noyau,
#     p(s) = densité(joueurs élite) / densité(tous les joueurs)
# C'est l'estimateur de risque relatif de Kelsall et Diggle. Son intérêt
# majeur ici : comme le numérateur et le dénominateur subissent la même
# distribution de population sous-jacente, l'effet de la population S'ANNULE.
# Aucune donnée démographique externe n'est nécessaire.

densite_elite <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$NbEliteSeuil, edge = TRUE
)

# On masque les zones où le dénominateur est trop faible : le rapport y est
# numériquement instable (division de deux quasi-zéros) et produirait des
# artefacts spectaculaires mais dénués de sens.
#
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

# NOTE TECHNIQUE
# eval.im(ifelse(..., NA)) ne met pas des NA dans l'image : il RÉTRÉCIT la
# fenêtre de l'image résultante. Il faut donc compter les pixels valides dans
# la matrice $v (et non via im[], qui ne renvoie que les pixels de la fenêtre
# et ne contient jamais de NA).
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


# =============================================================================
# 4. FONCTION L DE RIPLEY
# =============================================================================
# La fonction K de Ripley compte le nombre moyen de voisins dans un rayon r.
# La fonction L = sqrt(K/pi) - r la stabilise : sous l'hypothèse nulle d'un
# semis complètement aléatoire (CSR), L(r) = 0 pour tout r.
#   L(r) > 0 -> regroupement à la distance r
#   L(r) < 0 -> répulsion / régularité
# L'enveloppe simule NB_SIM semis aléatoires et délimite l'intervalle
# compatible avec le hasard.

message("  Calcul de la fonction L de Ripley (", NB_SIM, " simulations)...")

set.seed(2026)
enveloppe_L <- envelope(
  semis, Lest, nsim = NB_SIM, verbose = FALSE,
  correction = "border"   # correction de bordure adaptée à une fenêtre
)                         # aussi découpée que le Canada

df_L <- as.data.frame(enveloppe_L) |>
  mutate(across(everything(), as.numeric)) |>
  mutate(r_km = r / 1000)

graph_ripley <- ggplot(df_L, aes(x = r_km)) +
  geom_ribbon(aes(ymin = lo / 1000, ymax = hi / 1000,
                  fill = "Enveloppe du hasard"), alpha = 0.45) +
  geom_line(aes(y = theo / 1000, colour = "Hypothèse nulle (CSR)"),
            linetype = "dashed", linewidth = 0.7) +
  geom_line(aes(y = obs / 1000, colour = "Semis observé"), linewidth = 0.9) +
  scale_fill_manual(values = c("Enveloppe du hasard" = "grey70")) +
  scale_colour_manual(values = c("Semis observé" = "#b2182b",
                                 "Hypothèse nulle (CSR)" = "grey30")) +
  labs(
    title = paste0("Fonction L de Ripley — localités productrices de joueurs (",
                   PAYS_ETUDE, ")"),
    subtitle = paste0("Au-dessus de l'enveloppe = regroupement significatif. ",
                      NB_SIM, " simulations."),
    x = "Distance r (km)", y = "L(r) - r  (km)",
    colour = NULL, fill = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

sauver_graphique(graph_ripley, "08_graph_fonction_ripley.png",
                 largeur = 9, hauteur = 6)

# Résumé quantitatif : à quelles distances le regroupement est-il significatif ?
df_L <- df_L |>
  mutate(
    Statut = case_when(
      obs > hi ~ "Regroupement significatif",
      obs < lo ~ "Répulsion significative",
      TRUE     ~ "Compatible avec le hasard"
    )
  )

resume_L <- df_L |>
  filter(r > 0) |>
  group_by(Statut) |>
  summarise(
    NbDistances = n(),
    DistanceMin_km = round(min(r_km), 1),
    DistanceMax_km = round(max(r_km), 1),
    .groups = "drop"
  )

sauver_tableau(
  df_L |> select(r_km, obs, theo, lo, hi, Statut),
  "08_table_fonction_ripley.csv"
)

jrn$capturer(as.data.frame(resume_L),
             "FONCTION L DE RIPLEY — plages de distance par statut")

# Écart maximal à l'hypothèse nulle
ligne_max <- df_L |> filter(r > 0) |> slice_max(obs - theo, n = 1)

jrn$ecrire("")
jrn$ecrire("Écart maximal au hasard à r = ", round(ligne_max$r_km, 1), " km")
jrn$ecrire("  L(r) - r observé : ", round(ligne_max$obs / 1000, 2), " km")
jrn$ecrire("  borne haute de l'enveloppe : ", round(ligne_max$hi / 1000, 2), " km")

# Signalement automatique de l'artefact de grande distance
repulsion <- df_L |> filter(Statut == "Répulsion significative", r > 0)
if (nrow(repulsion) > 0) {
  jrn$ecrire("")
  jrn$ecrire("ARTEFACT À SIGNALER")
  jrn$ecrire("Une 'répulsion significative' apparaît entre ",
             round(min(repulsion$r_km), 0), " et ",
             round(max(repulsion$r_km), 0), " km.")
  jrn$ecrire("Il ne faut PAS l'interpréter comme un phénomène réel. Aux")
  jrn$ecrire("grandes distances, la fonction L est dominée par la GÉOMÉTRIE de")
  jrn$ecrire("la fenêtre : le Canada est un territoire très allongé d'est en")
  jrn$ecrire("ouest, si bien que peu de paires de localités peuvent être")
  jrn$ecrire("séparées de 700 km dans toutes les directions. La correction de")
  jrn$ecrire("bordure atténue ce biais sans l'éliminer.")
  jrn$ecrire("Règle pratique : ne pas interpréter L(r) au-delà du quart de la")
  jrn$ecrire("plus petite dimension de la fenêtre.")
}

jrn$ecrire("")
jrn$ecrire("MISE EN GARDE ESSENTIELLE SUR CE TEST")
jrn$ecrire("L'hypothèse nulle CSR suppose que les localités productrices")
jrn$ecrire("pourraient se répartir UNIFORMÉMENT sur le territoire canadien.")
jrn$ecrire("C'est évidemment faux : la population elle-même est concentrée")
jrn$ecrire("dans le sud du pays. Un rejet de CSR était donc acquis d'avance et")
jrn$ecrire("ne démontre RIEN sur le hockey : il redémontre que les Canadiens")
jrn$ecrire("vivent en ville.")
jrn$ecrire("")
jrn$ecrire("Ce test est rapporté pour deux raisons légitimes :")
jrn$ecrire(" 1. il quantifie l'ÉCHELLE du regroupement (la distance à laquelle")
jrn$ecrire("    l'écart au hasard est maximal), information utile en soi ;")
jrn$ecrire(" 2. il illustre une règle de méthode : un test spatial ne vaut que")
jrn$ecrire("    ce que vaut son hypothèse nulle.")
jrn$ecrire("")
jrn$ecrire("Un test rigoureux exigerait une hypothèse nulle INHOMOGÈNE, calée")
jrn$ecrire("sur la densité de population (fonction Linhom avec une intensité")
jrn$ecrire("issue d'une grille de population). C'est la principale piste")
jrn$ecrire("d'amélioration de cette section.")
jrn$ecrire("")
jrn$ecrire("À l'inverse, la surface de risque relatif de la section 3 n'a PAS")
jrn$ecrire("ce défaut : le rapport de deux densités élimine l'effet de la")
jrn$ecrire("population. C'est le résultat le plus solide de ce module.")

jrn$fermer()
message("=== 08 terminé ===")
