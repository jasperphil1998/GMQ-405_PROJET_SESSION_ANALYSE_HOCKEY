# =============================================================================
# 03_semis_points.R — Analyse de semis de points (densite de noyau, Ripley)
# =============================================================================
# PROBLEME ADRESSE
# Le script principal represente les villes par des cercles proportionnels. Ce
# mode de representation sature des que les villes sont nombreuses et proches
# (sud de l'Ontario, corridor Windsor-Quebec) : les cercles se superposent et
# l'information devient illisible. Une SURFACE DE DENSITE resout ce probleme et
# transforme un semis de points en champ continu interpretable.
#
# TROIS ANALYSES
#  1. Densite de noyau ponderee : ou se concentre la production de joueurs, et
#     ou se concentre la production de POINTS ?
#  2. Surface de risque relatif : quelle est la probabilite qu'un joueur ne a
#     un endroit donne devienne un joueur d'elite ? Cet indicateur est un
#     RAPPORT de deux densites, donc l'effet de la population s'annule.
#  3. Fonction L de Ripley : les localites productrices sont-elles plus
#     regroupees que le hasard, et a quelle distance ?
#
# CADRE D'ETUDE : le Canada. C'est le pays qui fournit le plus de joueurs
# (5598), ce qui donne une fenetre d'observation bien remplie. La demarche est
# transposable telle quelle a un autre pays en changeant PAYS_ETUDE.
#
# SORTIES : 4 figures, 1 tableau, 1 journal de resultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("analyse_spatiale", "R", "00_config.R"))

suppressPackageStartupMessages({
  library(spatstat.geom)
  library(spatstat.explore)
})

message("\n=== 03 — ANALYSE DE SEMIS DE POINTS ===")

# --- Parametres de l'analyse ------------------------------------------------

PAYS_ETUDE   <- "Canada"
SIGMA        <- 100000   # rayon du noyau gaussien, en metres (100 km)
SEUIL_ELITE  <- 500      # seuil de "joueur d'elite" en points en carriere
NB_SIM       <- 99       # simulations pour l'enveloppe de Ripley
RESOLUTION   <- 300      # cotes de la grille de calcul

jrn <- journal_resultats("03_resultats_semis_points.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 03. SEMIS DE POINTS")
jrn$ecrire("=====================================================")
jrn$ecrire("Pays etudie      : ", PAYS_ETUDE)
jrn$ecrire("Noyau gaussien   : sigma = ", SIGMA / 1000, " km")
jrn$ecrire("Seuil d'elite    : ", SEUIL_ELITE, " points en carriere")
jrn$ecrire("Projection       : EPSG:", CRS_CA,
           " (Lambert conforme, Statistique Canada)")


# =============================================================================
# 1. PREPARATION : FENETRE D'OBSERVATION ET SEMIS
# =============================================================================

hockey         <- charger_hockey()
lieux_geocodes <- charger_lieux_geocodes()
villes_sf      <- construire_villes_sf(hockey, lieux_geocodes)

# --- Fenetre d'observation (owin) -------------------------------------------
# Toute analyse de semis exige une FENETRE : le domaine dans lequel les points
# auraient PU se trouver. Sans elle, la densite et la fonction L n'ont aucun
# sens (on ne saurait pas distinguer "absence de joueurs" de "hors du pays").

contour_pays <- ne_countries(country = PAYS_ETUDE, scale = "medium",
                            returnclass = "sf") %>%
  st_transform(CRS_CA) %>%
  st_geometry() %>%
  # Simplification : le contour detaille du Canada (milliers d'iles) rend les
  # calculs tres lents sans rien changer a une analyse a l'echelle de 100 km.
  st_simplify(dTolerance = 2000) %>%
  # Dilatation de 15 km : la simplification rogne les cotes et rejetait 76
  # localites cotieres pourtant valides (Vancouver, Halifax, villes du
  # Saint-Laurent). La fenetre depasse ainsi legerement le trait de cote, ce
  # qui est sans consequence a l'echelle d'un noyau de 100 km, alors que
  # perdre 8 % des localites biaiserait toutes les densites.
  st_buffer(15000) %>%
  st_make_valid()

fenetre <- as.owin(contour_pays)
# Conversion en masque matriciel : controle explicite de la resolution de
# calcul, et gain de vitesse important sur les noyaux de densite.
fenetre <- as.mask(fenetre, dimyx = RESOLUTION)

jrn$ecrire("Superficie de la fenetre : ",
           format(round(area.owin(fenetre) / 1e6), big.mark = " "), " km2")

# --- Semis de points --------------------------------------------------------
# IMPORTANT : on travaille au niveau des LOCALITES (une ville = un point), et
# non des joueurs. Empiler 5598 joueurs sur 997 coordonnees creerait des
# milliers de paires a distance nulle, ce qui invaliderait completement la
# fonction de Ripley. Le nombre de joueurs devient un POIDS, pas une
# repetition du point.

villes_pays <- villes_sf %>%
  filter(Country == PAYS_ETUDE) %>%
  st_transform(CRS_CA) %>%
  mutate(NbElite = as.numeric(NbElite))

# Recalcul du nombre de joueurs d'elite selon le seuil choisi ici
elite_par_ville <- hockey %>%
  filter(Country == PAYS_ETUDE) %>%
  group_by(Birthplace) %>%
  summarise(NbEliteSeuil = sum(Pts >= SEUIL_ELITE, na.rm = TRUE),
            .groups = "drop")

villes_pays <- villes_pays %>%
  left_join(elite_par_ville, by = "Birthplace") %>%
  mutate(NbEliteSeuil = coalesce(NbEliteSeuil, 0))

# Les points doivent tomber DANS la fenetre. Le geocodage place parfois une
# localite cotiere legerement en mer : on les ecarte explicitement plutot que
# de laisser spatstat les supprimer en silence.
coords_villes <- st_coordinates(villes_pays)
dans_fenetre  <- inside.owin(coords_villes[, 1], coords_villes[, 2], fenetre)

jrn$ecrire("")
jrn$ecrire("Localites du pays : ", nrow(villes_pays))
jrn$ecrire("Retenues (dans la fenetre) : ", sum(dans_fenetre))
jrn$ecrire("Ecartees (hors fenetre, geocodage cotier) : ", sum(!dans_fenetre))

villes_ok <- villes_pays[dans_fenetre, ]
coords_ok <- coords_villes[dans_fenetre, ]

semis <- ppp(
  x = coords_ok[, 1], y = coords_ok[, 2],
  window = fenetre, check = FALSE
)

jrn$ecrire("Joueurs representes : ", sum(villes_ok$NbJoueurs))
jrn$ecrire("Joueurs d'elite (", SEUIL_ELITE, "+ pts) : ",
           sum(villes_ok$NbEliteSeuil))


# --- Utilitaire de cartographie des surfaces --------------------------------
# Les objets "im" de spatstat sont convertis en data.frame pour etre traces
# avec ggplot2, ce qui donne des figures homogenes avec le reste du projet.

# EMPRISE D'AFFICHAGE
# Les densites sont calculees sur la fenetre COMPLETE du pays (c'est necessaire
# pour que la correction de bordure soit juste). En revanche, l'affichage est
# cadre sur l'emprise des donnees, elargie d'une marge : l'Extreme-Arctique ne
# contient aucun lieu de naissance et n'occuperait que de la place, en plus de
# faire apparaitre des artefacts de tramage lies a la rasterisation des
# milliers d'iles du masque.
MARGE_AFFICHAGE <- 250000   # 250 km

emprise <- st_bbox(villes_ok)
limites_x <- c(emprise[["xmin"]] - MARGE_AFFICHAGE,
               emprise[["xmax"]] + MARGE_AFFICHAGE)
limites_y <- c(emprise[["ymin"]] - MARGE_AFFICHAGE,
               emprise[["ymax"]] + MARGE_AFFICHAGE)

carte_surface <- function(image, titre, sous_titre, legende,
                          palette = "viridis", transformation = "identity") {
  df <- as.data.frame(image)
  names(df) <- c("x", "y", "valeur")

  # ORDRE DES COUCHES (important)
  #  1. le pays en gris pale : sert de fond, de sorte que les zones sans
  #     donnee se lisent comme "pas de donnee" et non comme du vide blanc ;
  #  2. la surface calculee ;
  #  3. le contour en gris fonce PAR-DESSUS. Un contour blanc serait
  #     invisible sur le fond blanc de la figure.
  gg <- ggplot() +
    geom_sf(data = contour_pays, fill = "grey92", colour = NA) +
    geom_raster(data = df, aes(x = x, y = y, fill = valeur)) +
    geom_sf(data = contour_pays, fill = NA, colour = "grey35",
            linewidth = 0.3) +
    coord_sf(xlim = limites_x, ylim = limites_y, expand = FALSE) +
    labs(title = titre, subtitle = sous_titre, fill = legende,
         x = NULL, y = NULL,
         caption = "Auteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud") +
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
# 2. DENSITE DE NOYAU PONDEREE
# =============================================================================
# density.ppp estime l'intensite locale du semis. En passant des POIDS, on
# estime non pas la densite de localites, mais la densite de JOUEURS (ou de
# points marques) par unite de surface.
#
# Le choix de sigma est un arbitrage : trop petit, la surface se reduit a des
# taches autour des grandes villes ; trop grand, tout est lisse. 100 km
# correspond a l'echelle d'une region de recrutement.

message("  Calcul des surfaces de densite...")

densite_joueurs <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$NbJoueurs,
  edge = TRUE   # correction de bordure : sans elle, la densite est
                # systematiquement sous-estimee pres des frontieres
)

densite_points <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$TotalPts, edge = TRUE
)

# Conversion en unites lisibles : joueurs par 10 000 km2
# (l'unite native est "joueurs par m2", illisible)
densite_joueurs_lis <- eval.im(densite_joueurs * 1e10)
densite_points_lis  <- eval.im(densite_points  * 1e10)

carte_dens_joueurs <- carte_surface(
  densite_joueurs_lis,
  titre = paste0("Densite de production de joueurs de la LNH — ", PAYS_ETUDE),
  sous_titre = paste0("Noyau gaussien, sigma = ", SIGMA / 1000,
                      " km, correction de bordure"),
  legende = "Joueurs par\n10 000 km2",
  transformation = "sqrt"   # ecrase la domination du corridor Windsor-Quebec
)

sauver_graphique(carte_dens_joueurs, "03_carte_densite_joueurs.png",
                 largeur = 10, hauteur = 7)

carte_dens_points <- carte_surface(
  densite_points_lis,
  titre = paste0("Densite de production offensive — ", PAYS_ETUDE),
  sous_titre = paste0("Points en carriere ponderes, sigma = ",
                      SIGMA / 1000, " km"),
  legende = "Points par\n10 000 km2",
  transformation = "sqrt"
)

sauver_graphique(carte_dens_points, "03_carte_densite_points.png",
                 largeur = 10, hauteur = 7)

jrn$ecrire("")
jrn$ecrire("--- Surfaces de densite ---")
jrn$ecrire("Densite max de joueurs  : ",
           round(max(densite_joueurs_lis), 1), " par 10 000 km2")
jrn$ecrire("Densite max de points   : ",
           round(max(densite_points_lis), 1), " par 10 000 km2")
jrn$ecrire("L'echelle de couleur est en racine carree : sans cela, le sud de")
jrn$ecrire("l'Ontario ecrase visuellement tout le reste du pays.")


# =============================================================================
# 3. SURFACE DE RISQUE RELATIF
# =============================================================================
# QUESTION : un joueur ne dans telle region a-t-il plus de chances de devenir
# un joueur d'elite ?
#
# METHODE : rapport de deux densites estimees avec le MEME noyau,
#     p(s) = densite(joueurs elite) / densite(tous les joueurs)
# C'est l'estimateur de risque relatif de Kelsall et Diggle. Son interet
# majeur ici : comme le numerateur et le denominateur subissent la meme
# distribution de population sous-jacente, l'effet de la population S'ANNULE.
# Aucune donnee demographique externe n'est necessaire.

densite_elite <- density.ppp(
  semis, sigma = SIGMA, weights = villes_ok$NbEliteSeuil, edge = TRUE
)

# On masque les zones ou le denominateur est trop faible : le rapport y est
# numeriquement instable (division de deux quasi-zeros) et produirait des
# artefacts spectaculaires mais denues de sens.
#
# CRITERE DE FIABILITE : plutot qu'un percentile arbitraire, on exige un
# EFFECTIF MINIMAL de joueurs dans le voisinage du noyau. Pour un noyau
# gaussien d'ecart-type sigma, la masse effective couvre 2*pi*sigma^2 ; le
# nombre attendu de joueurs sous le noyau vaut donc densite * 2*pi*sigma^2.
# En exigeant au moins MIN_JOUEURS_NOYAU joueurs, on garantit que chaque pixel
# affiche repose sur un echantillon interpretable.
MIN_JOUEURS_NOYAU <- 30
masse_noyau <- 2 * pi * SIGMA^2          # en m2
seuil_fiabilite <- MIN_JOUEURS_NOYAU / masse_noyau   # en joueurs par m2

risque_relatif <- eval.im(
  ifelse(densite_joueurs >= seuil_fiabilite,
         densite_elite / densite_joueurs, NA)
)

# NOTE TECHNIQUE
# eval.im(ifelse(..., NA)) ne met pas des NA dans l'image : il RETRECIT la
# fenetre de l'image resultante. Il faut donc compter les pixels valides dans
# la matrice $v (et non via im[], qui ne renvoie que les pixels de la fenetre
# et ne contient jamais de NA).
n_pixels_retenus <- sum(!is.na(risque_relatif$v))
n_pixels_total   <- sum(!is.na(densite_joueurs$v))
part_pixels_retenus <- n_pixels_retenus / n_pixels_total

carte_risque <- carte_surface(
  risque_relatif,
  titre = paste0("Probabilite qu'un joueur devienne un joueur d'elite — ",
                 PAYS_ETUDE),
  sous_titre = paste0("Rapport de densites (Kelsall-Diggle) : joueurs de ",
                      SEUIL_ELITE, "+ points / tous les joueurs.",
                      "\nZones grises : trop peu de joueurs pour un rapport fiable."),
  legende = paste0("Part de joueurs\nde ", SEUIL_ELITE, "+ pts"),
  palette = "YlGnBu"
)

sauver_graphique(carte_risque, "03_carte_risque_relatif_elite.png",
                 largeur = 10, hauteur = 7)

part_globale <- sum(villes_ok$NbEliteSeuil) / sum(villes_ok$NbJoueurs)

jrn$ecrire("")
jrn$ecrire("--- Surface de risque relatif ---")
jrn$ecrire("Part globale de joueurs d'elite : ",
           round(part_globale * 100, 2), " %")
jrn$ecrire("Critere de fiabilite : au moins ", MIN_JOUEURS_NOYAU,
           " joueurs sous le noyau")
jrn$ecrire("Part du territoire cartographiee : ",
           round(part_pixels_retenus * 100, 1), " %")
jrn$ecrire("Etendue de la surface (zones fiables) : ",
           round(min(risque_relatif, na.rm = TRUE) * 100, 2), " % a ",
           round(max(risque_relatif, na.rm = TRUE) * 100, 2), " %")
jrn$ecrire("(a comparer a la moyenne nationale de ",
           round(part_globale * 100, 2), " %)")
jrn$ecrire("")
jrn$ecrire("LECTURE : cette carte repond a une question que le script")
jrn$ecrire("principal ne peut pas poser. Ses cartes de points totaux melangent")
jrn$ecrire("VOLUME et CALIBRE : une region apparait productive parce qu'elle")
jrn$ecrire("envoie beaucoup de joueurs, pas necessairement de meilleurs.")
jrn$ecrire("Ici, le volume est au denominateur : il est neutralise.")


# =============================================================================
# 4. FONCTION L DE RIPLEY
# =============================================================================
# La fonction K de Ripley compte le nombre moyen de voisins dans un rayon r.
# La fonction L = sqrt(K/pi) - r la stabilise : sous l'hypothese nulle d'un
# semis completement aleatoire (CSR), L(r) = 0 pour tout r.
#   L(r) > 0 -> regroupement a la distance r
#   L(r) < 0 -> repulsion / regularite
# L'enveloppe simule NB_SIM semis aleatoires et delimite l'intervalle
# compatible avec le hasard.

message("  Calcul de la fonction L de Ripley (", NB_SIM, " simulations)...")

set.seed(2026)
enveloppe_L <- envelope(
  semis, Lest, nsim = NB_SIM, verbose = FALSE,
  correction = "border"   # correction de bordure adaptee a une fenetre
)                         # aussi decoupee que le Canada

df_L <- as.data.frame(enveloppe_L) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(r_km = r / 1000)

graph_ripley <- ggplot(df_L, aes(x = r_km)) +
  geom_ribbon(aes(ymin = lo / 1000, ymax = hi / 1000,
                  fill = "Enveloppe du hasard"), alpha = 0.45) +
  geom_line(aes(y = theo / 1000, colour = "Hypothese nulle (CSR)"),
            linetype = "dashed", linewidth = 0.7) +
  geom_line(aes(y = obs / 1000, colour = "Semis observe"), linewidth = 0.9) +
  scale_fill_manual(values = c("Enveloppe du hasard" = "grey70")) +
  scale_colour_manual(values = c("Semis observe" = "#b2182b",
                                 "Hypothese nulle (CSR)" = "grey30")) +
  labs(
    title = paste0("Fonction L de Ripley — localites productrices de joueurs (",
                   PAYS_ETUDE, ")"),
    subtitle = paste0("Au-dessus de l'enveloppe = regroupement significatif. ",
                      NB_SIM, " simulations."),
    x = "Distance r (km)", y = "L(r) - r  (km)",
    colour = NULL, fill = NULL,
    caption = "Auteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

sauver_graphique(graph_ripley, "03_graph_fonction_ripley.png",
                 largeur = 9, hauteur = 6)

# Resume quantitatif : a quelles distances le regroupement est-il significatif ?
df_L <- df_L %>%
  mutate(
    Statut = case_when(
      obs > hi ~ "Regroupement significatif",
      obs < lo ~ "Repulsion significative",
      TRUE     ~ "Compatible avec le hasard"
    )
  )

resume_L <- df_L %>%
  filter(r > 0) %>%
  group_by(Statut) %>%
  summarise(
    NbDistances = n(),
    DistanceMin_km = round(min(r_km), 1),
    DistanceMax_km = round(max(r_km), 1),
    .groups = "drop"
  )

sauver_tableau(
  df_L %>% select(r_km, obs, theo, lo, hi, Statut),
  "03_table_fonction_ripley.csv"
)

jrn$capturer(as.data.frame(resume_L),
             "FONCTION L DE RIPLEY — plages de distance par statut")

# Ecart maximal a l'hypothese nulle
ligne_max <- df_L %>% filter(r > 0) %>% slice_max(obs - theo, n = 1)

jrn$ecrire("")
jrn$ecrire("Ecart maximal au hasard a r = ", round(ligne_max$r_km, 1), " km")
jrn$ecrire("  L(r) - r observe : ", round(ligne_max$obs / 1000, 2), " km")
jrn$ecrire("  borne haute de l'enveloppe : ", round(ligne_max$hi / 1000, 2), " km")

# Signalement automatique de l'artefact de grande distance
repulsion <- df_L %>% filter(Statut == "Repulsion significative", r > 0)
if (nrow(repulsion) > 0) {
  jrn$ecrire("")
  jrn$ecrire("ARTEFACT A SIGNALER")
  jrn$ecrire("Une 'repulsion significative' apparait entre ",
             round(min(repulsion$r_km), 0), " et ",
             round(max(repulsion$r_km), 0), " km.")
  jrn$ecrire("Il ne faut PAS l'interpreter comme un phenomene reel. Aux")
  jrn$ecrire("grandes distances, la fonction L est dominee par la GEOMETRIE de")
  jrn$ecrire("la fenetre : le Canada est un territoire tres allonge d'est en")
  jrn$ecrire("ouest, si bien que peu de paires de localites peuvent etre")
  jrn$ecrire("separees de 700 km dans toutes les directions. La correction de")
  jrn$ecrire("bordure atenue ce biais sans l'eliminer.")
  jrn$ecrire("Regle pratique : ne pas interpreter L(r) au-dela du quart de la")
  jrn$ecrire("plus petite dimension de la fenetre.")
}

jrn$ecrire("")
jrn$ecrire("MISE EN GARDE ESSENTIELLE SUR CE TEST")
jrn$ecrire("L'hypothese nulle CSR suppose que les localites productrices")
jrn$ecrire("pourraient se repartir UNIFORMEMENT sur le territoire canadien.")
jrn$ecrire("C'est evidemment faux : la population elle-meme est concentree")
jrn$ecrire("dans le sud du pays. Un rejet de CSR etait donc acquis d'avance et")
jrn$ecrire("ne demontre RIEN sur le hockey : il redemontre que les Canadiens")
jrn$ecrire("vivent en ville.")
jrn$ecrire("")
jrn$ecrire("Ce test est rapporte pour deux raisons legitimes :")
jrn$ecrire(" 1. il quantifie l'ECHELLE du regroupement (la distance a laquelle")
jrn$ecrire("    l'ecart au hasard est maximal), information utile en soi ;")
jrn$ecrire(" 2. il illustre une regle de methode : un test spatial ne vaut que")
jrn$ecrire("    ce que vaut son hypothese nulle.")
jrn$ecrire("")
jrn$ecrire("Un test rigoureux exigerait une hypothese nulle INHOMOGENE, calee")
jrn$ecrire("sur la densite de population (fonction Linhom avec une intensite")
jrn$ecrire("issue d'une grille de population). C'est la principale piste")
jrn$ecrire("d'amelioration de cette section.")
jrn$ecrire("")
jrn$ecrire("A l'inverse, la surface de risque relatif de la section 3 n'a PAS")
jrn$ecrire("ce defaut : le rapport de deux densites elimine l'effet de la")
jrn$ecrire("population. C'est le resultat le plus solide de ce module.")

jrn$fermer()
message("=== 03 termine ===")
