# =============================================================================
# 09_centrographie.R — Centre de gravité et dispersion dans le temps
# =============================================================================
# PROBLÈME TRAITÉ
# Le projet contient deux dimensions riches — l'ESPACE (lieux de naissance) et
# le TEMPS (décennie de naissance) — mais ne les croise jamais. Les graphiques
# temporels ignorent la géographie ; les cartes ignorent le temps.
#
# La centrographie comble exactement ce vide : elle résume un semis de points
# par quelques mesures (centre, dispersion) que l'on peut alors suivre d'une
# décennie à l'autre. Une seule figure raconte le déplacement du "centre de
# gravité" du hockey sur plus d'un siècle.
#
# DEUX ÉCHELLES
#  A. Monde : le centre se déplace-t-il vers l'Europe à mesure que la ligue
#     s'internationalise ?
#  B. Canada seul : y a-t-il une dérive interne vers l'Ouest ?
#
# NOTE MÉTHODOLOGIQUE IMPORTANTE
# À l'échelle mondiale, on ne peut PAS faire la moyenne de coordonnées en
# degrés : la longitude est cyclique et un degré ne représente pas la même
# distance selon la latitude. Le centre moyen est donc calculé sur la SPHÈRE,
# en passant par des vecteurs cartésiens 3D. C'est la méthode correcte pour des
# points dispersés sur plusieurs continents.
#
# SORTIES : 3 figures, 2 tableaux, 1 journal de résultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 09 — CENTROGRAPHIE TEMPORELLE ===")

MIN_JOUEURS_DECENNIE <- 30   # sous ce seuil, un centre n'a aucune stabilité

jrn <- journal_resultats("09_resultats_centrographie.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 09. CENTROGRAPHIE TEMPORELLE")
jrn$ecrire("=====================================================")

hockey         <- charger_hockey()
lieux_geocodes <- charger_lieux_geocodes()

# Données au niveau du JOUEUR, chacun porte les coordonnées de sa ville.
# Contrairement au module 08, la duplication des coordonnées est ici SANS
# problème : on calcule des moyennes pondérées, pas des distances entre paires.
joueurs_geo <- hockey |>
  left_join(lieux_geocodes, by = "Birthplace") |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(Decennie))

jrn$ecrire("Joueurs géolocalisés et dates : ", nrow(joueurs_geo),
           " sur ", nrow(hockey))


# =============================================================================
# FONCTIONS DE CENTROGRAPHIE
# =============================================================================

# --- Centre moyen sphérique -------------------------------------------------
# Chaque point (lat, lon) devient un vecteur unitaire 3D. On fait la moyenne
# pondérée des vecteurs, puis on reprojette le résultat sur la sphère.
centre_spherique <- function(lat, lon, poids = NULL) {
  if (is.null(poids)) poids <- rep(1, length(lat))
  phi    <- lat * pi / 180
  lambda <- lon * pi / 180

  x <- sum(poids * cos(phi) * cos(lambda))
  y <- sum(poids * cos(phi) * sin(lambda))
  z <- sum(poids * sin(phi))

  norme <- sqrt(x^2 + y^2 + z^2)
  if (norme == 0) return(c(latitude = NA, longitude = NA))

  c(
    latitude  = asin(z / norme) * 180 / pi,
    longitude = atan2(y, x) * 180 / pi
  )
}

# --- Distance standard géodésique -------------------------------------------
# "Distance standard" est le terme employé par le manuel (section 3.2.2).
# C'est l'équivalent spatial de l'écart-type : racine de la moyenne des carrés
# des distances au centre moyen. Elle mesure la DISPERSION du semis, en km.
distance_type <- function(lat, lon, centre) {
  pts <- st_as_sf(
    data.frame(lat = lat, lon = lon),
    coords = c("lon", "lat"), crs = CRS_GEO
  )
  ctr <- st_sfc(st_point(c(centre[["longitude"]], centre[["latitude"]])),
                crs = CRS_GEO)
  # st_distance en CRS géographique renvoie des distances géodésiques (m)
  d <- as.numeric(st_distance(pts, ctr))
  sqrt(mean(d^2)) / 1000   # en km
}


# --- Ellipse de déviation standard ------------------------------------------
# Manuel, section 3.2.2 : troisième paramètre de dispersion, après la distance
# standard des X / des Y et le cercle de distance standard. Contrairement au
# cercle, l'ellipse a une ORIENTATION : elle révèle l'axe le long duquel le
# semis s'étire. Pour le hockey canadien, cet axe devrait suivre le corridor
# habité est-ouest, et c'est précisément ce qu'on veut montrer.
#
# Formulation retenue : celle de CrimeStat (Levine, 2006 ; équations 3.14 et
# 3.15 du manuel).
#
# MISE EN GARDE, reprise telle quelle du manuel : il existe plusieurs
# définitions de l'ellipse (Yuill, ArcGIS Pro, CrimeStat, correction de Wang).
# Toutes donnent le même CENTRE et le même ANGLE, mais des TAILLES
# différentes. Il ne faut donc jamais comparer une ellipse produite ici avec
# une ellipse produite par ArcGIS ou QGIS. À l'intérieur de ce module, toutes
# les ellipses viennent de la même formule : elles sont comparables entre
# elles, ce qui est le seul usage qu'on en fait.
ellipse_deviation_standard <- function(x, y, n_sommets = 180) {
  n  <- length(x)
  if (n < 3) return(NULL)
  cx <- mean(x)
  cy <- mean(y)
  xd <- x - cx
  yd <- y - cy

  somme_xd2 <- sum(xd^2)
  somme_yd2 <- sum(yd^2)
  somme_xy  <- sum(xd * yd)

  # Angle de rotation de l'ellipse (équation 3.14)
  if (abs(somme_xy) < .Machine$double.eps) {
    theta <- 0
  } else {
    numerateur <- (somme_xd2 - somme_yd2) +
      sqrt((somme_xd2 - somme_yd2)^2 + 4 * somme_xy^2)
    theta <- atan(numerateur / (2 * somme_xy))
  }

  # Demi-axes (équation 3.15).
  # Les deux directions de l'ellipse dans le plan de la carte sont
  #   u = ( cos(theta), sin(theta) )  et  v = ( -sin(theta), cos(theta) ).
  # On projette les écarts au centre sur CES directions-là, exactement celles
  # qui servent ensuite à tracer le contour. Les formules imprimées dans le
  # manuel supposent une convention de rotation qui n'est pas explicitée ;
  # projeter soi-même garantit que les demi-axes calculés et l'ellipse tracée
  # décrivent bien la même chose.
  u <- xd * cos(theta) + yd * sin(theta)
  v <- -xd * sin(theta) + yd * cos(theta)

  sigma_x <- sqrt(2) * sqrt(sum(u^2) / (n - 2))
  sigma_y <- sqrt(2) * sqrt(sum(v^2) / (n - 2))

  # Échantillonnage du contour, puis rotation de theta
  t <- seq(0, 2 * pi, length.out = n_sommets + 1)
  ex <- sigma_x * cos(t)
  ey <- sigma_y * sin(t)

  contour <- cbind(
    cx + ex * cos(theta) - ey * sin(theta),
    cy + ex * sin(theta) + ey * cos(theta)
  )
  # st_polygon exige un anneau FERMÉ au bit près. cos(2*pi) et cos(0) ne sont
  # pas exactement égaux en virgule flottante : on recopie donc explicitement
  # le premier sommet en dernier plutôt que de compter sur l'arithmétique.
  contour[nrow(contour), ] <- contour[1, ]

  # Azimut du GRAND axe, en degrés depuis le nord (lecture cartographique).
  # Le grand axe n'est pas toujours "u" : si la dispersion est plus forte dans
  # la direction perpendiculaire, c'est "v" qu'il faut décrire. Sans ce test,
  # l'azimut est décalé de 90 degrés une fois sur deux.
  direction <- if (sigma_x >= sigma_y) {
    c(cos(theta), sin(theta))
  } else {
    c(-sin(theta), cos(theta))
  }
  azimut <- (atan2(direction[1], direction[2]) * 180 / pi) %% 180

  list(
    centre  = c(x = cx, y = cy),
    theta   = theta,
    sigma_x = sigma_x,
    sigma_y = sigma_y,
    azimut  = azimut,
    contour = contour
  )
}


# =============================================================================
# PARTIE A — CENTRE DE GRAVITÉ MONDIAL PAR DÉCENNIE
# =============================================================================

decennies_valides <- joueurs_geo |>
  count(Decennie) |>
  filter(n >= MIN_JOUEURS_DECENNIE) |>
  pull(Decennie)

jrn$ecrire("Décennies retenues (>= ", MIN_JOUEURS_DECENNIE, " joueurs) : ",
           paste(range(decennies_valides), collapse = " à "))

centres_monde <- lapply(sort(decennies_valides), function(dec) {
  sous <- joueurs_geo |> filter(Decennie == dec)
  ctr  <- centre_spherique(sous$latitude, sous$longitude)
  tibble::tibble(
    Decennie   = dec,
    NbJoueurs  = nrow(sous),
    latitude   = ctr[["latitude"]],
    longitude  = ctr[["longitude"]],
    DistanceType_km = distance_type(sous$latitude, sous$longitude, ctr),
    PartEurope = mean(sous$GroupeGeo == "Europe") * 100,
    PartCanada = mean(sous$GroupeGeo == "Canada") * 100
  )
}) |> bind_rows()

# Déplacement d'une décennie à la suivante
centres_monde <- centres_monde |>
  mutate(
    Deplacement_km = c(NA, sapply(2:n(), function(i) {
      p1 <- st_sfc(st_point(c(longitude[i - 1], latitude[i - 1])), crs = CRS_GEO)
      p2 <- st_sfc(st_point(c(longitude[i],     latitude[i])),     crs = CRS_GEO)
      as.numeric(st_distance(p1, p2)) / 1000
    }))
  )

sauver_tableau(centres_monde, "09_table_centres_monde.csv")

jrn$capturer(
  centres_monde |>
    mutate(across(c(latitude, longitude, DistanceType_km,
                    PartEurope, PartCanada, Deplacement_km),
                  ~ round(.x, 2))) |>
    as.data.frame(),
  "CENTRE DE GRAVITÉ MONDIAL PAR DÉCENNIE DE NAISSANCE"
)

# Bilan du déplacement total
depart  <- centres_monde |> slice_head(n = 1)
arrivee <- centres_monde |> slice_tail(n = 1)
deplacement_total <- as.numeric(st_distance(
  st_sfc(st_point(c(depart$longitude,  depart$latitude)),  crs = CRS_GEO),
  st_sfc(st_point(c(arrivee$longitude, arrivee$latitude)), crs = CRS_GEO)
)) / 1000

jrn$ecrire("")
jrn$ecrire("--- Bilan mondial ---")
jrn$ecrire("Décennie ", depart$Decennie, " : ",
           round(depart$latitude, 2), " N, ", round(depart$longitude, 2), " E",
           "  (", round(depart$PartEurope, 1), " % d'Européens)")
jrn$ecrire("Décennie ", arrivee$Decennie, " : ",
           round(arrivee$latitude, 2), " N, ", round(arrivee$longitude, 2), " E",
           "  (", round(arrivee$PartEurope, 1), " % d'Européens)")
jrn$ecrire("Déplacement net du centre : ", round(deplacement_total), " km")
jrn$ecrire("Dispersion (distance-type) : de ",
           round(depart$DistanceType_km), " km à ",
           round(arrivee$DistanceType_km), " km")

sens_lon <- ifelse(arrivee$longitude > depart$longitude, "vers l'EST",
                   "vers l'OUEST")
jrn$ecrire("Sens du déplacement en longitude : ", sens_lon)
jrn$ecrire("")
jrn$ecrire("INTERPRETATION")
jrn$ecrire("Le centre de gravité se déplace ", sens_lon, " et la dispersion")
if (arrivee$DistanceType_km > depart$DistanceType_km) {
  jrn$ecrire("AUGMENTE : le bassin de recrutement de la LNH s'élargit")
  jrn$ecrire("géographiquement. Les deux mesures traduisent le même")
  jrn$ecrire("phénomène : l'internationalisation de la ligue.")
} else {
  jrn$ecrire("DIMINUE : le bassin de recrutement se resserre.")
}
jrn$ecrire("")
jrn$ecrire("ATTENTION À UNE FAUSSE LECTURE")
jrn$ecrire("Le centre de gravité est une moyenne : il tombe dans l'Atlantique,")
jrn$ecrire("là où personne ne naît. Il ne désigne donc AUCUN lieu réel. Seuls")
jrn$ecrire("son DÉPLACEMENT et la distance-type ont un sens ici.")


# --- Carte A : trajectoire du centre de gravité mondial ---------------------
# Projection azimutale équivalente centrée sur l'Atlantique Nord : elle place
# le Canada et l'Europe dans le même champ avec une déformation limitée.
# Projection retenue : voir CRS_ATL dans 00_config.R

monde <- ne_countries(scale = "medium", returnclass = "sf")

centres_sf <- centres_monde |>
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO, remove = FALSE) |>
  st_transform(CRS_ATL)

# Trajectoire reliant les centres successifs
trajectoire <- centres_monde |>
  arrange(Decennie) |>
  select(longitude, latitude) |>
  as.matrix() |>
  st_linestring() |>
  st_sfc(crs = CRS_GEO) |>
  # Segmentation : sans cela, la ligne serait tracée "droite" dans la
  # projection au lieu de suivre le trajet géodésique
  st_segmentize(units::set_units(50, km)) |>
  st_transform(CRS_ATL)

coords_centres <- st_coordinates(centres_sf)
centres_monde_proj <- centres_monde |>
  mutate(X = coords_centres[, 1], Y = coords_centres[, 2])

emprise_atl <- st_bbox(st_buffer(centres_sf, 2200000))

carte_trajectoire <- ggplot() +
  geom_sf(data = st_transform(monde, CRS_ATL),
          fill = "grey93", colour = "grey70", linewidth = 0.2) +
  geom_sf(data = trajectoire, colour = "#b2182b", linewidth = 0.8,
          alpha = 0.8) +
  geom_point(data = centres_monde_proj,
             aes(x = X, y = Y, size = NbJoueurs, fill = Decennie),
             shape = 21, colour = "white", stroke = 0.6) +
  ggrepel::geom_text_repel(
    data = centres_monde_proj,
    aes(x = X, y = Y, label = Decennie),
    size = 3, min.segment.length = 0, segment.colour = "grey50",
    max.overlaps = Inf, seed = 2026
  ) +
  scale_fill_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(2, 9), labels = scales::comma) +
  coord_sf(xlim = emprise_atl[c("xmin", "xmax")],
           ylim = emprise_atl[c("ymin", "ymax")], expand = FALSE) +
  labs(
    title = "Déplacement du centre de gravité des naissances de joueurs de la LNH",
    subtitle = paste0("Centre moyen sphérique pondéré, par décennie de ",
                      "naissance (", min(centres_monde$Decennie), "-",
                      max(centres_monde$Decennie), ")"),
    size = "Joueurs nés\ndans la décennie",
    fill = "Décennie", x = NULL, y = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))

sauver_graphique(carte_trajectoire, "09_carte_trajectoire_centre_monde.png",
                 largeur = 10, hauteur = 8)


# --- Graphique A2 : longitude et dispersion dans le temps -------------------
# Version non cartographique, souvent plus convaincante dans un rapport parce
# qu'elle montre la tendance sans ambiguïté de lecture.

donnees_tendance <- centres_monde |>
  select(Decennie, `Longitude du centre (degrés)` = longitude,
         `Distance-type (km)` = DistanceType_km,
         `Part de joueurs européens (%)` = PartEurope) |>
  pivot_longer(-Decennie, names_to = "Indicateur", values_to = "Valeur")

graph_tendance <- ggplot(donnees_tendance,
                         aes(x = Decennie, y = Valeur)) +
  geom_line(colour = "#2c7fb8", linewidth = 0.8) +
  geom_point(colour = "#2c7fb8", size = 1.8) +
  facet_wrap(~ Indicateur, scales = "free_y", ncol = 1) +
  labs(
    title = "Internationalisation de la LNH : trois indicateurs concordants",
    subtitle = paste("Par décennie de naissance des joueurs.",
                     "La longitude du centre et la dispersion évoluent",
                     "\nde pair avec la part de joueurs européens."),
    x = "Décennie de naissance", y = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

sauver_graphique(graph_tendance, "09_graph_tendances_centre.png",
                 largeur = 9, hauteur = 8)


# =============================================================================
# PARTIE B — DÉRIVE INTERNE AU CANADA
# =============================================================================
# Le signal mondial est dominé par l'arrivée des Européens. En se restreignant
# au Canada, on isole une question différente : à l'intérieur d'un même pays,
# le foyer du hockey s'est-il déplacé ?
#
# Ici, une projection plane (Lambert de Statistique Canada) est appropriée :
# l'emprise est assez petite pour que les distances planes soient fiables. On
# peut donc ajouter un CERCLE DE DISTANCE-TYPE, impossible à tracer proprement
# à l'échelle mondiale.

joueurs_ca <- joueurs_geo |> filter(Country == "Canada")

decennies_ca <- joueurs_ca |>
  count(Decennie) |>
  filter(n >= MIN_JOUEURS_DECENNIE) |>
  pull(Decennie)

joueurs_ca_sf <- joueurs_ca |>
  filter(Decennie %in% decennies_ca) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO) |>
  st_transform(CRS_CA)

coords_ca <- st_coordinates(joueurs_ca_sf)
joueurs_ca_plan <- joueurs_ca_sf |>
  st_drop_geometry() |>
  mutate(X = coords_ca[, 1], Y = coords_ca[, 2])

# On passe par group_modify plutôt que summarise : la distance-type a besoin du
# centre ET de tous les points individuels du groupe en même temps, ce qu'un
# summarise ne permet pas d'exprimer sans réutiliser une colonne déjà agrégée.
centres_ca <- joueurs_ca_plan |>
  group_by(Decennie) |>
  group_modify(~ {
    cx <- mean(.x$X); cy <- mean(.x$Y)
    tibble::tibble(
      NbJoueurs = nrow(.x),
      X = cx, Y = cy,
      DistanceType_km = sqrt(mean((.x$X - cx)^2 + (.x$Y - cy)^2)) / 1000
    )
  }) |>
  ungroup()

# Retour en coordonnées géographiques pour le tableau de sortie
centres_ca_geo <- centres_ca |>
  st_as_sf(coords = c("X", "Y"), crs = CRS_CA, remove = FALSE) |>
  st_transform(CRS_GEO)

coords_geo_ca <- st_coordinates(centres_ca_geo)
centres_ca <- centres_ca |>
  mutate(longitude = coords_geo_ca[, 1],
         latitude  = coords_geo_ca[, 2])

sauver_tableau(centres_ca, "09_table_centres_canada.csv")

jrn$ecrire("")
jrn$ecrire("-----------------------------------------------------")
jrn$ecrire(" DÉRIVE INTERNE AU CANADA")
jrn$ecrire("-----------------------------------------------------")

jrn$capturer(
  centres_ca |>
    mutate(across(c(DistanceType_km, longitude, latitude), ~ round(.x, 2))) |>
    select(Decennie, NbJoueurs, longitude, latitude, DistanceType_km) |>
    as.data.frame(),
  "CENTRE DE GRAVITÉ DES JOUEURS CANADIENS PAR DÉCENNIE"
)

depart_ca  <- centres_ca |> slice_head(n = 1)
arrivee_ca <- centres_ca |> slice_tail(n = 1)
derive_ca  <- sqrt((arrivee_ca$X - depart_ca$X)^2 +
                   (arrivee_ca$Y - depart_ca$Y)^2) / 1000

jrn$ecrire("")
jrn$ecrire("Déplacement net du centre canadien : ", round(derive_ca), " km")
jrn$ecrire("Longitude : de ", round(depart_ca$longitude, 2), " à ",
           round(arrivee_ca$longitude, 2), " degrés")
sens_ca <- ifelse(arrivee_ca$longitude < depart_ca$longitude,
                  "vers l'OUEST", "vers l'EST")
jrn$ecrire("Sens : ", sens_ca)
jrn$ecrire("Dispersion : de ", round(depart_ca$DistanceType_km), " km à ",
           round(arrivee_ca$DistanceType_km), " km")

# Test de tendance : la dérive longitudinale est-elle systématique ?
if (nrow(centres_ca) >= 4) {
  tendance <- cor.test(centres_ca$Decennie, centres_ca$longitude,
                       method = "spearman", exact = FALSE)
  jrn$capturer(tendance,
               "TEST DE TENDANCE — corrélation de Spearman entre décennie et longitude du centre")
  jrn$ecrire("Une corrélation négative significative confirmerait une dérive")
  jrn$ecrire("régulière vers l'ouest, et non un simple va-et-vient.")
}


# --- Ellipses de déviation standard par décennie ----------------------------
# Le cercle de distance standard ne dit rien de l'ORIENTATION du semis.
# L'ellipse, elle, donne l'axe d'étirement et son azimut : c'est le troisième
# paramètre de dispersion du manuel (section 3.2.2).

ellipses_ca <- lapply(sort(unique(joueurs_ca_plan$Decennie)), function(dec) {
  sous <- joueurs_ca_plan |> filter(Decennie == dec)
  ell  <- ellipse_deviation_standard(sous$X, sous$Y)
  if (is.null(ell)) return(NULL)
  list(
    decennie = dec,
    resume = tibble::tibble(
      Decennie      = dec,
      NbJoueurs     = nrow(sous),
      GrandAxe_km   = round(max(ell$sigma_x, ell$sigma_y) / 1000, 1),
      PetitAxe_km   = round(min(ell$sigma_x, ell$sigma_y) / 1000, 1),
      Aplatissement = round(min(ell$sigma_x, ell$sigma_y) /
                              max(ell$sigma_x, ell$sigma_y), 3),
      Azimut_deg    = round(ell$azimut, 1)
    ),
    geometrie = st_sfc(st_polygon(list(ell$contour)), crs = CRS_CA)
  )
})
ellipses_ca <- Filter(Negate(is.null), ellipses_ca)

table_ellipses <- bind_rows(lapply(ellipses_ca, `[[`, "resume"))
sauver_tableau(table_ellipses, "09_table_ellipses_canada.csv")

jrn$capturer(as.data.frame(table_ellipses),
             "ELLIPSES DE DÉVIATION STANDARD PAR DÉCENNIE (Canada)")
jrn$ecrire("LECTURE : l'azimut est l'orientation du GRAND axe, en degrés")
jrn$ecrire("depuis le nord. Une valeur proche de 90 signale un semis étiré")
jrn$ecrire("est-ouest ; l'aplatissement (petit axe / grand axe) dit à quel")
jrn$ecrire("point cet étirement est marqué : plus il est proche de 0, plus le")
jrn$ecrire("semis est allongé, plus il est proche de 1, plus il est circulaire.")
jrn$ecrire("")
jrn$ecrire("MISE EN GARDE (manuel, section 3.2.2) : la TAILLE d'une ellipse")
jrn$ecrire("dépend de la formule employée (Yuill, ArcGIS Pro, CrimeStat...).")
jrn$ecrire("Celle utilisée ici est celle de CrimeStat. Les ellipses de ce")
jrn$ecrire("tableau sont comparables entre elles, mais PAS avec des ellipses")
jrn$ecrire("produites par un autre logiciel.")

ellipses_sf <- st_sf(
  Decennie = vapply(ellipses_ca, function(e) e$decennie, numeric(1)),
  geometry = do.call(c, lapply(ellipses_ca, function(e) e$geometrie))
)

# --- Carte B : centres canadiens et cercles de dispersion -------------------

canada_contour <- ne_countries(country = "Canada", scale = "medium",
                              returnclass = "sf") |>
  st_transform(CRS_CA)

centres_ca_sf <- centres_ca |>
  st_as_sf(coords = c("X", "Y"), crs = CRS_CA, remove = FALSE)

# Cercles de distance standard : rayon = dispersion de la décennie
cercles_ca <- centres_ca_sf |>
  st_buffer(dist = centres_ca$DistanceType_km * 1000)

# On ne trace que la première et la dernière décennie pour rester lisible
decennies_extremes <- range(centres_ca$Decennie)

cercles_extremes <- cercles_ca |>
  filter(Decennie %in% decennies_extremes)

ellipses_extremes <- ellipses_sf |>
  filter(Decennie %in% decennies_extremes)

trajectoire_ca <- centres_ca |>
  arrange(Decennie) |>
  select(X, Y) |>
  as.matrix() |>
  st_linestring() |>
  st_sfc(crs = CRS_CA)

emprise_ca <- st_bbox(st_buffer(centres_ca_sf, 1500000))

carte_centres_ca <- ggplot() +
  geom_sf(data = canada_contour, fill = "grey95", colour = "grey65",
          linewidth = 0.25) +
  geom_sf(data = cercles_extremes,
          aes(colour = factor(Decennie)), fill = NA,
          linetype = "dashed", linewidth = 0.5) +
  # Ellipses de déviation standard : elles ajoutent l'ORIENTATION du semis,
  # que le cercle ne peut pas montrer.
  geom_sf(data = ellipses_extremes,
          aes(colour = factor(Decennie)), fill = NA,
          linetype = "solid", linewidth = 0.6) +
  geom_sf(data = trajectoire_ca, colour = "#b2182b", linewidth = 0.9) +
  geom_point(data = centres_ca, aes(x = X, y = Y, fill = Decennie),
             shape = 21, size = 3.4, colour = "white", stroke = 0.6) +
  ggrepel::geom_text_repel(
    data = centres_ca, aes(x = X, y = Y, label = Decennie),
    size = 3, min.segment.length = 0, segment.colour = "grey50",
    max.overlaps = Inf, seed = 2026
  ) +
  scale_fill_viridis_c(option = "plasma") +
  scale_colour_manual(values = c("#3b528b", "#f89540"),
                      name = "Dispersion\n(cercle et ellipse)") +
  coord_sf(xlim = emprise_ca[c("xmin", "xmax")],
           ylim = emprise_ca[c("ymin", "ymax")], expand = FALSE) +
  labs(
    title = "Dérive du centre de gravité des joueurs canadiens",
    subtitle = paste0("Centre moyen par décennie de naissance. Pour la ",
                      "première et la dernière décennie :\ncercle de distance ",
                      "standard (pointillé) et ellipse de déviation standard ",
                      "(trait plein)."),
    fill = "Décennie", x = NULL, y = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))

sauver_graphique(carte_centres_ca, "09_carte_trajectoire_centre_canada.png",
                 largeur = 10, hauteur = 8)

jrn$fermer()
message("=== 09 terminé ===")
