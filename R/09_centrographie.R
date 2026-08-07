# =============================================================================
# 09_centrographie.R — Centre de gravite et dispersion dans le temps
# =============================================================================
# PROBLEME ADRESSE
# Le projet contient deux dimensions riches — l'ESPACE (lieux de naissance) et
# le TEMPS (decennie de naissance) — mais ne les croise jamais. Les graphiques
# temporels ignorent la geographie ; les cartes ignorent le temps.
#
# La centrographie comble exactement ce vide : elle resume un semis de points
# par quelques mesures (centre, dispersion) que l'on peut alors suivre d'une
# decennie a l'autre. Une seule figure raconte le deplacement du "centre de
# gravite" du hockey sur plus d'un siecle.
#
# DEUX ECHELLES
#  A. Monde : le centre se deplace-t-il vers l'Europe a mesure que la ligue
#     s'internationalise ?
#  B. Canada seul : y a-t-il une derive interne vers l'Ouest ?
#
# NOTE METHODOLOGIQUE IMPORTANTE
# A l'echelle mondiale, on ne peut PAS faire la moyenne de coordonnees en
# degres : la longitude est cyclique et un degre ne represente pas la meme
# distance selon la latitude. Le centre moyen est donc calcule sur la SPHERE,
# en passant par des vecteurs cartesiens 3D. C'est la methode correcte pour des
# points disperses sur plusieurs continents.
#
# SORTIES : 3 figures, 2 tableaux, 1 journal de resultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 09 — CENTROGRAPHIE TEMPORELLE ===")

MIN_JOUEURS_DECENNIE <- 30   # sous ce seuil, un centre n'a aucune stabilite

jrn <- journal_resultats("09_resultats_centrographie.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 09. CENTROGRAPHIE TEMPORELLE")
jrn$ecrire("=====================================================")

hockey         <- charger_hockey()
lieux_geocodes <- charger_lieux_geocodes()

# Donnees au niveau du JOUEUR, chacun porte les coordonnees de sa ville.
# Contrairement au module 08, la duplication des coordonnees est ici SANS
# probleme : on calcule des moyennes ponderees, pas des distances entre paires.
joueurs_geo <- hockey |>
  left_join(lieux_geocodes, by = "Birthplace") |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(Decennie))

jrn$ecrire("Joueurs geolocalises et dates : ", nrow(joueurs_geo),
           " sur ", nrow(hockey))


# =============================================================================
# FONCTIONS DE CENTROGRAPHIE
# =============================================================================

# --- Centre moyen spherique -------------------------------------------------
# Chaque point (lat, lon) devient un vecteur unitaire 3D. On fait la moyenne
# ponderee des vecteurs, puis on reprojette le resultat sur la sphere.
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

# --- Distance standard geodesique -------------------------------------------
# "Distance standard" est le terme employe par le manuel (section 3.2.2).
# C'est l'equivalent spatial de l'ecart-type : racine de la moyenne des carres
# des distances au centre moyen. Elle mesure la DISPERSION du semis, en km.
distance_type <- function(lat, lon, centre) {
  pts <- st_as_sf(
    data.frame(lat = lat, lon = lon),
    coords = c("lon", "lat"), crs = CRS_GEO
  )
  ctr <- st_sfc(st_point(c(centre[["longitude"]], centre[["latitude"]])),
                crs = CRS_GEO)
  # st_distance en CRS geographique renvoie des distances geodesiques (m)
  d <- as.numeric(st_distance(pts, ctr))
  sqrt(mean(d^2)) / 1000   # en km
}


# --- Ellipse de deviation standard ------------------------------------------
# Manuel, section 3.2.2 : troisieme parametre de dispersion, apres la distance
# standard des X / des Y et le cercle de distance standard. Contrairement au
# cercle, l'ellipse a une ORIENTATION : elle revele l'axe le long duquel le
# semis s'etire. Pour le hockey canadien, cet axe devrait suivre le corridor
# habite est-ouest, et c'est precisement ce qu'on veut montrer.
#
# Formulation retenue : celle de CrimeStat (Levine, 2006 ; equations 3.14 et
# 3.15 du manuel).
#
# MISE EN GARDE, reprise telle quelle du manuel : il existe plusieurs
# definitions de l'ellipse (Yuill, ArcGIS Pro, CrimeStat, correction de Wang).
# Toutes donnent le meme CENTRE et le meme ANGLE, mais des TAILLES
# differentes. Il ne faut donc jamais comparer une ellipse produite ici avec
# une ellipse produite par ArcGIS ou QGIS. A l'interieur de ce module, toutes
# les ellipses viennent de la meme formule : elles sont comparables entre
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

  # Angle de rotation de l'ellipse (equation 3.14)
  if (abs(somme_xy) < .Machine$double.eps) {
    theta <- 0
  } else {
    numerateur <- (somme_xd2 - somme_yd2) +
      sqrt((somme_xd2 - somme_yd2)^2 + 4 * somme_xy^2)
    theta <- atan(numerateur / (2 * somme_xy))
  }

  # Demi-axes (equation 3.15).
  # Les deux directions de l'ellipse dans le plan de la carte sont
  #   u = ( cos(theta), sin(theta) )  et  v = ( -sin(theta), cos(theta) ).
  # On projette les ecarts au centre sur CES directions-la, exactement celles
  # qui servent ensuite a tracer le contour. Les formules imprimees dans le
  # manuel supposent une convention de rotation qui n'est pas explicitee ;
  # projeter soi-meme garantit que les demi-axes calcules et l'ellipse tracee
  # decrivent bien la meme chose.
  u <- xd * cos(theta) + yd * sin(theta)
  v <- -xd * sin(theta) + yd * cos(theta)

  sigma_x <- sqrt(2) * sqrt(sum(u^2) / (n - 2))
  sigma_y <- sqrt(2) * sqrt(sum(v^2) / (n - 2))

  # Echantillonnage du contour, puis rotation de theta
  t <- seq(0, 2 * pi, length.out = n_sommets + 1)
  ex <- sigma_x * cos(t)
  ey <- sigma_y * sin(t)

  contour <- cbind(
    cx + ex * cos(theta) - ey * sin(theta),
    cy + ex * sin(theta) + ey * cos(theta)
  )
  # st_polygon exige un anneau FERME au bit pres. cos(2*pi) et cos(0) ne sont
  # pas exactement egaux en virgule flottante : on recopie donc explicitement
  # le premier sommet en dernier plutot que de compter sur l'arithmetique.
  contour[nrow(contour), ] <- contour[1, ]

  # Azimut du GRAND axe, en degres depuis le nord (lecture cartographique).
  # Le grand axe n'est pas toujours "u" : si la dispersion est plus forte dans
  # la direction perpendiculaire, c'est "v" qu'il faut decrire. Sans ce test,
  # l'azimut est decale de 90 degres une fois sur deux.
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
# PARTIE A — CENTRE DE GRAVITE MONDIAL PAR DECENNIE
# =============================================================================

decennies_valides <- joueurs_geo |>
  count(Decennie) |>
  filter(n >= MIN_JOUEURS_DECENNIE) |>
  pull(Decennie)

jrn$ecrire("Decennies retenues (>= ", MIN_JOUEURS_DECENNIE, " joueurs) : ",
           paste(range(decennies_valides), collapse = " a "))

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

# Deplacement d'une decennie a la suivante
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
  "CENTRE DE GRAVITE MONDIAL PAR DECENNIE DE NAISSANCE"
)

# Bilan du deplacement total
depart  <- centres_monde |> slice_head(n = 1)
arrivee <- centres_monde |> slice_tail(n = 1)
deplacement_total <- as.numeric(st_distance(
  st_sfc(st_point(c(depart$longitude,  depart$latitude)),  crs = CRS_GEO),
  st_sfc(st_point(c(arrivee$longitude, arrivee$latitude)), crs = CRS_GEO)
)) / 1000

jrn$ecrire("")
jrn$ecrire("--- Bilan mondial ---")
jrn$ecrire("Decennie ", depart$Decennie, " : ",
           round(depart$latitude, 2), " N, ", round(depart$longitude, 2), " E",
           "  (", round(depart$PartEurope, 1), " % d'Europeens)")
jrn$ecrire("Decennie ", arrivee$Decennie, " : ",
           round(arrivee$latitude, 2), " N, ", round(arrivee$longitude, 2), " E",
           "  (", round(arrivee$PartEurope, 1), " % d'Europeens)")
jrn$ecrire("Deplacement net du centre : ", round(deplacement_total), " km")
jrn$ecrire("Dispersion (distance-type) : de ",
           round(depart$DistanceType_km), " km a ",
           round(arrivee$DistanceType_km), " km")

sens_lon <- ifelse(arrivee$longitude > depart$longitude, "vers l'EST",
                   "vers l'OUEST")
jrn$ecrire("Sens du deplacement en longitude : ", sens_lon)
jrn$ecrire("")
jrn$ecrire("INTERPRETATION")
jrn$ecrire("Le centre de gravite se deplace ", sens_lon, " et la dispersion")
if (arrivee$DistanceType_km > depart$DistanceType_km) {
  jrn$ecrire("AUGMENTE : le bassin de recrutement de la LNH s'elargit")
  jrn$ecrire("geographiquement. Les deux mesures traduisent le meme")
  jrn$ecrire("phenomene : l'internationalisation de la ligue.")
} else {
  jrn$ecrire("DIMINUE : le bassin de recrutement se resserre.")
}
jrn$ecrire("")
jrn$ecrire("ATTENTION A UNE FAUSSE LECTURE")
jrn$ecrire("Le centre de gravite est une moyenne : il tombe dans l'Atlantique,")
jrn$ecrire("la ou personne ne nait. Il ne designe donc AUCUN lieu reel. Seuls")
jrn$ecrire("son DEPLACEMENT et la distance-type ont un sens ici.")


# --- Carte A : trajectoire du centre de gravite mondial ---------------------
# Projection azimutale equivalente centree sur l'Atlantique Nord : elle place
# le Canada et l'Europe dans le meme champ avec une deformation limitee.
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
  # Segmentation : sans cela, la ligne serait tracee "droite" dans la
  # projection au lieu de suivre le trajet geodesique
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
    title = "Deplacement du centre de gravite des naissances de joueurs de la LNH",
    subtitle = paste0("Centre moyen spherique pondere, par decennie de ",
                      "naissance (", min(centres_monde$Decennie), "-",
                      max(centres_monde$Decennie), ")"),
    size = "Joueurs nes\ndans la decennie",
    fill = "Decennie", x = NULL, y = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))

sauver_graphique(carte_trajectoire, "09_carte_trajectoire_centre_monde.png",
                 largeur = 10, hauteur = 8)


# --- Graphique A2 : longitude et dispersion dans le temps -------------------
# Version non cartographique, souvent plus convaincante dans un rapport parce
# qu'elle montre la tendance sans ambiguite de lecture.

donnees_tendance <- centres_monde |>
  select(Decennie, `Longitude du centre (degres)` = longitude,
         `Distance-type (km)` = DistanceType_km,
         `Part de joueurs europeens (%)` = PartEurope) |>
  pivot_longer(-Decennie, names_to = "Indicateur", values_to = "Valeur")

graph_tendance <- ggplot(donnees_tendance,
                         aes(x = Decennie, y = Valeur)) +
  geom_line(colour = "#2c7fb8", linewidth = 0.8) +
  geom_point(colour = "#2c7fb8", size = 1.8) +
  facet_wrap(~ Indicateur, scales = "free_y", ncol = 1) +
  labs(
    title = "Internationalisation de la LNH : trois indicateurs concordants",
    subtitle = paste("Par decennie de naissance des joueurs.",
                     "La longitude du centre et la dispersion evoluent",
                     "\nde pair avec la part de joueurs europeens."),
    x = "Decennie de naissance", y = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

sauver_graphique(graph_tendance, "09_graph_tendances_centre.png",
                 largeur = 9, hauteur = 8)


# =============================================================================
# PARTIE B — DERIVE INTERNE AU CANADA
# =============================================================================
# Le signal mondial est domine par l'arrivee des Europeens. En se restreignant
# au Canada, on isole une question differente : a l'interieur d'un meme pays,
# le foyer du hockey s'est-il deplace ?
#
# Ici, une projection plane (Lambert de Statistique Canada) est appropriee :
# l'emprise est assez petite pour que les distances planes soient fiables. On
# peut donc ajouter un CERCLE DE DISTANCE-TYPE, impossible a tracer proprement
# a l'echelle mondiale.

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

# On passe par group_modify plutot que summarise : la distance-type a besoin du
# centre ET de tous les points individuels du groupe en meme temps, ce qu'un
# summarise ne permet pas d'exprimer sans reutiliser une colonne deja agregee.
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

# Retour en coordonnees geographiques pour le tableau de sortie
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
jrn$ecrire(" DERIVE INTERNE AU CANADA")
jrn$ecrire("-----------------------------------------------------")

jrn$capturer(
  centres_ca |>
    mutate(across(c(DistanceType_km, longitude, latitude), ~ round(.x, 2))) |>
    select(Decennie, NbJoueurs, longitude, latitude, DistanceType_km) |>
    as.data.frame(),
  "CENTRE DE GRAVITE DES JOUEURS CANADIENS PAR DECENNIE"
)

depart_ca  <- centres_ca |> slice_head(n = 1)
arrivee_ca <- centres_ca |> slice_tail(n = 1)
derive_ca  <- sqrt((arrivee_ca$X - depart_ca$X)^2 +
                   (arrivee_ca$Y - depart_ca$Y)^2) / 1000

jrn$ecrire("")
jrn$ecrire("Deplacement net du centre canadien : ", round(derive_ca), " km")
jrn$ecrire("Longitude : de ", round(depart_ca$longitude, 2), " a ",
           round(arrivee_ca$longitude, 2), " degres")
sens_ca <- ifelse(arrivee_ca$longitude < depart_ca$longitude,
                  "vers l'OUEST", "vers l'EST")
jrn$ecrire("Sens : ", sens_ca)
jrn$ecrire("Dispersion : de ", round(depart_ca$DistanceType_km), " km a ",
           round(arrivee_ca$DistanceType_km), " km")

# Test de tendance : la derive longitudinale est-elle systematique ?
if (nrow(centres_ca) >= 4) {
  tendance <- cor.test(centres_ca$Decennie, centres_ca$longitude,
                       method = "spearman", exact = FALSE)
  jrn$capturer(tendance,
               "TEST DE TENDANCE — correlation de Spearman entre decennie et longitude du centre")
  jrn$ecrire("Une correlation negative significative confirmerait une derive")
  jrn$ecrire("reguliere vers l'ouest, et non un simple va-et-vient.")
}


# --- Ellipses de deviation standard par decennie ----------------------------
# Le cercle de distance standard ne dit rien de l'ORIENTATION du semis.
# L'ellipse, elle, donne l'axe d'etirement et son azimut : c'est le troisieme
# parametre de dispersion du manuel (section 3.2.2).

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
             "ELLIPSES DE DEVIATION STANDARD PAR DECENNIE (Canada)")
jrn$ecrire("LECTURE : l'azimut est l'orientation du GRAND axe, en degres")
jrn$ecrire("depuis le nord. Une valeur proche de 90 signale un semis etire")
jrn$ecrire("est-ouest ; l'aplatissement (petit axe / grand axe) dit a quel")
jrn$ecrire("point cet etirement est marque : plus il est proche de 0, plus le")
jrn$ecrire("semis est allonge, plus il est proche de 1, plus il est circulaire.")
jrn$ecrire("")
jrn$ecrire("MISE EN GARDE (manuel, section 3.2.2) : la TAILLE d'une ellipse")
jrn$ecrire("depend de la formule employee (Yuill, ArcGIS Pro, CrimeStat...).")
jrn$ecrire("Celle utilisee ici est celle de CrimeStat. Les ellipses de ce")
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

# Cercles de distance standard : rayon = dispersion de la decennie
cercles_ca <- centres_ca_sf |>
  st_buffer(dist = centres_ca$DistanceType_km * 1000)

# On ne trace que la premiere et la derniere decennie pour rester lisible
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
  # Ellipses de deviation standard : elles ajoutent l'ORIENTATION du semis,
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
    title = "Derive du centre de gravite des joueurs canadiens",
    subtitle = paste0("Centre moyen par decennie de naissance. Pour la ",
                      "premiere et la derniere decennie :\ncercle de distance ",
                      "standard (pointille) et ellipse de deviation standard ",
                      "(trait plein)."),
    fill = "Decennie", x = NULL, y = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))

sauver_graphique(carte_centres_ca, "09_carte_trajectoire_centre_canada.png",
                 largeur = 10, hauteur = 8)

jrn$fermer()
message("=== 09 termine ===")
