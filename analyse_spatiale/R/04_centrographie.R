# =============================================================================
# 04_centrographie.R — Centre de gravite et dispersion dans le temps
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

if (!exists("RACINE")) source(file.path("analyse_spatiale", "R", "00_config.R"))

message("\n=== 04 — CENTROGRAPHIE TEMPORELLE ===")

MIN_JOUEURS_DECENNIE <- 30   # sous ce seuil, un centre n'a aucune stabilite

jrn <- journal_resultats("04_resultats_centrographie.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 04. CENTROGRAPHIE TEMPORELLE")
jrn$ecrire("=====================================================")

hockey         <- charger_hockey()
lieux_geocodes <- charger_lieux_geocodes()

# Donnees au niveau du JOUEUR, chacun porte les coordonnees de sa ville.
# Contrairement au module 03, la duplication des coordonnees est ici SANS
# probleme : on calcule des moyennes ponderees, pas des distances entre paires.
joueurs_geo <- hockey %>%
  left_join(lieux_geocodes, by = "Birthplace") %>%
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

# --- Distance-type geodesique -----------------------------------------------
# Equivalent spatial de l'ecart-type : racine de la moyenne des carres des
# distances au centre. Mesure la DISPERSION du semis, en kilometres.
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


# =============================================================================
# PARTIE A — CENTRE DE GRAVITE MONDIAL PAR DECENNIE
# =============================================================================

decennies_valides <- joueurs_geo %>%
  count(Decennie) %>%
  filter(n >= MIN_JOUEURS_DECENNIE) %>%
  pull(Decennie)

jrn$ecrire("Decennies retenues (>= ", MIN_JOUEURS_DECENNIE, " joueurs) : ",
           paste(range(decennies_valides), collapse = " a "))

centres_monde <- lapply(sort(decennies_valides), function(dec) {
  sous <- joueurs_geo %>% filter(Decennie == dec)
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
}) %>% bind_rows()

# Deplacement d'une decennie a la suivante
centres_monde <- centres_monde %>%
  mutate(
    Deplacement_km = c(NA, sapply(2:n(), function(i) {
      p1 <- st_sfc(st_point(c(longitude[i - 1], latitude[i - 1])), crs = CRS_GEO)
      p2 <- st_sfc(st_point(c(longitude[i],     latitude[i])),     crs = CRS_GEO)
      as.numeric(st_distance(p1, p2)) / 1000
    }))
  )

sauver_tableau(centres_monde, "04_table_centres_monde.csv")

jrn$capturer(
  centres_monde %>%
    mutate(across(c(latitude, longitude, DistanceType_km,
                    PartEurope, PartCanada, Deplacement_km),
                  ~ round(.x, 2))) %>%
    as.data.frame(),
  "CENTRE DE GRAVITE MONDIAL PAR DECENNIE DE NAISSANCE"
)

# Bilan du deplacement total
depart  <- centres_monde %>% slice_head(n = 1)
arrivee <- centres_monde %>% slice_tail(n = 1)
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
CRS_ATLANTIQUE <- "+proj=laea +lat_0=55 +lon_0=-45 +datum=WGS84 +units=m +no_defs"

monde <- ne_countries(scale = "medium", returnclass = "sf")

centres_sf <- centres_monde %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO, remove = FALSE) %>%
  st_transform(CRS_ATLANTIQUE)

# Trajectoire reliant les centres successifs
trajectoire <- centres_monde %>%
  arrange(Decennie) %>%
  select(longitude, latitude) %>%
  as.matrix() %>%
  st_linestring() %>%
  st_sfc(crs = CRS_GEO) %>%
  # Segmentation : sans cela, la ligne serait tracee "droite" dans la
  # projection au lieu de suivre le trajet geodesique
  st_segmentize(units::set_units(50, km)) %>%
  st_transform(CRS_ATLANTIQUE)

coords_centres <- st_coordinates(centres_sf)
centres_monde_proj <- centres_monde %>%
  mutate(X = coords_centres[, 1], Y = coords_centres[, 2])

emprise_atl <- st_bbox(st_buffer(centres_sf, 2200000))

carte_trajectoire <- ggplot() +
  geom_sf(data = st_transform(monde, CRS_ATLANTIQUE),
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
    caption = "Auteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))

sauver_graphique(carte_trajectoire, "04_carte_trajectoire_centre_monde.png",
                 largeur = 10, hauteur = 8)


# --- Graphique A2 : longitude et dispersion dans le temps -------------------
# Version non cartographique, souvent plus convaincante dans un rapport parce
# qu'elle montre la tendance sans ambiguite de lecture.

donnees_tendance <- centres_monde %>%
  select(Decennie, `Longitude du centre (degres)` = longitude,
         `Distance-type (km)` = DistanceType_km,
         `Part de joueurs europeens (%)` = PartEurope) %>%
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
    caption = "Auteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

sauver_graphique(graph_tendance, "04_graph_tendances_centre.png",
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

joueurs_ca <- joueurs_geo %>% filter(Country == "Canada")

decennies_ca <- joueurs_ca %>%
  count(Decennie) %>%
  filter(n >= MIN_JOUEURS_DECENNIE) %>%
  pull(Decennie)

joueurs_ca_sf <- joueurs_ca %>%
  filter(Decennie %in% decennies_ca) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO) %>%
  st_transform(CRS_CA)

coords_ca <- st_coordinates(joueurs_ca_sf)
joueurs_ca_plan <- joueurs_ca_sf %>%
  st_drop_geometry() %>%
  mutate(X = coords_ca[, 1], Y = coords_ca[, 2])

# On passe par group_modify plutot que summarise : la distance-type a besoin du
# centre ET de tous les points individuels du groupe en meme temps, ce qu'un
# summarise ne permet pas d'exprimer sans reutiliser une colonne deja agregee.
centres_ca <- joueurs_ca_plan %>%
  group_by(Decennie) %>%
  group_modify(~ {
    cx <- mean(.x$X); cy <- mean(.x$Y)
    tibble::tibble(
      NbJoueurs = nrow(.x),
      X = cx, Y = cy,
      DistanceType_km = sqrt(mean((.x$X - cx)^2 + (.x$Y - cy)^2)) / 1000
    )
  }) %>%
  ungroup()

# Retour en coordonnees geographiques pour le tableau de sortie
centres_ca_geo <- centres_ca %>%
  st_as_sf(coords = c("X", "Y"), crs = CRS_CA, remove = FALSE) %>%
  st_transform(CRS_GEO)

coords_geo_ca <- st_coordinates(centres_ca_geo)
centres_ca <- centres_ca %>%
  mutate(longitude = coords_geo_ca[, 1],
         latitude  = coords_geo_ca[, 2])

sauver_tableau(centres_ca, "04_table_centres_canada.csv")

jrn$ecrire("")
jrn$ecrire("-----------------------------------------------------")
jrn$ecrire(" DERIVE INTERNE AU CANADA")
jrn$ecrire("-----------------------------------------------------")

jrn$capturer(
  centres_ca %>%
    mutate(across(c(DistanceType_km, longitude, latitude), ~ round(.x, 2))) %>%
    select(Decennie, NbJoueurs, longitude, latitude, DistanceType_km) %>%
    as.data.frame(),
  "CENTRE DE GRAVITE DES JOUEURS CANADIENS PAR DECENNIE"
)

depart_ca  <- centres_ca %>% slice_head(n = 1)
arrivee_ca <- centres_ca %>% slice_tail(n = 1)
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

# --- Carte B : centres canadiens et cercles de dispersion -------------------

canada_contour <- ne_countries(country = "Canada", scale = "medium",
                              returnclass = "sf") %>%
  st_transform(CRS_CA)

centres_ca_sf <- centres_ca %>%
  st_as_sf(coords = c("X", "Y"), crs = CRS_CA, remove = FALSE)

# Cercles de distance-type : rayon = dispersion de la decennie
cercles_ca <- centres_ca_sf %>%
  st_buffer(dist = centres_ca$DistanceType_km * 1000)

# On ne trace que la premiere et la derniere decennie pour rester lisible
cercles_extremes <- cercles_ca %>%
  filter(Decennie %in% c(min(Decennie), max(Decennie)))

trajectoire_ca <- centres_ca %>%
  arrange(Decennie) %>%
  select(X, Y) %>%
  as.matrix() %>%
  st_linestring() %>%
  st_sfc(crs = CRS_CA)

emprise_ca <- st_bbox(st_buffer(centres_ca_sf, 1500000))

carte_centres_ca <- ggplot() +
  geom_sf(data = canada_contour, fill = "grey95", colour = "grey65",
          linewidth = 0.25) +
  geom_sf(data = cercles_extremes,
          aes(colour = factor(Decennie)), fill = NA,
          linetype = "dashed", linewidth = 0.5) +
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
                      name = "Cercle de\ndistance-type") +
  coord_sf(xlim = emprise_ca[c("xmin", "xmax")],
           ylim = emprise_ca[c("ymin", "ymax")], expand = FALSE) +
  labs(
    title = "Derive du centre de gravite des joueurs canadiens",
    subtitle = paste0("Centre moyen par decennie de naissance. Les cercles ",
                      "pointilles montrent la dispersion\n(distance-type) de ",
                      "la premiere et de la derniere decennie."),
    fill = "Decennie", x = NULL, y = NULL,
    caption = "Auteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))

sauver_graphique(carte_centres_ca, "04_carte_trajectoire_centre_canada.png",
                 largeur = 10, hauteur = 8)

jrn$fermer()
message("=== 04 termine ===")
