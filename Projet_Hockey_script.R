# 1. Chargement des librairies----

library(readr)          # Importer les fichiers CSV
library(dplyr)          # Manipulation de données
library(tidyr)          # Restructuration des données
library(stringr)        # Manipulation de texte
library(lubridate)      # Gestion des dates
library(ggplot2)        # Graphiques
library(sf)             # Données spatiales
library(tmap)           # Cartographie
library(rnaturalearth)  # Fonds de carte mondiaux
library(rnaturalearthdata)
library(tidygeocoder)   # Géocodage des villes

# 2. Définition du dossier de travail----

# Aucun setwd() codé en dur : le script utilise des chemins relatifs.
# Dans VS Code, ouvrir le DOSSIER du projet (File > Open Folder) : le
# terminal R démarre alors à la racine du projet et les chemins ci-dessous
# fonctionnent tels quels. Au besoin, décommenter la ligne suivante et y
# mettre le chemin du dossier du projet sur votre ordinateur :
# setwd("C:/Users/2004x/Desktop/Ecoles/UDS/Session_E2026/Modelisation/Projet_Final/GMQ-405_PROJET_SESSION_ANALYSE_HOCKEY")

# Création des dossiers de sortie si nécessaire
dir.create("figures", showWarnings = FALSE)
dir.create("data/geocodage", recursive = TRUE, showWarnings = FALSE)

# 3. Importation des données----

hockey <- read_csv("data/GMQ-405_Hockey_Players_complet_lieux_modernes.csv")

# Vérifications rapides----
View(hockey)
names(hockey)
summary(hockey)

# 4. Nettoyage et préparation des variables ----

hockey <- hockey %>%
  mutate(
    # Conversion de la date de naissance
    Birthdate = dmy(Birthdate),
    
    # Extraction de l’année de naissance
    AnneeNaissance = year(Birthdate),
    
    # Création de la décennie de naissance
    Decennie = floor(AnneeNaissance / 10) * 10,
    
    # Catégorie joueur élite
    Elite1000 = ifelse(Pts >= 1000, "1000 points et plus", "Moins de 1000 points")
  )

# Vérifications
summary(hockey$Birthdate)
summary(hockey$AnneeNaissance)
table(hockey$Decennie, useNA = "ifany")
table(hockey$Country)

# 5. Création des groupes géographiques----

hockey <- hockey %>%
  mutate(
    GroupeGeo = case_when(
      Country == "Canada" ~ "Canada",
      Country == "USA" ~ "USA",
      Country %in% c(
        "Sweden", "Russia", "Finland", "Czech Republic", "Slovakia",
        "Switzerland", "Germany", "Latvia", "Denmark", "Norway",
        "Austria", "Belarus", "Ukraine", "Poland", "France",
        "England", "Scotland", "Wales", "Northern Ireland",
        "Ireland", "Italy", "Netherlands", "Belgium",
        "Croatia", "Slovenia", "Serbia", "Lithuania",
        "Bulgaria", "Estonia", "United Kingdom"
      ) ~ "Europe",
      TRUE ~ "Autres"
    )
  )

table(hockey$GroupeGeo)

# SECTION 1 — GRAPHIQUES----

couleurs_geo <- c(
  "Autres" = "#F8766D",
  "Canada" = "#7CAE00",
  "Europe" = "#00BFC4",
  "USA" = "#C77CFF"
)



## Graphique 1 : Top 15 des pays producteurs de joueurs----

top15_pays <- hockey %>%
  count(Country, sort = TRUE) %>%
  slice_head(n = 15)

graph_top15_pays <- ggplot(top15_pays, aes(x = reorder(Country, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 des pays de naissance des joueurs de la LNH",
    x = "Pays",
    y = "Nombre de joueurs"
  ) +
  theme_minimal()

graph_top15_pays

ggsave(
  filename = "figures/graph_top15_pays.png",
  plot = graph_top15_pays,
  width = 10,
  height = 6,
  dpi = 300
)

## Graphique 2 : Répartition par décennie de naissance----

joueurs_decennie <- hockey %>%
  filter(!is.na(Decennie)) %>%
  count(Decennie)

graph_decennie <- ggplot(joueurs_decennie, aes(x = Decennie, y = n)) +
  geom_col() +
  labs(
    title = "Nombre de joueurs de la LNH par décennie de naissance",
    x = "Décennie de naissance",
    y = "Nombre de joueurs"
  ) +
  theme_minimal()

graph_decennie

ggsave(
  filename = "figures/graph_joueurs_decennie.png",
  plot = graph_decennie,
  width = 10,
  height = 6,
  dpi = 300
)

## Graphique 3 : Évolution Canada / USA / Europe / Autres----

evolution_geo <- hockey %>%
  filter(!is.na(Decennie)) %>%
  count(Decennie, GroupeGeo)

graph_evolution_geo_prop <- ggplot(evolution_geo, aes(x = Decennie, y = n, fill = GroupeGeo)) +
  geom_col(position = "fill") +
  labs(
    title = "Évolution de la provenance des joueurs de la LNH par décennie",
    subtitle = "Répartition proportionnelle selon le groupe géographique",
    x = "Décennie de naissance",
    y = "Proportion des joueurs",
    fill = "Groupe géographique"
  ) +
  theme_minimal()

graph_evolution_geo_prop

ggsave(
  filename = "figures/graph_evolution_geo_proportion.png",
  plot = graph_evolution_geo_prop,
  width = 10,
  height = 6,
  dpi = 300
)

## Graphique 4 : Top 20 des lieux de naissance----

top20_villes <- hockey %>%
  count(Birthplace,GroupeGeo, sort = TRUE) %>%
  slice_head(n = 20)

graph_top20_villes <- ggplot(top20_villes, aes(x = reorder(Birthplace, n), y = n, fill = GroupeGeo)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = couleurs_geo) +
  labs(
    title = "Top 20 des lieux de naissance des joueurs de la LNH",
    x = "Lieu de naissance",
    y = "Nombre de joueurs",
    fill = "Groupe géographique"
  ) +
  theme_minimal()

graph_top20_villes

ggsave(
  filename = "figures/graph_top20_villes.png",
  plot = graph_top20_villes,
  width = 11,
  height = 7,
  dpi = 300
)

## Graphique 5 : Moyenne de points par pays 
## On garde seulement les pays avec au moins 20 joueurs----

moyenne_pts_pays <- hockey %>%
  group_by(Country, GroupeGeo) %>%
  summarise(
    Joueurs = n(),
    MoyennePts = mean(Pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Joueurs >= 20) %>%
  arrange(desc(MoyennePts))

graph_moyenne_pts_pays <- ggplot(
  moyenne_pts_pays,
  aes(x = reorder(Country, MoyennePts), y = MoyennePts, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = couleurs_geo) +
  labs(
    title = "Nombre moyen de points par joueur selon le pays",
    subtitle = "Pays avec au moins 20 joueurs",
    x = "Pays",
    y = "Moyenne de points en carrière",
    fill = "Groupe géographique"
  ) +
  theme_minimal()

graph_moyenne_pts_pays

ggsave(
  filename = "figures/graph_moyenne_pts_pays.png",
  plot = graph_moyenne_pts_pays,
  width = 10,
  height = 6,
  dpi = 300
)

## Graphique 6 : Boxplot des points selon la position----

graph_boxplot_position <- hockey %>%
  filter(!is.na(`Pos.`), Pts > 0) %>%
  ggplot(aes(x = `Pos.`, y = Pts)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Distribution des points en carrière selon la position",
    subtitle = "Échelle logarithmique",
    x = "Position",
    y = "Points en carrière"
  ) +
  theme_minimal()

graph_boxplot_position

ggsave(
  filename = "figures/graph_boxplot_position.png",
  plot = graph_boxplot_position,
  width = 9,
  height = 6,
  dpi = 300
)

## Graphique 7 : Relation entre matchs joués et points----

graph_gp_pts <- ggplot(hockey, aes(x = GP, y = Pts, color = Elite1000)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Relation entre les matchs joués et la production offensive",
    x = "Matchs joués",
    y = "Points",
    color = "Catégorie"
  ) +
  theme_minimal()

graph_gp_pts

ggsave(
  filename = "figures/graph_relation_gp_pts.png",
  plot = graph_gp_pts,
  width = 10,
  height = 6,
  dpi = 300
)

## Graphique 8 : Top 20 villes selon les points totaux----

top20_villes_pts <- hockey %>%
  group_by(Birthplace, GroupeGeo) %>%
  summarise(
    Joueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    MoyennePts = mean(Pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalPts)) %>%
  slice_head(n = 20)

graph_top20_villes_pts <- ggplot(
  top20_villes_pts,
  aes(x = reorder(Birthplace, TotalPts), y = TotalPts, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = couleurs_geo) +
  labs(
    title = "Top 20 des lieux de naissance selon les points totaux produits",
    x = "Lieu de naissance",
    y = "Total des points produits en LNH",
    fill = "Groupe géographique"
  ) +
  theme_minimal()

graph_top20_villes_pts

ggsave(
  filename = "figures/graph_top20_villes_points.png",
  plot = graph_top20_villes_pts,
  width = 11,
  height = 7,
  dpi = 300
)

## Graphique 9 : Joueurs de 1000 points et plus par pays----

elite_pays <- hockey %>%
  filter(Pts >= 1000) %>%
  count(Country, GroupeGeo, sort = TRUE)

graph_elite_pays <- ggplot(
  elite_pays,
  aes(x = reorder(Country, n), y = n, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = couleurs_geo) +
  labs(
    title = "Nombre de joueurs de 1000 points et plus par pays",
    x = "Pays",
    y = "Nombre de joueurs de 1000 points et plus",
    fill = "Groupe géographique"
  ) +
  theme_minimal()

graph_elite_pays

ggsave(
  filename = "figures/graph_elite_pays.png",
  plot = graph_elite_pays,
  width = 9,
  height = 6,
  dpi = 300
)

## Graphique 10 : pays avec le plus de minutes de pénalité----
library(scales)
# Fond de carte mondial
monde <- ne_countries(scale = "medium", returnclass = "sf")

# Nettoyage minimal pour les graphiques
hockey_graph <- hockey %>%
  mutate(
    Position = str_trim(.data[["Pos."]]),
    Position_label = recode(
      Position,
      "C"  = "Centre",
      "D"  = "Défenseur",
      "G"  = "Gardien",
      "LW" = "Ailier gauche",
      "RW" = "Ailier droit",
      .default = Position
    ),
    PIM = as.numeric(PIM),
    Pts = as.numeric(Pts)
  )

pim_pays_top <- hockey_graph %>%
  filter(!is.na(Country), !is.na(PIM)) %>%
  group_by(Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPIM = sum(PIM, na.rm = TRUE),
    MoyPIM = mean(PIM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalPIM)) %>%
  slice_head(n = 15)

graph_pim_pays <- ggplot(
  pim_pays_top,
  aes(x = reorder(Country, TotalPIM), y = TotalPIM)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Pays avec le plus de minutes de pénalité",
    subtitle = "Top 15 des pays selon le total de minutes de pénalité accumulées",
    x = "Pays",
    y = "Minutes de pénalité totales",
    caption = "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  theme_minimal()

graph_pim_pays

ggsave(
  filename = "figures/graph_pays_minutes_penalite.png",
  plot = graph_pim_pays,
  width = 10,
  height = 6,
  dpi = 300
)

## Graphique 11 : relation entre points et position jouée----

points_position <- hockey_graph %>%
  filter(
    !is.na(Position_label),
    !is.na(Pts)
  )

graph_points_position <- ggplot(
  points_position,
  aes(
    x = reorder(Position_label, Pts, FUN = median),
    y = Pts
  )
) +
  geom_boxplot(
    fill = "grey75",
    color = "grey30",
    outlier.alpha = 0.25
  ) +
  coord_flip() +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    labels = label_number(big.mark = " ", decimal.mark = ",")
  ) +
  labs(
    title = "Relation entre le nombre de points et la position jouée",
    subtitle = "Distribution des points en carrière selon la position principale du joueur",
    x = "Position",
    y = "Points en carrière, échelle pseudo-logarithmique",
    caption = "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud"
  ) +
  theme_minimal()

graph_points_position

ggsave(
  filename = "figures/graph_points_position.png",
  plot = graph_points_position,
  width = 10,
  height = 6,
  dpi = 300
)

# SECTION 2 - CARTES PAR PAYS----


## 1. Importation du fond de carte----

monde <- ne_countries(scale = "medium", returnclass = "sf")

# Vérification des noms de pays disponibles dans rnaturalearth
unique(monde$name)


## 2. Préparation des données par pays----

joueurs_pays <- hockey %>%
  count(Country, name = "NbJoueurs") %>%
  mutate(
    Country_map = case_when(
      Country == "USA" ~ "United States of America",
      Country == "Czech Republic" ~ "Czechia",
      Country %in% c("England", "Scotland", "Wales", "Northern Ireland", "United Kingdom") ~ "United Kingdom",
      TRUE ~ Country
    )
  ) %>%
  group_by(Country_map) %>%
  summarise(
    NbJoueurs = sum(NbJoueurs),
    .groups = "drop"
  )

# Vérifier les pays qui ne correspondent pas au fond de carte
anti_join(joueurs_pays, monde, by = c("Country_map" = "name"))

## 3. Jointure entre les pays et les données----

monde_hockey <- monde %>%
  left_join(joueurs_pays, by = c("name" = "Country_map"))

# Remplacer les NA par 0 pour les pays sans joueur
monde_hockey$NbJoueurs[is.na(monde_hockey$NbJoueurs)] <- 0

## Carte 1 : Nombre de joueurs par pays----

tmap_mode("plot")

carte_joueurs_pays <- tm_shape(monde_hockey) +
  tm_polygons(
    fill = "NbJoueurs",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 7,
      values = "brewer.blues"
    ),
    col = "grey40",
    lwd = 0.3,
    fill.legend = tm_legend(title = "Nombre de joueurs")
  ) +
  tm_title("Nombre de joueurs de la LNH par pays de naissance") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavuer St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_joueurs_pays

tmap_save(
  tm = carte_joueurs_pays,
  filename = "figures/carte_joueurs_pays.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte 2 : Joueurs de 1000 points et plus par pays----

elite_pays_carte <- hockey %>%
  filter(Pts >= 1000) %>%
  count(Country, name = "NbElite") %>%
  mutate(
    Country_map = case_when(
      Country == "USA" ~ "United States of America",
      Country == "Czech Republic" ~ "Czechia",
      Country %in% c("England", "Scotland", "Wales", "Northern Ireland", "United Kingdom") ~ "United Kingdom",
      TRUE ~ Country
    )
  ) %>%
  group_by(Country_map) %>%
  summarise(
    NbElite = sum(NbElite),
    .groups = "drop"
    )

monde_elite <- monde %>%
  left_join(elite_pays_carte, by = c("name" = "Country_map"))

monde_elite$NbElite[is.na(monde_elite$NbElite)] <- 0

carte_elite_pays <- tm_shape(monde_elite) +
  tm_polygons(
    fill = "NbElite",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 7,
      values = "brewer.reds"
    ),
    col = "grey40",
    lwd = 0.3,
    fill.legend = tm_legend(title = "Joueurs 1000+ pts")
  ) +
  tm_title("Joueurs de 1000 points et plus par pays de naissance") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_elite_pays

tmap_save(
  tm = carte_elite_pays,
  filename = "figures/carte_elite_pays.png",
  width = 10,
  height = 6,
  dpi = 300
)

# SECTION 3 - CARTES PAR VILLE / LIEU DE NAISSANCE----

## 1. Préparer les lieux uniques à géocoder----

lieux_naissance <- hockey %>%
  count(Birthplace, Country, sort = TRUE, name = "NbJoueurs")

# Vérification
View(lieux_naissance)


## 2. Géocoder les lieux de naissance (incrémental)----

# IMPORTANT : le fichier cache ci-dessous contient DÉJÀ les coordonnées de
# tous les lieux de naissance. On ne relance donc PAS un géocodage complet
# (OSM = 1 requête/seconde ≈ 40 min pour 2300 lieux). On ne géocode que les
# NOUVEAUX lieux éventuellement absents du cache. Aujourd'hui : 0 -> instantané.

dir.create("data/geocodage", recursive = TRUE, showWarnings = FALSE)
fichier_cache <- "data/geocodage/lieux_naissance_geocodes_lieux_modernes.csv"

if (file.exists(fichier_cache)) {
  cache_geo <- read_csv(fichier_cache, show_col_types = FALSE)
} else {
  cache_geo <- tibble::tibble(
    Birthplace = character(),
    latitude   = double(),
    longitude  = double()
  )
}

# Lieux présents dans les données mais pas encore dans le cache
lieux_a_geocoder <- lieux_naissance %>%
  filter(!(Birthplace %in% cache_geo$Birthplace))

if (nrow(lieux_a_geocoder) > 0) {
  message("Géocodage de ", nrow(lieux_a_geocoder), " nouveau(x) lieu(x)...")
  nouveaux_geo <- lieux_a_geocoder %>%
    geocode(
      address = Birthplace,
      method  = "arcgis",   # rapide, sans limite 1 req/sec, sans clé API
      lat     = latitude,
      long    = longitude
    )
  cache_geo <- bind_rows(cache_geo, nouveaux_geo)
  write_csv(cache_geo, fichier_cache)   # met le cache à jour
} else {
  message("Aucun nouveau lieu à géocoder : le cache est complet.")
}

# Jeu de données géocodé prêt pour la suite du script
lieux_geocodes <- cache_geo


# 4. Conversion en objet spatial sf

lieux_sf <- lieux_geocodes %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## Carte 3 : Principales villes de naissance----

top_villes_sf <- lieux_sf %>%
  arrange(desc(NbJoueurs)) %>%
  slice_head(n = 50)

carte_top_villes <- tm_shape(monde) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(top_villes_sf) +
  tm_symbols(
    size = "NbJoueurs",
    scale = 1.5,
    col = "red",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Nombre de joueurs")
  ) +
  tm_title("Principales villes de naissance des joueurs de la LNH") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_top_villes

tmap_save(
  tm = carte_top_villes,
  filename = "figures/carte_top_villes.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte 4 : Villes ayant produit des joueurs de 1000+ pts----

villes_elite <- hockey %>%
  filter(Pts >= 1000) %>%
  count(Birthplace, Country, name = "NbElite")

villes_elite_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_elite, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

carte_villes_elite <- tm_shape(monde) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_elite_geo) +
  tm_symbols(
    size = "NbElite",
    scale = 1.8,
    col = "red",
    alpha = 0.7,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Joueurs 1000+ pts")
  ) +
  tm_title("Villes ayant produit des joueurs de 1000 points et plus") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_elite

tmap_save(
  tm = carte_villes_elite,
  filename = "figures/carte_villes_elite.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte 5 : Production offensive totale par ville----

villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

top_villes_points_sf <- villes_points_geo %>%
  arrange(desc(TotalPts)) %>%
  slice_head(n = 50)

carte_villes_points <- tm_shape(monde) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(top_villes_points_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.6,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points

tmap_save(
  tm = carte_villes_points,
  filename = "figures/carte_villes_points.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte 5B : Production offensive totale par ville au Québec ceci est un test----

library(stringr)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthhires)

# Agrégation des points par lieu de naissance
villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure avec les coordonnées
villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Garder seulement les villes du Québec
villes_points_qc_sf <- villes_points_geo %>%
  filter(str_detect(Birthplace, ", QC, Canada")) %>%
  arrange(desc(TotalPts))

# Boîte de zoom approximative sur le Québec
bbox_quebec <- st_bbox(
  c(
    xmin = -76.5,
    xmax = -67.0,
    ymin = 44.8,
    ymax = 49.5
  ),
  crs = st_crs(4326)
)

# Carte zoom Québec
carte_villes_points_qc <- tm_shape(monde, bbox = bbox_quebec) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_qc_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.4,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance au Québec") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_qc

tmap_save(
  tm = carte_villes_points_qc,
  filename = "figures/carte_villes_points_quebec.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte pour le reste des provinces
# Fond de carte : provinces canadiennes
canada_prov <- ne_states(country = "Canada", returnclass = "sf")

# Agrégation des points par lieu de naissance
villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure avec les coordonnées géocodées
villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

tmap_mode("plot")

##Carte : Ontario---- 

villes_points_on_sf <- villes_points_geo %>%
  filter(str_detect(Birthplace, ", ON, Canada")) %>%
  arrange(desc(TotalPts))

bbox_ontario <- st_bbox(
  c(
    xmin = -95.5,
    xmax = -74.0,
    ymin = 41.3,
    ymax = 57.5
  ),
  crs = st_crs(4326)
)

carte_villes_points_ontario <- tm_shape(canada_prov, bbox = bbox_ontario) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_on_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.3,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Ontario") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_ontario

tmap_save(
  tm = carte_villes_points_ontario,
  filename = "figures/carte_villes_points_ontario.png",
  width = 10,
  height = 6,
  dpi = 300
)
## Carte : Ontario

villes_points_on_sf <- villes_points_geo %>%
  filter(str_detect(Birthplace, ", ON, Canada")) %>%
  arrange(desc(TotalPts))

bbox_ontario <- st_bbox(
  c(
    xmin = -95.5,
    xmax = -74.0,
    ymin = 41.3,
    ymax = 57.5
  ),
  crs = st_crs(4326)
)

carte_villes_points_ontario <- tm_shape(canada_prov, bbox = bbox_ontario) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_on_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.3,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Ontario") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_ontario

tmap_save(
  tm = carte_villes_points_ontario,
  filename = "figures/carte_villes_points_ontario.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte : Alberta----

villes_points_ab_sf <- villes_points_geo %>%
  filter(str_detect(Birthplace, ", AB, Canada")) %>%
  arrange(desc(TotalPts))

bbox_alberta <- st_bbox(
  c(
    xmin = -120.0,
    xmax = -109.0,
    ymin = 48.8,
    ymax = 60.5
  ),
  crs = st_crs(4326)
)

carte_villes_points_alberta <- tm_shape(canada_prov, bbox = bbox_alberta) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_ab_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.5,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Alberta") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_alberta

tmap_save(
  tm = carte_villes_points_alberta,
  filename = "figures/carte_villes_points_alberta.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte : Colombie-Britannique----

villes_points_bc_sf <- villes_points_geo %>%
  filter(str_detect(Birthplace, ", BC, Canada")) %>%
  arrange(desc(TotalPts))

bbox_bc <- st_bbox(
  c(
    xmin = -139.5,
    xmax = -113.5,
    ymin = 48.0,
    ymax = 60.5
  ),
  crs = st_crs(4326)
)

carte_villes_points_bc <- tm_shape(canada_prov, bbox = bbox_bc) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_bc_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.4,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Colombie-Britannique") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_bc

tmap_save(
  tm = carte_villes_points_bc,
  filename = "figures/carte_villes_points_bc.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte : Provinces atlantiques----
# NB = Nouveau-Brunswick
# NS = Nouvelle-Écosse
# PE / PEI = Île-du-Prince-Édouard
# NL / NFLD / NFL = Terre-Neuve-et-Labrador

villes_points_atl_sf <- villes_points_geo %>%
  filter(str_detect(Birthplace, ", (NB|NS|PE|PEI|NL|NFLD|NFL), Canada")) %>%
  arrange(desc(TotalPts))

bbox_atlantique <- st_bbox(
  c(
    xmin = -68.5,
    xmax = -52.0,
    ymin = 43.0,
    ymax = 54.5
  ),
  crs = st_crs(4326)
)

carte_villes_points_atlantique <- tm_shape(canada_prov, bbox = bbox_atlantique) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_atl_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.7,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance dans les provinces atlantiques") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_atlantique

tmap_save(
  tm = carte_villes_points_atlantique,
  filename = "figures/carte_villes_points_atlantique.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Cartes régionales des États-Unis----
# Production offensive totale par ville de naissance

# Fond de carte : États américains
usa_states <- ne_states(country = "United States of America", returnclass = "sf")

# Agrégation des points par lieu de naissance
villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure avec les coordonnées géocodées
villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Garder seulement les villes aux États-Unis
villes_points_us_sf <- villes_points_geo %>%
  filter(Country == "USA") %>%
  mutate(
    State = str_match(Birthplace, ", ([A-Z]{2}), USA$")[, 2]
  )

# Vérification des États détectés
table(villes_points_us_sf$State, useNA = "ifany")

# Tableau de correspondance État -> région
us_regions <- tibble::tibble(
  State = c(
    "ME","NH","VT","MA","RI","CT","NY","NJ","PA",
    "OH","MI","IN","IL","WI","MN","IA","MO","ND","SD","NE","KS",
    "DE","MD","DC","VA","WV","NC","SC","GA","FL",
    "KY","TN","MS","AL","OK","TX","AR","LA",
    "MT","ID","WY","CO","UT","NV","AZ","NM",
    "WA","OR","CA",
    "AK","HI"
  ),
  RegionUS = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("Mountain_Southwest", 8),
    rep("Pacific", 3),
    rep("Alaska_Hawaii", 2)
  )
)

# Ajouter la région aux villes américaines
villes_points_us_sf <- villes_points_us_sf %>%
  left_join(us_regions, by = "State")

# Vérifier si certaines villes américaines n'ont pas de région
villes_points_us_sf %>%
  filter(is.na(RegionUS)) %>%
  select(Birthplace, State, NbJoueurs, TotalPts)

# Mode cartographie
tmap_mode("plot")

## Carte : Northeast----

villes_points_us_ne_sf <- villes_points_us_sf %>%
  filter(RegionUS == "Northeast") %>%
  arrange(desc(TotalPts))

bbox_us_northeast <- st_bbox(
  c(
    xmin = -82.5,
    xmax = -66.0,
    ymin = 37.0,
    ymax = 48.8
  ),
  crs = st_crs(4326)
)

carte_us_northeast <- tm_shape(usa_states, bbox = bbox_us_northeast) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_us_ne_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.2,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance dans le Nord-Est des États-Unis") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_us_northeast

tmap_save(
  tm = carte_us_northeast,
  filename = "figures/carte_villes_points_us_northeast.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte : Midwest----

villes_points_us_midwest_sf <- villes_points_us_sf %>%
  filter(RegionUS == "Midwest") %>%
  arrange(desc(TotalPts))

bbox_us_midwest <- st_bbox(
  c(
    xmin = -105.5,
    xmax = -79.0,
    ymin = 36.0,
    ymax = 50.5
  ),
  crs = st_crs(4326)
)

carte_us_midwest <- tm_shape(usa_states, bbox = bbox_us_midwest) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_us_midwest_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.2,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance dans le Midwest des États-Unis") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_us_midwest

tmap_save(
  tm = carte_us_midwest,
  filename = "figures/carte_villes_points_us_midwest.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte : South----

villes_points_us_south_sf <- villes_points_us_sf %>%
  filter(RegionUS == "South") %>%
  arrange(desc(TotalPts))

bbox_us_south <- st_bbox(
  c(
    xmin = -107.5,
    xmax = -74.0,
    ymin = 24.0,
    ymax = 39.5
  ),
  crs = st_crs(4326)
)

carte_us_south <- tm_shape(usa_states, bbox = bbox_us_south) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_us_south_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.25,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance dans le Sud des États-Unis") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_us_south

tmap_save(
  tm = carte_us_south,
  filename = "figures/carte_villes_points_us_south.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte : Mountain / Southwest----

villes_points_us_mountain_sf <- villes_points_us_sf %>%
  filter(RegionUS == "Mountain_Southwest") %>%
  arrange(desc(TotalPts))

bbox_us_mountain <- st_bbox(
  c(
    xmin = -117.8,
    xmax = -100.0,
    ymin = 31.0,
    ymax = 49.5
  ),
  crs = st_crs(4326)
)

carte_us_mountain <- tm_shape(usa_states, bbox = bbox_us_mountain) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_us_mountain_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.35,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance dans la région Mountain / Southwest des États-Unis") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_us_mountain

tmap_save(
  tm = carte_us_mountain,
  filename = "figures/carte_villes_points_us_mountain_southwest.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte : Pacific / West Coast----

villes_points_us_pacific_sf <- villes_points_us_sf %>%
  filter(RegionUS == "Pacific") %>%
  arrange(desc(TotalPts))

bbox_us_pacific <- st_bbox(
  c(
    xmin = -125.5,
    xmax = -114.0,
    ymin = 31.0,
    ymax = 49.5
  ),
  crs = st_crs(4326)
)

carte_us_pacific <- tm_shape(usa_states, bbox = bbox_us_pacific) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_us_pacific_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.35,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance sur la côte Pacifique des États-Unis") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_us_pacific

tmap_save(
  tm = carte_us_pacific,
  filename = "figures/carte_villes_points_us_pacific.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte optionnelle : Alaska et Hawaii----

villes_points_us_ak_hi_sf <- villes_points_us_sf %>%
  filter(RegionUS == "Alaska_Hawaii") %>%
  arrange(desc(TotalPts))

nrow(villes_points_us_ak_hi_sf)

bbox_us_ak_hi <- st_bbox(
  c(
    xmin = -180.0,
    xmax = -150.0,
    ymin = 18.0,
    ymax = 72.0
  ),
  crs = st_crs(4326)
)

carte_us_ak_hi <- tm_shape(usa_states, bbox = bbox_us_ak_hi) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_us_ak_hi_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.5,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Alaska et Hawaii") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_us_ak_hi

tmap_save(
  tm = carte_us_ak_hi,
  filename = "figures/carte_villes_points_us_alaska_hawaii.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte Europe----
# Production offensive totale par ville de naissance

dir.create("figures", showWarnings = FALSE)

# Fond de carte mondial
monde <- ne_countries(scale = "medium", returnclass = "sf")

# Agrégation des points par lieu de naissance
villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure avec les coordonnées géocodées
villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Liste des pays européens présents dans le jeu de données
# Russie exclue ici, car elle sera cartographiée séparément
pays_europe <- c(
  "England",
  "Scotland",
  "Wales",
  "Northern Ireland",
  "United Kingdom",
  "Ireland",
  "France",
  "Belgium",
  "Netherlands",
  "Germany",
  "Switzerland",
  "Austria",
  "Italy",
  "Czech Republic",
  "Slovakia",
  "Poland",
  "Sweden",
  "Finland",
  "Norway",
  "Denmark",
  "Latvia",
  "Lithuania",
  "Estonia",
  "Belarus",
  "Ukraine",
  "Croatia",
  "Slovenia",
  "Serbia"
)

# Garder seulement les villes européennes
villes_points_europe_sf <- villes_points_geo %>%
  filter(Country %in% pays_europe) %>%
  arrange(desc(TotalPts))

# Vérification rapide
villes_points_europe_sf %>%
  st_drop_geometry() %>%
  count(Country, sort = TRUE)

# Boîte de zoom pour l'Europe
bbox_europe <- st_bbox(
  c(
    xmin = -12.5,
    xmax = 35.0,
    ymin = 35.0,
    ymax = 72.0
  ),
  crs = st_crs(4326)
)

# Mode cartographie
tmap_mode("plot")

## Carte Europe----
carte_villes_points_europe <- tm_shape(monde, bbox = bbox_europe) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_europe_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.25,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Europe") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_europe

tmap_save(
  tm = carte_villes_points_europe,
  filename = "figures/carte_villes_points_europe.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte des pays nordiques----
# Production offensive totale par ville de naissance
# Fond de carte mondial
monde <- ne_countries(scale = "medium", returnclass = "sf")

# Agrégation des points par lieu de naissance
villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure avec les coordonnées géocodées
villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Pays nordiques
pays_nordiques <- c(
  "Sweden",
  "Norway",
  "Denmark",
  "Finland"
)

# Garder seulement les villes des pays nordiques
villes_points_nordiques_sf <- villes_points_geo %>%
  filter(Country %in% pays_nordiques) %>%
  arrange(desc(TotalPts))

# Vérification rapide
villes_points_nordiques_sf %>%
  st_drop_geometry() %>%
  count(Country, sort = TRUE)

# Boîte de zoom pour les pays nordiques
bbox_nordiques <- st_bbox(
  c(
    xmin = 4.0,
    xmax = 32.0,
    ymin = 54.0,
    ymax = 71.5
  ),
  crs = st_crs(4326)
)

# Mode cartographie
tmap_mode("plot")

# Carte pays nordiques
carte_villes_points_nordiques <- tm_shape(monde, bbox = bbox_nordiques) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_nordiques_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.4,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance dans les pays nordiques") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_nordiques

tmap_save(
  tm = carte_villes_points_nordiques,
  filename = "figures/carte_villes_points_nordiques.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte Russie européenne----
# Production offensive totale par ville de naissance
# Fond de carte mondial
monde <- ne_countries(scale = "medium", returnclass = "sf")

# Agrégation des points par lieu de naissance
villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure avec les coordonnées géocodées
villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Garder seulement les villes de la Russie européenne
villes_points_russie_euro_sf <- villes_points_geo %>%
  filter(
    Country == "Russia",
    longitude <= 60
  ) %>%
  arrange(desc(TotalPts))

# Vérification rapide
villes_points_russie_euro_sf %>%
  st_drop_geometry() %>%
  select(Birthplace, Country, longitude, TotalPts) %>%
  arrange(desc(TotalPts))

# Boîte de zoom pour la Russie européenne
bbox_russie_euro <- st_bbox(
  c(
    xmin = 20.0,
    xmax = 65.0,
    ymin = 41.0,
    ymax = 72.0
  ),
  crs = st_crs(4326)
)

# Mode cartographie
tmap_mode("plot")

# Carte Russie européenne
carte_villes_points_russie_euro <- tm_shape(monde, bbox = bbox_russie_euro) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_russie_euro_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.35,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Russie européenne") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_russie_euro

tmap_save(
  tm = carte_villes_points_russie_euro,
  filename = "figures/carte_villes_points_russie_europeenne.png",
  width = 10,
  height = 6,
  dpi = 300
)

## Carte Russie asiatique----
# Production offensive totale par ville de naissance
# Fond de carte mondial
monde <- ne_countries(scale = "medium", returnclass = "sf")

# Agrégation des points par lieu de naissance
villes_points <- hockey %>%
  group_by(Birthplace, Country) %>%
  summarise(
    NbJoueurs = n(),
    TotalPts = sum(Pts, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure avec les coordonnées géocodées
villes_points_geo <- lieux_geocodes %>%
  select(Birthplace, latitude, longitude) %>%
  right_join(villes_points, by = "Birthplace") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Garder seulement les villes de la Russie asiatique
villes_points_russie_asie_sf <- villes_points_geo %>%
  filter(
    Country == "Russia",
    longitude > 60
  ) %>%
  arrange(desc(TotalPts))

# Vérification rapide
villes_points_russie_asie_sf %>%
  st_drop_geometry() %>%
  select(Birthplace, Country, longitude, latitude, TotalPts) %>%
  arrange(desc(TotalPts))

# Boîte de zoom pour la Russie asiatique
bbox_russie_asie <- st_bbox(
  c(
    xmin = 55.0,
    xmax = 180.0,
    ymin = 40.0,
    ymax = 73.0
  ),
  crs = st_crs(4326)
)

# Mode cartographie
tmap_mode("plot")

# Carte Russie asiatique
carte_villes_points_russie_asie <- tm_shape(monde, bbox = bbox_russie_asie) +
  tm_polygons(col = "grey80", fill = "grey95") +
  tm_shape(villes_points_russie_asie_sf) +
  tm_symbols(
    size = "TotalPts",
    scale = 1.5,
    col = "blue",
    alpha = 0.6,
    legend.size.show = TRUE,
    size.legend = tm_legend(title = "Points totaux")
  ) +
  tm_title("Production offensive totale par ville de naissance en Russie asiatique") +
  tm_credits(
    "Source : Hockey DB / NHL player data\nAuteur : Philippe Filion, Xavier Lafrance, Xavier St-Arnaud",
    position = tm_pos_in("left", "bottom"),
    size = 0.7
  )

carte_villes_points_russie_asie

tmap_save(
  tm = carte_villes_points_russie_asie,
  filename = "figures/carte_villes_points_russie_asiatique.png",
  width = 10,
  height = 6,
  dpi = 300
)

# SECTION 4 — TABLEAUX DE SORTIE POUR LE RAPPORT----


## Nombre de joueurs par pays----
table_pays <- hockey %>%
  count(Country, sort = TRUE) %>%
  mutate(Pourcentage = round(n / sum(n) * 100, 2))

write_csv(table_pays, "figures/table_joueurs_pays.csv")


## Top 20 villes----
write_csv(top20_villes, "figures/table_top20_villes.csv")


## Top 20 villes selon les points totaux----
write_csv(top20_villes_pts, "figures/table_top20_villes_points.csv")


## Joueurs de 1000 points et plus----
joueurs_1000 <- hockey %>%
  filter(Pts >= 1000) %>%
  arrange(desc(Pts)) %>%
  select(`Player Name`, `Pos.`, Birthdate, Birthplace, Country, GP, G, A, Pts)

write_csv(joueurs_1000, "figures/table_joueurs_1000pts.csv")

