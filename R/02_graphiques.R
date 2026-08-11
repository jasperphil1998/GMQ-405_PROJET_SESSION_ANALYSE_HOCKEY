# =============================================================================
# 02_graphiques.R — Graphiques descriptifs
# =============================================================================
# ORIGINE : SECTION 1 de archive/Projet_Hockey_script_ORIGINAL.R (11 graphiques
# réalisés par Philippe Filion et Xavier Lafrance).
#
# Le contenu statistique est inchangé. Ce qui a changé :
#  - le pipe %>% est remplacé par le pipe natif |> (convention du projet) ;
#  - la mention de source et d'auteurs vient de la constante CREDITS ;
#  - la palette vient de la constante COULEURS_GEO ;
#  - la préparation des données (dates, décennies, positions traduites) est
#    faite une seule fois dans 00_config.R au lieu d'être répétée ici ;
#  - les View() interactifs ont été retirés : ils bloquent une exécution par
#    Rscript. Les vérifications équivalentes passent par message().
#
# SORTIES : 11 figures PNG dans figures/ (non versionnées).
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 02 — GRAPHIQUES DESCRIPTIFS ===")

hockey <- charger_hockey()

# Vérifications rapides (remplacent les View() du script d'origine)
message("Joueurs : ", nrow(hockey))
message("Pays distincts : ", dplyr::n_distinct(hockey$Country))
message("Décennies : ",
        paste(range(hockey$Decennie, na.rm = TRUE), collapse = " à "))


## Graphique 1 : Top 15 des pays producteurs de joueurs ----------------------

top15_pays <- hockey |>
  count(Country, sort = TRUE) |>
  slice_head(n = 15)

graph_top15_pays <- ggplot(top15_pays, aes(x = reorder(Country, n), y = n)) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(
    title = "Top 15 des pays de naissance des joueurs de la LNH",
    x = "Pays",
    y = "Nombre de joueurs",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_top15_pays, "graph_top15_pays.png",
                     largeur = 10, hauteur = 6)


## Graphique 2 : Répartition par décennie de naissance -----------------------

joueurs_decennie <- hockey |>
  filter(!is.na(Decennie)) |>
  count(Decennie)

graph_decennie <- ggplot(joueurs_decennie, aes(x = Decennie, y = n)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    title = "Nombre de joueurs de la LNH par décennie de naissance",
    x = "Décennie de naissance",
    y = "Nombre de joueurs",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_decennie, "graph_joueurs_decennie.png",
                     largeur = 10, hauteur = 6)


## Graphique 3 : Évolution Canada / USA / Europe / Autres --------------------

evolution_geo <- hockey |>
  filter(!is.na(Decennie)) |>
  count(Decennie, GroupeGeo)

graph_evolution_geo_prop <- ggplot(
  evolution_geo,
  aes(x = Decennie, y = n, fill = GroupeGeo)
) +
  geom_col(position = "fill") +
  scale_fill_manual(values = COULEURS_GEO) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Évolution de la provenance des joueurs de la LNH par décennie",
    subtitle = "Répartition proportionnelle selon le groupe géographique",
    x = "Décennie de naissance",
    y = "Proportion des joueurs",
    fill = "Groupe géographique",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_evolution_geo_prop,
                     "graph_evolution_geo_proportion.png",
                     largeur = 10, hauteur = 6)


## Graphique 4 : Top 20 des lieux de naissance -------------------------------

top20_villes <- hockey |>
  count(Birthplace, GroupeGeo, sort = TRUE) |>
  slice_head(n = 20)

graph_top20_villes <- ggplot(
  top20_villes,
  aes(x = reorder(Birthplace, n), y = n, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = COULEURS_GEO) +
  labs(
    title = "Top 20 des lieux de naissance des joueurs de la LNH",
    x = "Lieu de naissance",
    y = "Nombre de joueurs",
    fill = "Groupe géographique",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_top20_villes, "graph_top20_villes.png",
                     largeur = 11, hauteur = 7)


## Graphique 5 : Moyenne de points par pays ----------------------------------
# Seuls les pays comptant au moins 20 joueurs sont retenus : sous ce seuil, la
# moyenne est dominée par le bruit d'échantillonnage.

moyenne_pts_pays <- hockey |>
  group_by(Country, GroupeGeo) |>
  summarise(
    Joueurs    = n(),
    MoyennePts = mean(Pts, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  filter(Joueurs >= 20) |>
  arrange(desc(MoyennePts))

graph_moyenne_pts_pays <- ggplot(
  moyenne_pts_pays,
  aes(x = reorder(Country, MoyennePts), y = MoyennePts, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = COULEURS_GEO) +
  labs(
    title = "Nombre moyen de points par joueur selon le pays",
    subtitle = "Pays avec au moins 20 joueurs",
    x = "Pays",
    y = "Moyenne de points en carrière",
    fill = "Groupe géographique",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_moyenne_pts_pays, "graph_moyenne_pts_pays.png",
                     largeur = 10, hauteur = 6)


## Graphique 6 : Boxplot des points selon la position ------------------------

graph_boxplot_position <- hockey |>
  filter(!is.na(PositionLabel), Pts > 0) |>
  ggplot(aes(x = PositionLabel, y = Pts)) +
  geom_boxplot(fill = "grey75", colour = "grey30") +
  scale_y_log10(labels = label_number(big.mark = " ")) +
  labs(
    title = "Distribution des points en carrière selon la position",
    subtitle = "Échelle logarithmique",
    x = "Position",
    y = "Points en carrière",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_boxplot_position, "graph_boxplot_position.png",
                     largeur = 9, hauteur = 6)


## Graphique 7 : Relation entre matchs joués et points -----------------------
# Nuage de points avec droite de régression : c'est le graphique ajouté par
# Philippe Filion (commits "droite de régression graphique #7" et "ajout de la
# droite et courbe de régression").

graph_gp_pts <- ggplot(hockey, aes(x = GP, y = Pts)) +
  geom_point(aes(colour = Elite1000), alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "black", linewidth = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              colour = "#d95f0e", linetype = "dashed", linewidth = 0.8) +
  scale_colour_manual(values = c("1000 points et plus"  = "#b2182b",
                                 "Moins de 1000 points" = "#2c7fb8")) +
  labs(
    title = "Relation entre les matchs joués et la production offensive",
    subtitle = paste("Droite de régression linéaire (noir) et courbe de",
                     "régression locale loess (orange pointillé)"),
    x = "Matchs joués",
    y = "Points",
    colour = "Catégorie",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_gp_pts, "graph_relation_gp_pts.png",
                     largeur = 10, hauteur = 6)

# Coefficient de la régression, utile à citer dans le rapport
modele_gp_pts <- lm(Pts ~ GP, data = hockey)
message("Régression Pts ~ GP : pente = ",
        round(coef(modele_gp_pts)[["GP"]], 3),
        " point par match ; R2 = ",
        round(summary(modele_gp_pts)$r.squared, 3))


## Graphique 8 : Top 20 villes selon les points totaux -----------------------

top20_villes_pts <- hockey |>
  group_by(Birthplace, GroupeGeo) |>
  summarise(
    Joueurs    = n(),
    TotalPts   = sum(Pts, na.rm = TRUE),
    MoyennePts = mean(Pts, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  arrange(desc(TotalPts)) |>
  slice_head(n = 20)

graph_top20_villes_pts <- ggplot(
  top20_villes_pts,
  aes(x = reorder(Birthplace, TotalPts), y = TotalPts, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = COULEURS_GEO) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Top 20 des lieux de naissance selon les points totaux produits",
    x = "Lieu de naissance",
    y = "Total des points produits en LNH",
    fill = "Groupe géographique",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_top20_villes_pts, "graph_top20_villes_points.png",
                     largeur = 11, hauteur = 7)


## Graphique 9 : Joueurs de 1000 points et plus par pays ---------------------

elite_pays <- hockey |>
  filter(Pts >= SEUIL_ELITE_PTS) |>
  count(Country, GroupeGeo, sort = TRUE)

graph_elite_pays <- ggplot(
  elite_pays,
  aes(x = reorder(Country, n), y = n, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = COULEURS_GEO) +
  labs(
    title = paste0("Nombre de joueurs de ", SEUIL_ELITE_PTS,
                   " points et plus par pays"),
    x = "Pays",
    y = paste0("Nombre de joueurs de ", SEUIL_ELITE_PTS, " points et plus"),
    fill = "Groupe géographique",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_elite_pays, "graph_elite_pays.png",
                     largeur = 9, hauteur = 6)


## Graphique 10 : Pays avec le plus de minutes de pénalité -------------------

pim_pays_top <- hockey |>
  filter(!is.na(Country), !is.na(PIM), !is.na(GroupeGeo)) |>
  group_by(Country, GroupeGeo) |>
  summarise(
    NbJoueurs = n(),
    TotalPIM  = sum(PIM, na.rm = TRUE),
    MoyPIM    = mean(PIM, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  arrange(desc(TotalPIM)) |>
  slice_head(n = 15)

graph_pim_pays <- ggplot(
  pim_pays_top,
  aes(x = reorder(Country, TotalPIM), y = TotalPIM, fill = GroupeGeo)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = COULEURS_GEO) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  labs(
    title = "Pays avec le plus de minutes de pénalité",
    subtitle = paste("Top 15 des pays selon le total de minutes de pénalité",
                     "accumulées"),
    x = "Pays",
    y = "Minutes de pénalité totales",
    fill = "Groupe géographique",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_pim_pays, "graph_pays_minutes_penalite.png",
                     largeur = 10, hauteur = 6)


## Graphique 11 : Relation entre points et position jouée --------------------

points_position <- hockey |>
  filter(!is.na(PositionLabel), !is.na(Pts))

graph_points_position <- ggplot(
  points_position,
  aes(x = reorder(PositionLabel, Pts, FUN = median), y = Pts)
) +
  geom_boxplot(fill = "grey75", colour = "grey30", outlier.alpha = 0.25) +
  coord_flip() +
  scale_y_continuous(
    transform = scales::pseudo_log_trans(base = 10),
    labels = label_number(big.mark = " ", decimal.mark = ",")
  ) +
  labs(
    title = "Relation entre le nombre de points et la position jouée",
    subtitle = paste("Distribution des points en carrière selon la position",
                     "principale du joueur"),
    x = "Position",
    y = "Points en carrière, échelle pseudo-logarithmique",
    caption = CREDITS
  ) +
  theme_minimal()

sauver_graphique(graph_points_position, "graph_points_position.png",
                     largeur = 10, hauteur = 6)


message("=== 02 terminé ===")
