# *****************************************************************************
# 05_tableaux.R — Tableaux de sortie pour le rapport
# *****************************************************************************
#
# Les tableaux de la section 4 d'origine dépendaient d'objets calculés plus
# haut dans le script (top20_villes, top20_villes_pts). Ils sont ici recalculés
# explicitement, pour que le module puisse être lancé seul.
#
# SORTIES : 6 tableaux CSV dans figures/ (non versionnés).
# *****************************************************************************

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 05 — TABLEAUX POUR LE RAPPORT ===")

hockey <- charger_hockey()


## Nombre de joueurs par pays ------------------------------------------------

table_pays <- hockey |>
  count(Country, sort = TRUE) |>
  mutate(Pourcentage = round(n / sum(n) * 100, 2))

sauver_tableau(table_pays, "table_joueurs_pays.csv")


## Top 20 des lieux de naissance selon le nombre de joueurs ------------------

top20_villes <- hockey |>
  count(Birthplace, GroupeGeo, sort = TRUE) |>
  slice_head(n = 20)

sauver_tableau(top20_villes, "table_top20_villes.csv")


## Top 20 des lieux de naissance selon les points totaux ---------------------

top20_villes_pts <- hockey |>
  group_by(Birthplace, GroupeGeo) |>
  summarise(
    Joueurs    = n(),
    TotalPts   = sum(Pts, na.rm = TRUE),
    MoyennePts = round(mean(Pts, na.rm = TRUE), 1),
    .groups    = "drop"
  ) |>
  arrange(desc(TotalPts)) |>
  slice_head(n = 20)

sauver_tableau(top20_villes_pts, "table_top20_villes_points.csv")


## Joueurs de 1000 points et plus --------------------------------------------

joueurs_1000 <- hockey |>
  filter(Pts >= SEUIL_ELITE_PTS) |>
  arrange(desc(Pts)) |>
  select(`Player Name`, `Pos.`, Birthdate, Birthplace, Country, GP, G, A, Pts)

sauver_tableau(joueurs_1000, "table_joueurs_1000pts.csv")
message("Joueurs de ", SEUIL_ELITE_PTS, " points et plus : ",
        nrow(joueurs_1000))


## Répartition par décennie et groupe géographique ---------------------------

table_decennie_geo <- hockey |>
  filter(!is.na(Decennie)) |>
  count(Decennie, GroupeGeo) |>
  tidyr::pivot_wider(names_from = GroupeGeo, values_from = n,
                     values_fill = 0) |>
  arrange(Decennie) |>
  mutate(Total = rowSums(across(-Decennie)))

sauver_tableau(table_decennie_geo, "table_decennie_groupe_geo.csv")


## Statistiques par position -------------------------------------------------

table_positions <- hockey |>
  filter(!is.na(PositionLabel)) |>
  group_by(Position = PositionLabel) |>
  summarise(
    NbJoueurs   = n(),
    MatchsMoyen = round(mean(GP, na.rm = TRUE), 1),
    PtsMoyen    = round(mean(Pts, na.rm = TRUE), 1),
    PtsMedian   = median(Pts, na.rm = TRUE),
    PIMMoyen    = round(mean(PIM, na.rm = TRUE), 1),
    .groups     = "drop"
  ) |>
  arrange(desc(NbJoueurs))

sauver_tableau(table_positions, "table_statistiques_positions.csv")


message("=== 05 terminé ===")
