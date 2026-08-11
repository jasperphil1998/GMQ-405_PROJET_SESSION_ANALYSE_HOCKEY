# *****************************************************************************
# 07_grille_hexagonale.R — Production moyenne par joueur, grille hexagonale
# *****************************************************************************
#
# VARIABLE ANALYSÉE : PTS MOYEN PAR JOUEUR né dans la cellule. C'est un RATIO,
# donc déjà normalisé : contrairement à un effectif, il ne dépend pas de la
# population de la cellule. Il répond à une question différente et plus fine
# que les cartes d'effectifs des modules 03 et 04 : la QUALITÉ des joueurs
# est-elle spatialement structurée, et non seulement leur nombre ?
#
# PRÉREQUIS : lancer 06_normalisation.R avant (il produit
# sorties/unites_normalisees.rds, dont on ne tire ici que les limites
# administratives tracées par-dessus la grille).
#
# SORTIE : 1 carte.
# *****************************************************************************

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 07 — GRILLE HEXAGONALE ===")

fichier_unites <- chemin_sortie("unites_normalisees.rds")
if (!file.exists(fichier_unites)) {
  stop("Lancer d'abord 06_normalisation.R (produit unites_normalisees.rds).",
       call. = FALSE)
}


unites    <- readRDS(fichier_unites) |> st_transform(CRS_NA)
villes_sf <- construire_villes_sf()

amerique_nord <- ne_countries(
  country = c("Canada", "United States of America"),
  scale = "medium", returnclass = "sf"
) |>
  st_transform(CRS_NA) |>
  st_union() |>
  st_make_valid()

villes_na <- villes_sf |>
  filter(Country %in% c("Canada", "USA")) |>
  st_transform(CRS_NA)


# *****************************************************************************
# 1. CONSTRUCTION DE LA GRILLE ----
# *****************************************************************************

TAILLE_CELLULE <- 200000   # 200 km entre côtés opposés
MIN_JOUEURS    <- 5        # sous ce seuil, la moyenne est trop instable

grille_cellules <- st_make_grid(
  amerique_nord, cellsize = TAILLE_CELLULE, square = FALSE
)

grille <- st_sf(IdCellule = seq_along(grille_cellules),
                geometry = grille_cellules)

# On conserve uniquement les cellules qui touchent les terres émergées
grille <- grille[lengths(st_intersects(grille, amerique_nord)) > 0, ]


# *****************************************************************************
# 2. AGRÉGATION DES VILLES PAR CELLULE ----
# *****************************************************************************
# Affectation de chaque ville à sa cellule, puis agrégation.

villes_cellule <- st_join(villes_na, grille, join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(IdCellule)) |>
  group_by(IdCellule) |>
  summarise(
    NbJoueurs = sum(NbJoueurs),
    TotalPts  = sum(TotalPts),
    NbVilles  = n(),
    .groups   = "drop"
  ) |>
  mutate(PtsMoyen = TotalPts / NbJoueurs)

grille_hockey <- grille |>
  left_join(villes_cellule, by = "IdCellule") |>
  mutate(NbJoueurs = coalesce(NbJoueurs, 0L),
         TotalPts  = coalesce(TotalPts, 0))

grille_analyse <- grille_hockey |>
  filter(NbJoueurs >= MIN_JOUEURS, !is.na(PtsMoyen))

message("Taille de cellule : ", TAILLE_CELLULE / 1000, " km")
message("Cellules terrestres : ", nrow(grille_hockey))
message("Cellules retenues (>= ", MIN_JOUEURS, " joueurs) : ",
        nrow(grille_analyse))


# *****************************************************************************
# 3. CARTE DE LA PRODUCTION MOYENNE PAR CELLULE ----
# *****************************************************************************

carte_pts_grille <- tm_shape(amerique_nord) +
  tm_polygons(fill = "grey96", col = "grey70", lwd = 0.3) +
  tm_shape(grille_analyse) +
  tm_polygons(
    fill = "PtsMoyen",
    fill.scale = tm_scale_intervals(style = "quantile", n = 5,
                                    values = "brewer.purples"),
    fill.legend = tm_legend(title = "Points moyens\npar joueur"),
    col = "white", lwd = 0.2
  ) +
  tm_shape(unites) +
  tm_borders(col = "#4d4d4d41", lwd = 0.5, linetype = "solid") +
  tm_title("Production offensive moyenne par joueur selon le lieu de naissance") +
  tm_credits(
    paste0("Indicateur de calibre, indépendant de la population.",
           "\nCellules de ", MIN_JOUEURS, " joueurs et plus.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.6
  )

sauver_carte(carte_pts_grille, "07_carte_grille_pts_moyens.png")

message("=== 07 terminé ===")
