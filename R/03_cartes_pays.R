# *****************************************************************************
# 03_cartes_pays.R — Cartes mondiales à cercles proportionnels, par pays
# *****************************************************************************
#
# Deux cartes à cercles proportionnels :
#   1. nombre de joueurs de la LNH par pays de naissance ;
#   2. nombre de joueurs de 1000 points et plus par pays de naissance.
#
# SORTIES : 2 cartes PNG dans figures/ (non versionnées).
# *****************************************************************************

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 03 — CARTES PAR PAYS ===")

hockey <- charger_hockey()
monde  <- charger_monde("medium")


# --- Correspondance entre les noms de pays du jeu de données et ceux de
#     rnaturalearth. Sans ce recodage, les États-Unis, la Tchéquie et les
#     nations britanniques ne se joignent pas au fond de carte.

recoder_pays_carte <- function(pays) {
  case_when(
    pays == "USA"            ~ "United States of America",
    pays == "Czech Republic" ~ "Czechia",
    pays %in% c("England", "Scotland", "Wales",
                "Northern Ireland", "United Kingdom") ~ "United Kingdom",
    TRUE ~ pays
  )
}

agreger_par_pays <- function(donnees, nom_colonne) {
  donnees |>
    count(Country, name = "Effectif") |>
    mutate(Country_map = recoder_pays_carte(Country)) |>
    group_by(Country_map) |>
    summarise(Effectif = sum(Effectif), .groups = "drop") |>
    rename(!!nom_colonne := Effectif)
}


## 1. Nombre de joueurs par pays ---------------------------------------------

joueurs_pays <- agreger_par_pays(hockey, "NbJoueurs")

# Contrôle : quels pays du jeu de données ne trouvent pas leur équivalent ?
orphelins <- anti_join(joueurs_pays, st_drop_geometry(monde),
                       by = c("Country_map" = "name"))
if (nrow(orphelins) > 0) {
  message("Pays sans correspondance dans le fond de carte : ",
          paste(orphelins$Country_map, collapse = ", "))
} else {
  message("Tous les pays trouvent leur correspondance dans le fond de carte.")
}

monde_hockey <- monde |>
  left_join(joueurs_pays, by = c("name" = "Country_map")) |>
  mutate(NbJoueurs = coalesce(NbJoueurs, 0L))

# st_point_on_surface (et non st_centroid) garantit que le point tombe DANS
# le polygone, même pour un pays de forme concave comme la Norvège.
monde_hockey_points <- monde_hockey |>
  filter(NbJoueurs > 0) |>
  st_make_valid() |>
  st_point_on_surface()

carte_joueurs_pays <- tm_shape(monde_hockey) +
  tm_polygons(fill = "grey95", col = "grey60", lwd = 0.3) +
  tm_shape(monde_hockey_points) +
  tm_symbols(
    size = "NbJoueurs",
    size.scale = tm_scale_continuous(
      values.scale = 2.5,
      ticks = c(50, 100, 250, 500, 1000, 2500, 3000)
    ),
    size.legend = tm_legend(title = "Nombre de joueurs"),
    fill = "#2C7FB8", col = "grey20", lwd = 0.7, fill_alpha = 0.95
  ) +
  tm_title("Nombre de joueurs de la LNH par pays de naissance") +
  tm_credits(CREDITS, position = tm_pos_in("left", "bottom"), size = 0.7)

sauver_carte(carte_joueurs_pays, "carte_joueurs_pays_cercles.png")


## 2. Joueurs de 1000 points et plus par pays --------------------------------

elite_pays_carte <- hockey |>
  filter(Pts >= SEUIL_ELITE_PTS) |>
  agreger_par_pays("NbElite")

monde_elite <- monde |>
  left_join(elite_pays_carte, by = c("name" = "Country_map")) |>
  mutate(NbElite = coalesce(NbElite, 0L))

monde_elite_points <- monde_elite |>
  filter(NbElite > 0) |>
  st_make_valid() |>
  st_point_on_surface()

carte_elite_pays <- tm_shape(monde_elite) +
  tm_polygons(fill = "grey95", col = "grey60", lwd = 0.3) +
  tm_shape(monde_elite_points) +
  tm_symbols(
    size = "NbElite",
    size.scale = tm_scale_continuous(
      values.scale = 2.5,
      ticks = c(1, 5, 10, 25, 50, 100, 250)
    ),
    size.legend = tm_legend(
      title = paste0("Joueurs ", SEUIL_ELITE_PTS, "+ pts")
    ),
    fill = "#CB181D", col = "grey20", lwd = 0.5, fill_alpha = 0.8
  ) +
  tm_title(paste0("Joueurs de ", SEUIL_ELITE_PTS,
                  " points et plus par pays de naissance")) +
  tm_credits(CREDITS, position = tm_pos_in("left", "bottom"), size = 0.7)

sauver_carte(carte_elite_pays, "carte_elite_pays_cercles.png")


message("=== 03 terminé ===")
