# =============================================================================
# 06_normalisation.R — Taux de production et quotient de localisation
# =============================================================================
# PROBLEME ADRESSE
# Les modules descriptifs 03 et 04 cartographient des EFFECTIFS BRUTS (nombre de joueurs,
# points totaux). Or l'Ontario produit beaucoup de joueurs surtout parce que
# l'Ontario compte beaucoup d'habitants. Une carte d'effectifs bruts est donc
# largement une carte de la population : elle ne dit rien de la geographie
# propre du hockey.
#
# DEUX INDICATEURS NORMALISES
#  1. Taux de production : joueurs par 100 000 habitants.
#  2. Quotient de localisation (QL) :
#         QL = (joueurs_i / joueurs_total) / (population_i / population_total)
#     QL = 1  -> la region produit exactement sa "part demographique"
#     QL = 4  -> elle produit 4 fois plus de joueurs qu'attendu
#     QL < 1  -> sous-representation
#     Le QL est l'indicateur standard en analyse regionale (specialisation).
#
# SORTIES : 4 cartes, 3 tableaux, 1 journal de resultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 06 — NORMALISATION ===")

hockey <- charger_hockey()
jrn    <- journal_resultats("06_resultats_normalisation.txt")

jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 06. NORMALISATION")
jrn$ecrire(" Taux de production et quotient de localisation")
jrn$ecrire("=====================================================")


# =============================================================================
# PARTIE A — ECHELLE DES PAYS
# =============================================================================

monde <- ne_countries(scale = "medium", returnclass = "sf")

# Recodage identique au module 03 pour la jointure au fond de carte
joueurs_pays <- hockey |>
  mutate(
    Country_map = case_when(
      Country == "USA" ~ "United States of America",
      Country == "Czech Republic" ~ "Czechia",
      Country %in% c("England", "Scotland", "Wales",
                     "Northern Ireland", "United Kingdom") ~ "United Kingdom",
      TRUE ~ Country
    )
  ) |>
  group_by(Country_map) |>
  summarise(
    NbJoueurs = n(),
    TotalPts  = sum(Pts, na.rm = TRUE),
    NbElite   = sum(Pts >= 1000, na.rm = TRUE),
    .groups   = "drop"
  )

monde_norm <- monde |>
  left_join(joueurs_pays, by = c("name" = "Country_map")) |>
  mutate(
    NbJoueurs = coalesce(NbJoueurs, 0L),
    TotalPts  = coalesce(TotalPts, 0),
    NbElite   = coalesce(NbElite, 0L)
  )

# Totaux de reference calcules UNIQUEMENT sur les pays effectivement
# representes dans le fond de carte, pour que les parts se somment a 1.
total_joueurs_pays <- sum(monde_norm$NbJoueurs, na.rm = TRUE)
total_pop_pays     <- sum(monde_norm$pop_est,   na.rm = TRUE)

monde_norm <- monde_norm |>
  mutate(
    TauxParMillion = ifelse(pop_est > 0, NbJoueurs / pop_est * 1e6, NA_real_),
    QL = ifelse(
      pop_est > 0 & total_joueurs_pays > 0,
      (NbJoueurs / total_joueurs_pays) / (pop_est / total_pop_pays),
      NA_real_
    )
  )

# --- Tableau comparatif : rang brut vs rang normalise -----------------------
# C'est LE tableau qui montre l'apport de la normalisation.

table_pays_norm <- monde_norm |>
  st_drop_geometry() |>
  filter(NbJoueurs > 0) |>
  select(Pays = name, Population = pop_est,
         NbJoueurs, TotalPts, NbElite, TauxParMillion, QL) |>
  mutate(
    RangBrut = rank(-NbJoueurs, ties.method = "min"),
    RangTaux = rank(-TauxParMillion, ties.method = "min"),
    DeplacementRang = RangBrut - RangTaux
  ) |>
  arrange(desc(TauxParMillion))

sauver_tableau(table_pays_norm, "06_table_pays_normalise.csv")

jrn$capturer(
  table_pays_norm |>
    filter(NbJoueurs >= 20) |>
    mutate(across(c(TauxParMillion, QL), ~ round(.x, 2))) |>
    select(Pays, NbJoueurs, RangBrut, TauxParMillion, RangTaux,
           DeplacementRang, QL) |>
    head(20),
  paste0("PAYS : top 20 par TAUX (min. 20 joueurs) — ",
         "comparaison avec le rang brut")
)

jrn$ecrire("")
jrn$ecrire("LECTURE : un DeplacementRang positif signifie que le pays monte")
jrn$ecrire("dans le classement une fois la population prise en compte.")


# --- Carte A1 : taux de production par pays ---------------------------------

# On ne cartographie que les pays avec au moins 5 joueurs : sous ce seuil, le
# taux est domine par le bruit d'echantillonnage (un seul joueur ne dans un
# petit pays produit un taux aberrant).
monde_taux <- monde_norm |>
  mutate(TauxAffiche = ifelse(NbJoueurs >= 5, TauxParMillion, NA_real_))

carte_taux_pays <- tm_shape(monde_taux) +
  tm_polygons(
    fill = "TauxAffiche",
    fill.scale = tm_scale_intervals(
      style = "fisher", n = 6, values = "brewer.yl_gn_bu",
      value.na = "grey90", label.na = "Moins de 5 joueurs"
    ),
    fill.legend = tm_legend(title = "Joueurs par\nmillion d'habitants"),
    col = "grey40", lwd = 0.3
  ) +
  tm_title("Taux de production de joueurs de la LNH par pays") +
  tm_credits(
    paste0("Population : Natural Earth (pop_est). Pays avec au moins 5 joueurs.",
           "\nSource : Hockey DB / NHL player data",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.6
  )

sauver_carte(carte_taux_pays, "06_carte_taux_pays.png")


# --- Carte A2 : quotient de localisation par pays ---------------------------

# Palette divergente centree sur QL = 1 (la valeur d'equilibre). Les bornes
# sont fixees manuellement pour que la classe centrale encadre 1.
monde_ql <- monde_norm |>
  mutate(QLAffiche = ifelse(NbJoueurs >= 5, QL, NA_real_))

carte_ql_pays <- tm_shape(monde_ql) +
  tm_polygons(
    fill = "QLAffiche",
    fill.scale = tm_scale_intervals(
      breaks = c(0, 0.25, 0.5, 1, 2, 5, 10, Inf),
      values = "brewer.rd_yl_bu",
      value.na = "grey90", label.na = "Moins de 5 joueurs"
    ),
    fill.legend = tm_legend(
      title = "Quotient de\nlocalisation\n(1 = attendu)"
    ),
    col = "grey40", lwd = 0.3
  ) +
  tm_title("Quotient de localisation des joueurs de la LNH par pays") +
  tm_credits(
    paste0("QL = part des joueurs / part de la population mondiale.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.6
  )

sauver_carte(carte_ql_pays, "06_carte_ql_pays.png")


# =============================================================================
# PARTIE B — ECHELLE DES PROVINCES ET DES ETATS
# =============================================================================
# C'est l'echelle la plus parlante : elle isole les "chateaux d'eau" du hockey
# a l'interieur du Canada et des Etats-Unis.

# Extraction du code de province / etat au niveau du JOUEUR
hockey_prov <- hockey |>
  mutate(
    CodeProv = case_when(
      Country == "Canada" ~ str_match(Birthplace, ",\\s*([A-Z]{2}),\\s*Canada$")[, 2],
      Country == "USA"    ~ str_match(Birthplace, ",\\s*([A-Z]{2}),\\s*USA$")[, 2],
      TRUE ~ NA_character_
    )
  )

# Controle de qualite : combien de joueurs nord-americains non rattaches ?
non_rattaches <- hockey_prov |>
  filter(Country %in% c("Canada", "USA"), is.na(CodeProv))

jrn$ecrire("")
jrn$ecrire("-------------------------------------------------")
jrn$ecrire(" PROVINCES ET ETATS")
jrn$ecrire("-------------------------------------------------")
jrn$ecrire("Joueurs canadiens et americains : ",
           sum(hockey_prov$Country %in% c("Canada", "USA")))
jrn$ecrire("Non rattaches a une province/etat : ", nrow(non_rattaches))
if (nrow(non_rattaches) > 0) {
  jrn$capturer(
    non_rattaches |> count(Birthplace, sort = TRUE) |> head(10),
    "Lieux non rattaches (a corriger dans le geocodage si nombreux)"
  )
}

joueurs_par_prov <- hockey_prov |>
  filter(!is.na(CodeProv)) |>
  group_by(CodeProv) |>
  summarise(
    NbJoueurs = n(),
    TotalPts  = sum(Pts, na.rm = TRUE),
    NbElite   = sum(Pts >= 1000, na.rm = TRUE),
    PtsMoyen  = mean(Pts, na.rm = TRUE),
    .groups   = "drop"
  )

population <- read_csv(
  chemin_donnees("population_provinces_etats.csv"),
  show_col_types = FALSE
)

# Fond de carte : provinces canadiennes + etats americains
unites_geo <- ne_states(
  country = c("Canada", "United States of America"),
  returnclass = "sf"
) |>
  select(postal, name, admin, geometry)

unites <- unites_geo |>
  left_join(population, by = c("postal" = "CodeProv")) |>
  left_join(joueurs_par_prov, by = c("postal" = "CodeProv")) |>
  mutate(
    NbJoueurs = coalesce(NbJoueurs, 0L),
    TotalPts  = coalesce(TotalPts, 0),
    NbElite   = coalesce(NbElite, 0L)
  )

# Verification de la jointure population : toute unite sans population
# invaliderait le calcul du taux.
manquantes <- unites |> st_drop_geometry() |> filter(is.na(Population))
if (nrow(manquantes) > 0) {
  warning("Population manquante pour : ",
          paste(manquantes$postal, collapse = ", "))
  jrn$capturer(manquantes |> select(postal, name),
               "ATTENTION — unites sans population")
}

total_joueurs_na <- sum(unites$NbJoueurs, na.rm = TRUE)
total_pop_na     <- sum(unites$Population, na.rm = TRUE)

unites <- unites |>
  mutate(
    TauxPar100k = NbJoueurs / Population * 1e5,
    QL = (NbJoueurs / total_joueurs_na) / (Population / total_pop_na),
    PtsPar100k  = TotalPts / Population * 1e5
  )

table_unites_norm <- unites |>
  st_drop_geometry() |>
  select(Code = postal, Unite = NomUnite, Pays, Population,
         NbJoueurs, TotalPts, NbElite, TauxPar100k, QL, PtsPar100k) |>
  mutate(
    RangBrut = rank(-NbJoueurs, ties.method = "min"),
    RangTaux = rank(-TauxPar100k, ties.method = "min"),
    DeplacementRang = RangBrut - RangTaux
  ) |>
  arrange(desc(TauxPar100k))

sauver_tableau(table_unites_norm, "06_table_provinces_etats_normalise.csv")

jrn$capturer(
  table_unites_norm |>
    mutate(across(c(TauxPar100k, QL), ~ round(.x, 2))) |>
    select(Code, Unite, NbJoueurs, RangBrut, TauxPar100k, RangTaux,
           DeplacementRang, QL) |>
    head(15),
  "PROVINCES / ETATS : 15 premieres unites par TAUX"
)

jrn$capturer(
  table_unites_norm |>
    arrange(desc(NbJoueurs)) |>
    mutate(across(c(TauxPar100k, QL), ~ round(.x, 2))) |>
    select(Code, Unite, NbJoueurs, RangBrut, TauxPar100k, RangTaux,
           DeplacementRang, QL) |>
    head(15),
  "PROVINCES / ETATS : 15 premieres unites par EFFECTIF BRUT (comparaison)"
)

# Sauvegarde de l'objet spatial pour reutilisation par 07 et 10.
# Evite de refaire toute la preparation dans chaque script.
saveRDS(unites, chemin_sortie("unites_normalisees.rds"))
message("  -> unites_normalisees.rds (reutilise par 07 et 10)")


# --- Carte B1 : taux par province / etat ------------------------------------

# Projection : Albers equivalente d'Amerique du Nord. Indispensable pour une
# carte de taux (les aires doivent etre comparables visuellement).
unites_proj <- st_transform(unites, CRS_NA)

carte_taux_unites <- tm_shape(unites_proj) +
  tm_polygons(
    fill = "TauxPar100k",
    fill.scale = tm_scale_intervals(
      style = "fisher", n = 6, values = "brewer.yl_or_rd",
      value.na = "grey90", label.na = "Aucune donnee"
    ),
    fill.legend = tm_legend(title = "Joueurs par\n100 000 habitants"),
    col = "white", lwd = 0.4
  ) +
  tm_title("Taux de production de joueurs de la LNH par province et etat") +
  tm_credits(
    paste0("Population : Recensement 2021 (Canada) et Census 2020 (E.-U.).",
           "\nProjection : Albers equivalente Amerique du Nord.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.6
  )

sauver_carte(carte_taux_unites, "06_carte_taux_provinces_etats.png")


# --- Carte B2 : effectif brut vs taux (comparaison en facettes) -------------
# Une seule figure qui juxtapose les deux lectures : c'est l'illustration la
# plus efficace de l'effet de la normalisation.

unites_long <- bind_rows(
  unites_proj |>
    mutate(Valeur = NbJoueurs,
           Indicateur = "a) Effectif brut (nombre de joueurs)") |>
    select(Valeur, Indicateur, geometry),
  unites_proj |>
    mutate(Valeur = TauxPar100k,
           Indicateur = "b) Taux (joueurs par 100 000 hab.)") |>
    select(Valeur, Indicateur, geometry)
)

carte_comparaison <- tm_shape(unites_long) +
  tm_polygons(
    fill = "Valeur",
    fill.scale = tm_scale_intervals(
      style = "fisher", n = 6, values = "brewer.yl_or_rd",
      value.na = "grey90", label.na = "Aucune donnee"
    ),
    fill.legend = tm_legend(title = "Valeur", orientation = "landscape"),
    col = "white", lwd = 0.3
  ) +
  tm_facets(by = "Indicateur", ncol = 2, free.scales = TRUE) +
  tm_title("Effet de la normalisation par la population") +
  tm_credits(
    paste0("Auteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.5
  )

sauver_carte(carte_comparaison, "06_carte_comparaison_brut_taux.png",
             largeur = 14, hauteur = 6)


# --- Graphique B3 : diagramme du deplacement de rang ------------------------
# Visualise quelles unites gagnent ou perdent le plus au changement
# d'indicateur. Complement non cartographique utile au rapport.

top_deplacement <- table_unites_norm |>
  filter(NbJoueurs >= 20) |>
  slice_max(abs(DeplacementRang), n = 20) |>
  mutate(
    Unite = factor(Unite, levels = Unite[order(DeplacementRang)]),
    Sens = ifelse(DeplacementRang > 0, "Monte au taux", "Descend au taux")
  )

graph_deplacement <- ggplot(top_deplacement,
                            aes(x = DeplacementRang, y = Unite, fill = Sens)) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  scale_fill_manual(values = c("Monte au taux" = "#2c7fb8",
                               "Descend au taux" = "#d95f0e")) +
  labs(
    title = "Provinces et etats : deplacement de rang apres normalisation",
    subtitle = paste("Rang selon l'effectif brut moins rang selon le taux",
                     "par 100 000 habitants (unites de 20 joueurs et plus)"),
    x = "Deplacement de rang (positif = gagne des places au taux)",
    y = NULL, fill = NULL,
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

sauver_graphique(graph_deplacement, "06_graph_deplacement_rang.png",
                 largeur = 9, hauteur = 7)


jrn$fermer()
message("=== 06 termine ===")
