# =============================================================================
# 04_cartes_villes.R — Cartes par ville de naissance, mondiales et regionales
# =============================================================================
# ORIGINE : SECTION 3 de archive/Projet_Hockey_script_ORIGINAL.R.
#
# CE QUI A CHANGE PAR RAPPORT A L'ORIGINAL
#  - Le tableau villes_points / villes_points_geo etait reconstruit SIX fois a
#    l'identique dans le script d'origine. Il est desormais calcule une seule
#    fois par construire_villes_sf() (00_config.R) et mis en cache.
#  - Le bloc de vingt lignes qui produisait chaque carte regionale etait
#    recopie quatorze fois. Il est factorise dans carte_villes_region()
#    (00_config.R) et les regions sont decrites par une simple table.
#  - La carte de l'Ontario etait presente EN DOUBLE (deux blocs identiques
#    ecrivant le meme fichier). Le doublon est supprime.
#  - Les provinces des Prairies (Manitoba et Saskatchewan) n'avaient aucune
#    carte, alors que le module 06 montre que ce sont les unites au plus fort
#    taux de production par habitant du continent. La carte est ajoutee.
#  - Les regions sont extraites de CodeProv (calcule une fois dans la config)
#    plutot que par un str_detect() sur la chaine de caracteres a chaque carte.
#
# SORTIES : 18 cartes PNG dans figures/ (non versionnees).
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 04 — CARTES PAR VILLE DE NAISSANCE ===")

villes_sf <- construire_villes_sf()
monde     <- charger_monde("medium")

message("Localites geocodees : ", nrow(villes_sf))


# =============================================================================
# 1. CARTES MONDIALES
# =============================================================================

emprise_monde <- st_bbox(monde)

## Carte 1 : principales villes de naissance (top 50) ------------------------

carte_villes_region(
  villes  = villes_sf |> arrange(desc(NbJoueurs)) |> slice_head(n = 50),
  fond    = monde,
  emprise = emprise_monde,
  titre   = "Principales villes de naissance des joueurs de la LNH",
  fichier = "carte_top_villes.png",
  variable = "NbJoueurs",
  titre_legende = "Nombre de joueurs",
  echelle = 1.5,
  couleur = "red",
  note = "Les 50 localites ayant produit le plus de joueurs."
)

## Carte 2 : villes ayant produit des joueurs de 1000 points et plus ---------

carte_villes_region(
  villes  = villes_sf |> filter(NbElite > 0) |> arrange(desc(NbElite)),
  fond    = monde,
  emprise = emprise_monde,
  titre   = paste0("Villes ayant produit des joueurs de ", SEUIL_ELITE_PTS,
                   " points et plus"),
  fichier = "carte_villes_elite.png",
  variable = "NbElite",
  titre_legende = paste0("Joueurs ", SEUIL_ELITE_PTS, "+ pts"),
  echelle = 1.8,
  couleur = "red"
)

## Carte 3 : production offensive totale par ville (top 50) ------------------

carte_villes_region(
  villes  = villes_sf |> arrange(desc(TotalPts)) |> slice_head(n = 50),
  fond    = monde,
  emprise = emprise_monde,
  titre   = "Production offensive totale par ville de naissance",
  fichier = "carte_villes_points.png",
  echelle = 1.6,
  note = "Les 50 localites ayant produit le plus de points en carriere."
)


# =============================================================================
# 2. CARTES REGIONALES DU CANADA
# =============================================================================
# Fond de carte : provinces canadiennes (ne_states, package
# rnaturalearthhires). Emprise fixee a la main pour chaque province.

canada_prov <- charger_provinces_canada()

villes_canada <- villes_sf |> filter(Country == "Canada")

regions_canada <- list(
  list(
    codes   = "QC",
    titre   = "Quebec",
    fichier = "carte_villes_points_quebec.png",
    emprise = emprise_geo(-76.5, -67.0, 44.8, 49.5),
    echelle = 1.4
  ),
  list(
    codes   = "ON",
    titre   = "Ontario",
    fichier = "carte_villes_points_ontario.png",
    emprise = emprise_geo(-95.5, -74.0, 41.3, 57.5),
    echelle = 1.3
  ),
  list(
    codes   = "AB",
    titre   = "Alberta",
    fichier = "carte_villes_points_alberta.png",
    emprise = emprise_geo(-120.0, -109.0, 48.8, 60.5),
    echelle = 1.5
  ),
  list(
    codes   = "BC",
    titre   = "Colombie-Britannique",
    fichier = "carte_villes_points_bc.png",
    emprise = emprise_geo(-139.5, -113.5, 48.0, 60.5),
    echelle = 1.4
  ),
  list(
    # AJOUT : les Prairies etaient absentes du script d'origine, alors que la
    # Saskatchewan est l'unite au plus fort taux de production du continent
    # (voir sorties/06_resultats_normalisation.txt).
    codes   = c("MB", "SK"),
    titre   = "Manitoba et Saskatchewan",
    fichier = "carte_villes_points_prairies.png",
    emprise = emprise_geo(-110.5, -88.5, 48.5, 60.5),
    echelle = 1.5
  ),
  list(
    # NB = Nouveau-Brunswick ; NS = Nouvelle-Ecosse
    # PE / PEI = Ile-du-Prince-Edouard
    # NL / NFLD / NFL = Terre-Neuve-et-Labrador
    codes   = c("NB", "NS", "PE", "PEI", "NL", "NFLD", "NFL"),
    titre   = "les provinces atlantiques",
    fichier = "carte_villes_points_atlantique.png",
    emprise = emprise_geo(-68.5, -52.0, 43.0, 54.5),
    echelle = 1.7
  )
)

for (region in regions_canada) {
  carte_villes_region(
    villes  = villes_canada |>
      filter(CodeProv %in% region$codes) |>
      arrange(desc(TotalPts)),
    fond    = canada_prov,
    emprise = region$emprise,
    titre   = paste0("Production offensive totale par ville de naissance — ",
                     region$titre),
    fichier = region$fichier,
    echelle = region$echelle
  )
}


# =============================================================================
# 3. CARTES REGIONALES DES ETATS-UNIS
# =============================================================================

usa_states <- charger_etats_us()

# Table de correspondance Etat -> region, reprise telle quelle de l'original.
us_regions <- tibble::tibble(
  CodeProv = c(
    "ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA",
    "OH", "MI", "IN", "IL", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS",
    "DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL",
    "KY", "TN", "MS", "AL", "OK", "TX", "AR", "LA",
    "MT", "ID", "WY", "CO", "UT", "NV", "AZ", "NM",
    "WA", "OR", "CA",
    "AK", "HI"
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

villes_us <- villes_sf |>
  filter(Country == "USA") |>
  left_join(us_regions, by = "CodeProv")

# Controle de qualite : villes americaines sans region rattachee
sans_region <- villes_us |>
  st_drop_geometry() |>
  filter(is.na(RegionUS))

if (nrow(sans_region) > 0) {
  message("Villes americaines sans region (Etat non reconnu) : ",
          nrow(sans_region))
  print(utils::head(sans_region[, c("Birthplace", "CodeProv", "NbJoueurs")], 10))
}

regions_us <- list(
  list(cle = "Northeast",
       titre   = "le Nord-Est des Etats-Unis",
       fichier = "carte_villes_points_us_northeast.png",
       emprise = emprise_geo(-82.5, -66.0, 37.0, 48.8),
       echelle = 1.2),
  list(cle = "Midwest",
       titre   = "le Midwest des Etats-Unis",
       fichier = "carte_villes_points_us_midwest.png",
       emprise = emprise_geo(-105.5, -79.0, 36.0, 50.5),
       echelle = 1.2),
  list(cle = "South",
       titre   = "le Sud des Etats-Unis",
       fichier = "carte_villes_points_us_south.png",
       emprise = emprise_geo(-107.5, -74.0, 24.0, 39.5),
       echelle = 1.25),
  list(cle = "Mountain_Southwest",
       titre   = "la region Mountain / Southwest des Etats-Unis",
       fichier = "carte_villes_points_us_mountain_southwest.png",
       emprise = emprise_geo(-117.8, -100.0, 31.0, 49.5),
       echelle = 1.35),
  list(cle = "Pacific",
       titre   = "la cote Pacifique des Etats-Unis",
       fichier = "carte_villes_points_us_pacific.png",
       emprise = emprise_geo(-125.5, -114.0, 31.0, 49.5),
       echelle = 1.35),
  list(cle = "Alaska_Hawaii",
       titre   = "l'Alaska et Hawaii",
       fichier = "carte_villes_points_us_alaska_hawaii.png",
       emprise = emprise_geo(-180.0, -150.0, 18.0, 72.0),
       echelle = 1.5)
)

for (region in regions_us) {
  carte_villes_region(
    villes  = villes_us |>
      filter(RegionUS == region$cle) |>
      arrange(desc(TotalPts)),
    fond    = usa_states,
    emprise = region$emprise,
    titre   = paste0("Production offensive totale par ville de naissance dans ",
                     region$titre),
    fichier = region$fichier,
    echelle = region$echelle
  )
}


# =============================================================================
# 4. CARTES REGIONALES DE L'EUROPE ET DE LA RUSSIE
# =============================================================================

# Liste des pays europeens presents dans le jeu de donnees.
# La Russie est exclue ici : elle est cartographiee separement, en deux
# parties, parce qu'une seule carte du pays entier rendrait illisibles les
# villes de la partie europeenne ou se concentrent les joueurs.
PAYS_EUROPE_CARTE <- setdiff(PAYS_EUROPE, "Russia")
PAYS_NORDIQUES    <- c("Sweden", "Norway", "Denmark", "Finland")

villes_europe <- villes_sf |>
  filter(Country %in% PAYS_EUROPE_CARTE) |>
  arrange(desc(TotalPts))

message("Villes europeennes (hors Russie) : ", nrow(villes_europe))

carte_villes_region(
  villes  = villes_europe,
  fond    = monde,
  emprise = emprise_geo(-12.5, 35.0, 35.0, 72.0),
  titre   = "Production offensive totale par ville de naissance en Europe",
  fichier = "carte_villes_points_europe.png",
  echelle = 1.25,
  note = "Russie exclue : cartographiee separement."
)

carte_villes_region(
  villes  = villes_sf |>
    filter(Country %in% PAYS_NORDIQUES) |>
    arrange(desc(TotalPts)),
  fond    = monde,
  emprise = emprise_geo(4.0, 32.0, 54.0, 71.5),
  titre   = paste("Production offensive totale par ville de naissance dans",
                  "les pays nordiques"),
  fichier = "carte_villes_points_nordiques.png",
  echelle = 1.4
)

# La coupure a 60 degres de longitude est la limite conventionnelle entre la
# Russie europeenne et la Russie asiatique (chaine de l'Oural).
villes_russie <- villes_sf |> filter(Country == "Russia")

carte_villes_region(
  villes  = villes_russie |> filter(longitude <= 60) |> arrange(desc(TotalPts)),
  fond    = monde,
  emprise = emprise_geo(20.0, 65.0, 41.0, 72.0),
  titre   = paste("Production offensive totale par ville de naissance en",
                  "Russie europeenne"),
  fichier = "carte_villes_points_russie_europeenne.png",
  echelle = 1.35,
  note = "Coupure a 60 degres est (chaine de l'Oural)."
)

carte_villes_region(
  villes  = villes_russie |> filter(longitude > 60) |> arrange(desc(TotalPts)),
  fond    = monde,
  emprise = emprise_geo(55.0, 180.0, 40.0, 73.0),
  titre   = paste("Production offensive totale par ville de naissance en",
                  "Russie asiatique"),
  fichier = "carte_villes_points_russie_asiatique.png",
  echelle = 1.5,
  note = "Coupure a 60 degres est (chaine de l'Oural)."
)


message("=== 04 termine ===")
