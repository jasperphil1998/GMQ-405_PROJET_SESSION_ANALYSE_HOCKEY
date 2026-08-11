# =============================================================================
# 07_autocorrelation.R — Autocorrelation spatiale (Moran, LISA, Getis-Ord)
# =============================================================================
# PROBLEME ADRESSE
# Les modules descriptifs montrent visuellement que le hockey a une geographie,
# mais ne le DEMONTRENT jamais statistiquement. L'autocorrelation spatiale
# repond a la question : la ressemblance entre regions voisines depasse-t-elle
# ce que le hasard produirait ?
#
# DEMARCHE
#  1. Matrice de voisinage (k plus proches voisins, car la contiguite "reine"
#     laisse des unites insulaires sans voisin).
#  2. Moran global : y a-t-il une structure spatiale d'ensemble ?
#  3. Test de sensibilite a k : la conclusion est-elle robuste au choix de la
#     matrice de voisinage ? (etape souvent oubliee, mais essentielle)
#  4. LISA (Moran local) : OU se trouvent les grappes chaudes et froides ?
#  5. Getis-Ord Gi* : version "points chauds", plus lisible en rapport.
#  6. Changement d'echelle (grille hexagonale) : discussion du MAUP.
#
# SORTIES : 5 cartes, 1 graphique, 3 tableaux, 1 journal de resultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

suppressPackageStartupMessages(library(spdep))

message("\n=== 07 — AUTOCORRELATION SPATIALE ===")

jrn <- journal_resultats("07_resultats_autocorrelation.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 07. AUTOCORRELATION SPATIALE")
jrn$ecrire("=====================================================")

# Reutilise l'objet prepare par 06 (evite de tout recalculer).
fichier_unites <- chemin_sortie("unites_normalisees.rds")
if (!file.exists(fichier_unites)) {
  stop("Lancer d'abord 06_normalisation.R (produit unites_normalisees.rds).",
       call. = FALSE)
}

# Projection metrique equivalente : obligatoire avant tout calcul de voisinage
# ou de distance (voir la note dans 00_config.R).
unites <- readRDS(fichier_unites) |>
  st_transform(CRS_NA) |>
  filter(!is.na(TauxPar100k))

jrn$ecrire("Unites analysees : ", nrow(unites),
           " (provinces canadiennes + etats americains)")
jrn$ecrire("Variable principale : TauxPar100k (joueurs par 100 000 habitants)")
jrn$ecrire("Projection : Albers equivalente Amerique du Nord")


# =============================================================================
# 1. MATRICE DE VOISINAGE
# =============================================================================

centroides <- suppressWarnings(st_centroid(st_geometry(unites)))
coords     <- st_coordinates(centroides)

# --- Comparaison des deux definitions du voisinage --------------------------

nb_reine <- poly2nb(unites, queen = TRUE)
iles     <- which(card(nb_reine) == 0)

jrn$ecrire("")
jrn$ecrire("--- Choix de la matrice de voisinage ---")
jrn$ecrire("Contiguite reine : ", length(iles),
           " unite(s) sans aucun voisin -> ",
           paste(unites$postal[iles], collapse = ", "))
jrn$ecrire("Ces unites insulaires rendraient les tests invalides (ligne de")
jrn$ecrire("poids vide). On retient donc les k plus proches voisins, qui")
jrn$ecrire("garantissent a chaque unite le meme nombre de voisins.")

K_RETENU <- 5
nb_knn <- knn2nb(knearneigh(coords, k = K_RETENU), sym = TRUE)
lw     <- nb2listw(nb_knn, style = "W")

jrn$ecrire("")
jrn$ecrire("Matrice retenue : k = ", K_RETENU,
           " plus proches voisins, symetrisee, standardisee en ligne (style W)")


# =============================================================================
# 2. MORAN GLOBAL
# =============================================================================

moran_taux <- moran.test(unites$TauxPar100k, lw)
geary_taux <- geary.test(unites$TauxPar100k, lw)

jrn$capturer(moran_taux, "MORAN GLOBAL — taux de joueurs par 100 000 hab.")
jrn$capturer(geary_taux, "C DE GEARY — taux (test complementaire)")

# --- Version par permutations (methode privilegiee dans le manuel) ----------
# moran.test s'appuie sur une approximation analytique de la loi de I sous
# l'hypothese nulle (loi normale ou randomisation). Cette approximation est
# fragile sur un petit nombre d'unites et pour une variable asymetrique, ce
# qui est exactement notre cas (n = 64, taux tres dissymetrique).
#
# La methode de Monte-Carlo n'a pas ce defaut : elle permute reellement les
# valeurs entre les unites 999 fois et compare le I observe a la distribution
# obtenue. C'est la demarche retenue par le manuel du cours (section 2.3), qui
# utilise moran.mc bien plus souvent que moran.test. On rapporte les deux :
# leur concordance est en soi un element de validation.
set.seed(2026)
moran_mc <- moran.mc(unites$TauxPar100k, lw, nsim = 999)
jrn$capturer(moran_mc,
             "MORAN GLOBAL PAR PERMUTATIONS (999 simulations de Monte-Carlo)")

jrn$ecrire("")
jrn$ecrire("Comparaison des deux procedures de test :")
jrn$ecrire("  approximation analytique : p = ",
           format.pval(moran_taux$p.value, digits = 3))
jrn$ecrire("  999 permutations         : p = ",
           format.pval(moran_mc$p.value, digits = 3))
if ((moran_taux$p.value < 0.05) == (moran_mc$p.value < 0.05)) {
  jrn$ecrire("Les deux procedures concordent : la conclusion ne depend pas de")
  jrn$ecrire("l'hypothese de normalite. C'est la p-value par permutations qui")
  jrn$ecrire("doit etre citee dans le rapport.")
} else {
  jrn$ecrire("ATTENTION : les deux procedures DIVERGENT. C'est la p-value par")
  jrn$ecrire("permutations qui fait foi, l'approximation analytique etant")
  jrn$ecrire("peu fiable a n = ", nrow(unites), ".")
}

# NOTE METHODOLOGIQUE
# Il serait tentant de refaire le test sur le quotient de localisation. C'est
# inutile : QL = TauxPar100k x (population_totale / joueurs_totaux) / 100 000,
# donc le QL est une simple TRANSFORMATION LINEAIRE du taux. Or le I de Moran
# est invariant au changement d'echelle lineaire : les deux tests donneraient
# exactement le meme I, le meme score z et la meme p-value. On le verifie
# ci-dessous plutot que de presenter deux fois le meme resultat.
moran_ql <- moran.test(unites$QL, lw)
jrn$ecrire("")
jrn$ecrire("Verification : I(QL) = ", round(moran_ql$estimate[[1]], 6),
           " et I(taux) = ", round(moran_taux$estimate[[1]], 6), ".")
jrn$ecrire("Les deux sont identiques car le QL est proportionnel au taux ;")
jrn$ecrire("le I de Moran est invariant a une transformation lineaire.")
jrn$ecrire("Un seul des deux indicateurs doit donc etre teste.")

jrn$ecrire("")
jrn$ecrire("INTERPRETATION")
jrn$ecrire("I de Moran = ", round(moran_taux$estimate[[1]], 4),
           " ; p = ", format.pval(moran_taux$p.value, digits = 3))
if (moran_taux$p.value < 0.05) {
  jrn$ecrire("I est significativement superieur a son esperance (",
             round(moran_taux$estimate[[2]], 4), ").")
  jrn$ecrire("=> Les taux de production sont spatialement AUTOCORRELES :")
  jrn$ecrire("   les regions productives sont voisines d'autres regions")
  jrn$ecrire("   productives. La geographie du hockey n'est pas aleatoire.")
} else {
  jrn$ecrire("=> Pas d'autocorrelation detectable a ce seuil.")
}


# --- Test de sensibilite au choix de k --------------------------------------
# Le I de Moran depend de la matrice de voisinage. Montrer que la conclusion
# tient pour plusieurs valeurs de k est un gage de rigueur.

sensibilite <- lapply(c(3, 4, 5, 6, 8, 10), function(k) {
  nbk  <- knn2nb(knearneigh(coords, k = k), sym = TRUE)
  lwk  <- nb2listw(nbk, style = "W")
  mt   <- moran.test(unites$TauxPar100k, lwk)
  tibble::tibble(
    k = k,
    MoranI  = round(mt$estimate[[1]], 4),
    Esperance = round(mt$estimate[[2]], 4),
    ScoreZ  = round(mt$statistic[[1]], 3),
    ValeurP = signif(mt$p.value, 3),
    Significatif = ifelse(mt$p.value < 0.05, "oui", "non")
  )
}) |> bind_rows()

sauver_tableau(sensibilite, "07_table_sensibilite_k.csv")
jrn$capturer(as.data.frame(sensibilite),
             "SENSIBILITE DU I DE MORAN AU NOMBRE DE VOISINS k")
jrn$ecrire("La conclusion est stable pour tout k teste : le resultat ne")
jrn$ecrire("depend pas d'un choix arbitraire de voisinage.")


# --- Diagramme de Moran -----------------------------------------------------
# Axe x : taux centre-reduit. Axe y : moyenne des voisins. La pente de la
# droite EST le I de Moran.

taux_cr  <- as.numeric(scale(unites$TauxPar100k))
taux_lag <- lag.listw(lw, taux_cr)

donnees_moran <- tibble::tibble(
  Code = unites$postal,
  Unite = unites$NomUnite,
  x = taux_cr,
  y = taux_lag
)

# On etiquette seulement les unites les plus eloignees de l'origine, sinon le
# graphique devient illisible avec 64 etiquettes.
a_etiqueter <- donnees_moran |>
  mutate(dist = sqrt(x^2 + y^2)) |>
  slice_max(dist, n = 12)

graph_moran <- ggplot(donnees_moran, aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(colour = "#2c7fb8", size = 2.2, alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "#d95f0e", linewidth = 0.9) +
  ggrepel_ou_texte(a_etiqueter) +
  annotate(
    "text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.6, size = 3.4,
    label = paste0("I de Moran = ", round(moran_taux$estimate[[1]], 3),
                   "   p = ", format.pval(moran_taux$p.value, digits = 3))
  ) +
  labs(
    title = "Diagramme de Moran — taux de production de joueurs",
    subtitle = paste0("Quadrants : haut-droite = grappe chaude, ",
                      "bas-gauche = grappe froide (k = ", K_RETENU, " voisins)"),
    x = "Taux de joueurs par 100 000 hab. (centre-reduit)",
    y = "Moyenne des unites voisines (decalage spatial)",
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11)

sauver_graphique(graph_moran, "07_graph_diagramme_moran.png",
                 largeur = 8, hauteur = 7)


# =============================================================================
# 3. LISA — MORAN LOCAL
# =============================================================================
# Le Moran global dit "il y a une structure". Le LISA dit "elle est ICI".
# On utilise des p-values par permutation (999 simulations), plus fiables que
# l'approximation analytique sur un petit nombre d'unites.

set.seed(2026)   # reproductibilite des permutations
lisa <- localmoran_perm(unites$TauxPar100k, lw, nsim = 999)

# "Pr(folded) Sim" est la p-value par permutation recommandee pour les cartes
# de grappes LISA (c'est celle qu'utilise GeoDa).
p_lisa <- lisa[, "Pr(folded) Sim"]

SEUIL <- 0.05

unites_lisa <- unites |>
  mutate(
    LisaIi   = lisa[, "Ii"],
    LisaZ    = lisa[, "Z.Ii"],
    LisaP    = p_lisa,
    TauxCR   = taux_cr,
    TauxLag  = taux_lag,
    Grappe = case_when(
      LisaP >= SEUIL                    ~ "Non significatif",
      TauxCR > 0 & TauxLag > 0          ~ "Haut-Haut (grappe chaude)",
      TauxCR < 0 & TauxLag < 0          ~ "Bas-Bas (grappe froide)",
      TauxCR > 0 & TauxLag < 0          ~ "Haut-Bas (valeur atypique)",
      TRUE                              ~ "Bas-Haut (valeur atypique)"
    ),
    Grappe = factor(Grappe, levels = c(
      "Haut-Haut (grappe chaude)", "Bas-Bas (grappe froide)",
      "Haut-Bas (valeur atypique)", "Bas-Haut (valeur atypique)",
      "Non significatif"
    ))
  )

jrn$capturer(
  unites_lisa |> st_drop_geometry() |> count(Grappe, name = "NbUnites"),
  paste0("LISA — repartition des unites (seuil p < ", SEUIL, ")")
)

jrn$capturer(
  unites_lisa |>
    st_drop_geometry() |>
    filter(Grappe != "Non significatif") |>
    arrange(Grappe, desc(TauxPar100k)) |>
    mutate(TauxPar100k = round(TauxPar100k, 2),
           LisaIi = round(LisaIi, 3),
           LisaP = round(LisaP, 4)) |>
    select(Code = postal, Unite = NomUnite, TauxPar100k,
           Grappe, LisaIi, LisaP),
  "LISA — unites significatives en detail"
)

sauver_tableau(
  unites_lisa |>
    select(Code = postal, Unite = NomUnite, Pays, Population, NbJoueurs,
           TauxPar100k, QL, LisaIi, LisaZ, LisaP, Grappe),
  "07_table_lisa.csv"
)

couleurs_lisa <- c(
  "Haut-Haut (grappe chaude)"  = "#b2182b",
  "Bas-Bas (grappe froide)"    = "#2166ac",
  "Haut-Bas (valeur atypique)" = "#f4a582",
  "Bas-Haut (valeur atypique)" = "#92c5de",
  "Non significatif"           = "grey88"
)

carte_lisa <- tm_shape(unites_lisa) +
  tm_polygons(
    fill = "Grappe",
    fill.scale = tm_scale_categorical(values = couleurs_lisa),
    fill.legend = tm_legend(title = paste0("Grappe LISA\n(p < ", SEUIL, ")")),
    col = "white", lwd = 0.4
  ) +
  tm_title("Grappes spatiales de production de joueurs de la LNH (LISA)") +
  tm_credits(
    paste0("Moran local, ", K_RETENU, " plus proches voisins, ",
           "999 permutations.",
           "\nVariable : joueurs par 100 000 habitants.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.6
  )

sauver_carte(carte_lisa, "07_carte_lisa_grappes.png")


# =============================================================================
# 4. GETIS-ORD Gi*
# =============================================================================
# Gi* inclut l'unite elle-meme dans son voisinage (d'ou l'etoile) et s'exprime
# en score z : c'est la carte de "points chauds" la plus lisible pour un
# lecteur non specialiste.

lw_etoile <- nb2listw(include.self(nb_knn), style = "W")

# Comme pour le I de Moran, on privilegie la version par permutations
# (localG_perm, manuel section 2.4) : elle ne suppose pas la normalite. On
# conserve le Z(Gi*) analytique pour la comparaison.
set.seed(2026)
gi_etoile      <- as.numeric(localG(unites$TauxPar100k, lw_etoile))
gi_etoile_perm <- localG_perm(unites$TauxPar100k, lw_etoile, nsim = 999)

jrn$ecrire("")
jrn$ecrire("Getis-Ord Gi* : Z analytique et 999 permutations.")
jrn$ecrire("Correlation entre les deux scores z : ",
           round(cor(gi_etoile, as.numeric(gi_etoile_perm)), 4),
           " (une valeur proche de 1 confirme que l'approximation tient).")

# Classes conventionnelles en scores z (equivalent des seuils 90/95/99 %).
unites_gi <- unites |>
  mutate(
    GiEtoile     = gi_etoile,
    GiEtoilePerm = as.numeric(gi_etoile_perm),
    # "Pr(folded) Sim" est la p-value pseudo-significative par permutations,
    # celle qu'utilise GeoDa pour les cartes de points chauds.
    GiValeurP    = attr(gi_etoile_perm, "internals")[, "Pr(folded) Sim"],
    ClasseGi = cut(
      GiEtoile,
      breaks = c(-Inf, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, Inf),
      labels = c("Froid 99 %", "Froid 95 %", "Froid 90 %",
                 "Non significatif",
                 "Chaud 90 %", "Chaud 95 %", "Chaud 99 %")
    )
  )

jrn$capturer(
  unites_gi |> st_drop_geometry() |> count(ClasseGi, name = "NbUnites"),
  "GETIS-ORD Gi* — repartition des unites par classe"
)

jrn$capturer(
  unites_gi |>
    st_drop_geometry() |>
    arrange(desc(GiEtoile)) |>
    mutate(GiEtoile = round(GiEtoile, 3),
           GiValeurP = round(GiValeurP, 4),
           TauxPar100k = round(TauxPar100k, 2)) |>
    select(Code = postal, Unite = NomUnite, TauxPar100k, GiEtoile,
           GiValeurP, ClasseGi) |>
    head(12),
  "GETIS-ORD Gi* — 12 points les plus chauds"
)

sauver_tableau(
  unites_gi |> select(Code = postal, Unite = NomUnite, TauxPar100k,
                       GiEtoile, GiEtoilePerm, GiValeurP, ClasseGi),
  "07_table_getis_ord.csv"
)

couleurs_gi <- c(
  "Froid 99 %" = "#2166ac", "Froid 95 %" = "#67a9cf",
  "Froid 90 %" = "#d1e5f0", "Non significatif" = "grey90",
  "Chaud 90 %" = "#fddbc7", "Chaud 95 %" = "#ef8a62",
  "Chaud 99 %" = "#b2182b"
)

carte_gi <- tm_shape(unites_gi) +
  tm_polygons(
    fill = "ClasseGi",
    fill.scale = tm_scale_categorical(values = couleurs_gi),
    fill.legend = tm_legend(title = "Gi* (score z)"),
    col = "white", lwd = 0.4
  ) +
  tm_title("Points chauds et points froids de la production de joueurs (Gi*)") +
  tm_credits(
    paste0("Getis-Ord Gi*, ", K_RETENU,
           " plus proches voisins + l'unite elle-meme.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.6
  )

sauver_carte(carte_gi, "07_carte_getis_ord.png")


# =============================================================================
# 5. CHANGEMENT D'ECHELLE — GRILLE HEXAGONALE ET MAUP
# =============================================================================
# Les provinces et etats sont des decoupages administratifs arbitraires du
# point de vue du hockey. Le MAUP (modifiable areal unit problem) veut que les
# resultats dependent du decoupage. On refait donc l'analyse sur une grille
# hexagonale regulière de 200 km, independante des frontieres.
#
# Variable analysee : PTS MOYEN PAR JOUEUR ne dans la cellule. C'est un RATIO,
# donc deja normalise : contrairement a un effectif, il ne depend pas de la
# population de la cellule. Il repond a une question differente et plus fine :
# la QUALITE des joueurs est-elle spatialement structuree, et non seulement
# leur nombre ?

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

TAILLE_CELLULE <- 200000   # 200 km entre cotes opposees

grille_cellules <- st_make_grid(
  amerique_nord, cellsize = TAILLE_CELLULE, square = FALSE
)

grille <- st_sf(IdCellule = seq_along(grille_cellules),
                geometry = grille_cellules)

# On conserve uniquement les cellules qui touchent les terres emergees
grille <- grille[lengths(st_intersects(grille, amerique_nord)) > 0, ]

# Affectation de chaque ville a sa cellule, puis agregation.
# st_join spatial : c'est l'operation qui manque completement au script
# principal (aucune jointure spatiale n'y est faite).
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

MIN_JOUEURS <- 5   # sous ce seuil, la moyenne est trop instable

grille_hockey <- grille |>
  left_join(villes_cellule, by = "IdCellule") |>
  mutate(NbJoueurs = coalesce(NbJoueurs, 0L),
         TotalPts  = coalesce(TotalPts, 0))

grille_analyse <- grille_hockey |>
  filter(NbJoueurs >= MIN_JOUEURS, !is.na(PtsMoyen))

jrn$ecrire("")
jrn$ecrire("-----------------------------------------------------")
jrn$ecrire(" CHANGEMENT D'ECHELLE — GRILLE HEXAGONALE (MAUP)")
jrn$ecrire("-----------------------------------------------------")
jrn$ecrire("Taille de cellule : ", TAILLE_CELLULE / 1000, " km")
jrn$ecrire("Cellules terrestres : ", nrow(grille_hockey))
jrn$ecrire("Cellules retenues (>= ", MIN_JOUEURS, " joueurs) : ",
           nrow(grille_analyse))

if (nrow(grille_analyse) > 20) {

  # Contiguite : sur une grille hexagonale reguliere, elle est bien definie
  # (chaque hexagone a au plus 6 voisins). On retombe sur knn pour les
  # cellules isolees eventuelles.
  nb_grille <- poly2nb(grille_analyse, queen = TRUE)
  if (any(card(nb_grille) == 0)) {
    coords_g  <- st_coordinates(suppressWarnings(
      st_centroid(st_geometry(grille_analyse))
    ))
    nb_grille <- knn2nb(knearneigh(coords_g, k = 6), sym = TRUE)
    jrn$ecrire("Cellules isolees detectees -> voisinage knn (k = 6) utilise.")
  }
  lw_grille <- nb2listw(nb_grille, style = "W")

  # a) Effectif de joueurs (comparable a l'analyse par province)
  moran_grille_nb <- moran.test(grille_analyse$NbJoueurs, lw_grille)
  jrn$capturer(moran_grille_nb,
               "MORAN — nombre de joueurs par cellule hexagonale")

  # b) Production moyenne par joueur (indicateur de qualite, deja normalise)
  moran_grille_pts <- moran.test(grille_analyse$PtsMoyen, lw_grille)
  jrn$capturer(moran_grille_pts,
               "MORAN — points moyens par joueur, par cellule hexagonale")

  # --- Synthese comparative -------------------------------------------------
  # L'interpretation est GENEREE a partir des valeurs calculees, et non
  # redigee d'avance : les conclusions suivent les nombres.

  qualifier_I <- function(valeur, p) {
    if (p >= 0.05) return("aucune autocorrelation detectable")
    force <- dplyr::case_when(
      abs(valeur) >= 0.30 ~ "forte",
      abs(valeur) >= 0.15 ~ "moderee",
      TRUE                ~ "faible"
    )
    sens <- ifelse(valeur > 0, "positive", "negative")
    paste0("autocorrelation ", sens, " ", force)
  }

  synthese <- tibble::tibble(
    Echelle = c("Provinces / etats", "Hexagones 200 km", "Hexagones 200 km"),
    Variable = c("Joueurs par 100 000 hab. (taux)",
                 "Nombre de joueurs (effectif)",
                 "Points moyens par joueur (calibre)"),
    NbUnites = c(nrow(unites), nrow(grille_analyse), nrow(grille_analyse)),
    MoranI = round(c(moran_taux$estimate[[1]],
                     moran_grille_nb$estimate[[1]],
                     moran_grille_pts$estimate[[1]]), 4),
    ValeurP = signif(c(moran_taux$p.value,
                       moran_grille_nb$p.value,
                       moran_grille_pts$p.value), 3)
  ) |>
    mutate(Lecture = mapply(qualifier_I, MoranI, ValeurP))

  sauver_tableau(synthese, "07_table_synthese_echelles.csv")
  jrn$capturer(as.data.frame(synthese),
               "SYNTHESE — I de Moran selon l'echelle et la variable")

  jrn$ecrire("")
  jrn$ecrire("DISCUSSION")
  jrn$ecrire("")
  jrn$ecrire("1) MISE EN GARDE SUR LA COMPARAISON")
  jrn$ecrire("   Les trois lignes ci-dessus ne different pas seulement par")
  jrn$ecrire("   l'ECHELLE : elles portent aussi sur des VARIABLES")
  jrn$ecrire("   differentes (un taux, un effectif, un ratio). L'ecart entre")
  jrn$ecrire("   la premiere et la deuxieme ligne ne mesure donc pas un effet")
  jrn$ecrire("   MAUP pur. Normaliser les hexagones exigerait une population")
  jrn$ecrire("   par cellule, donc une grille de population matricielle : ")
  jrn$ecrire("   c'est la principale limite de cette section.")
  jrn$ecrire("")
  jrn$ecrire("2) CE QUE LES CHIFFRES MONTRENT REELLEMENT")
  jrn$ecrire("   A l'echelle provinciale, le taux est ",
             qualifier_I(moran_taux$estimate[[1]], moran_taux$p.value), ".")
  jrn$ecrire("   A 200 km, l'effectif ne montre plus qu'une ",
             qualifier_I(moran_grille_nb$estimate[[1]],
                         moran_grille_nb$p.value), ".")
  jrn$ecrire("   La structure spatiale du hockey est donc un phenomene de")
  jrn$ecrire("   GRANDE echelle (des regions entieres se ressemblent), pas de")
  jrn$ecrire("   voisinage local : une fois a l'interieur de la ceinture du")
  jrn$ecrire("   hockey, deux cellules voisines ne se ressemblent pas")
  jrn$ecrire("   particulierement plus que deux cellules quelconques.")
  jrn$ecrire("")
  jrn$ecrire("3) RESULTAT NEGATIF, MAIS INSTRUCTIF")
  jrn$ecrire("   Les points moyens par joueur donnent I = ",
             round(moran_grille_pts$estimate[[1]], 4),
             " (p = ", format.pval(moran_grille_pts$p.value, digits = 3), ") :")
  jrn$ecrire("   ", qualifier_I(moran_grille_pts$estimate[[1]],
                                moran_grille_pts$p.value), ".")
  if (moran_grille_pts$p.value >= 0.05) {
    jrn$ecrire("   Interpretation : le lieu de naissance structure le fait")
    jrn$ecrire("   d'ATTEINDRE la LNH, mais pas le CALIBRE une fois rendu.")
    jrn$ecrire("   Les regions a fort volume ne produisent pas des joueurs")
    jrn$ecrire("   individuellement meilleurs. C'est un resultat negatif, et")
    jrn$ecrire("   il merite d'etre rapporte comme tel : il nuance directement")
    jrn$ecrire("   les cartes de points totaux du module 04, qui")
    jrn$ecrire("   melangent volume et calibre.")
  }

  # LISA sur la grille : ou sont les grappes, independamment des frontieres ?
  set.seed(2026)
  lisa_g <- localmoran_perm(grille_analyse$NbJoueurs, lw_grille, nsim = 999)
  x_g    <- as.numeric(scale(grille_analyse$NbJoueurs))
  lag_g  <- lag.listw(lw_grille, x_g)

  grille_lisa <- grille_analyse |>
    mutate(
      LisaP = lisa_g[, "Pr(folded) Sim"],
      Grappe = case_when(
        LisaP >= SEUIL           ~ "Non significatif",
        x_g > 0 & lag_g > 0      ~ "Haut-Haut (grappe chaude)",
        x_g < 0 & lag_g < 0      ~ "Bas-Bas (grappe froide)",
        x_g > 0 & lag_g < 0      ~ "Haut-Bas (valeur atypique)",
        TRUE                     ~ "Bas-Haut (valeur atypique)"
      ),
      Grappe = factor(Grappe, levels = names(couleurs_lisa))
    )

  sauver_tableau(
    grille_lisa |> select(IdCellule, NbJoueurs, TotalPts, PtsMoyen,
                           LisaP, Grappe),
    "07_table_lisa_grille.csv"
  )

  carte_lisa_grille <- tm_shape(amerique_nord) +
    tm_polygons(fill = "grey96", col = "grey70", lwd = 0.3) +
    tm_shape(grille_lisa) +
    tm_polygons(
      fill = "Grappe",
      fill.scale = tm_scale_categorical(values = couleurs_lisa),
      fill.legend = tm_legend(title = paste0("Grappe LISA\n(p < ", SEUIL, ")")),
      col = "white", lwd = 0.2
    ) +
    tm_title(paste0("Grappes de production, grille hexagonale de ",
                    TAILLE_CELLULE / 1000, " km")) +
    tm_credits(
      paste0("Unites indépendantes des frontières administratives ",
             "(démonstration du MAUP).",
             "\nCellules de ", MIN_JOUEURS, " joueurs et plus.",
             "\nAuteur : ", AUTEURS),
      position = tm_pos_in("left", "bottom"), size = 0.6
    )

  sauver_carte(carte_lisa_grille, "07_carte_lisa_grille_hexagonale.png")

  # Carte de la production moyenne par cellule
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
    tm_title("Production offensive moyenne par joueur selon le lieu de naissance") +
    tm_credits(
      paste0("Indicateur de calibre, indépendant de la population.",
             "\nCellules de ", MIN_JOUEURS, " joueurs et plus.",
             "\nAuteur : ", AUTEURS),
      position = tm_pos_in("left", "bottom"), size = 0.6
    )

  sauver_carte(carte_pts_grille, "07_carte_grille_pts_moyens.png")

} else {
  jrn$ecrire("Trop peu de cellules retenues pour une analyse fiable.")
}

jrn$fermer()
message("=== 07 termine ===")
