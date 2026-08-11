# =============================================================================
# 10_modelisation.R — Modélisation spatiale (OLS, SAR/SEM, GWR)
# =============================================================================
# PROBLÈME TRAITÉ
# Les modules 06 à 09 DÉCRIVENT la géographie du hockey. Ce module cherche à
# l'EXPLIQUER : quels facteurs géographiques prédisent le taux de production de
# joueurs d'une province ou d'un état ?
#
# LA SÉQUENCE CANONIQUE EN ANALYSE SPATIALE
#  1. Modèle OLS classique.
#  2. Test de Moran sur les RÉSIDUS. S'ils sont autocorrélés, l'OLS est
#     invalide : ses écarts-types sont sous-estimés et ses tests trop
#     permissifs. C'est l'étape que l'on saute le plus souvent, et c'est
#     précisément celle qui justifie tout le reste.
#  3. Tests du multiplicateur de Lagrange pour choisir entre un modèle à
#     décalage spatial (lag) et un modèle à erreur spatiale (error).
#  4. Estimation du modèle spatial, comparaison par AIC.
#  5. Vérification : l'autocorrélation des résidus a-t-elle disparu ?
#  6. Contrôles de robustesse : modèle de comptage, puis GWR.
#
# VARIABLES EXPLICATIVES (toutes calculées hors ligne, sans téléchargement)
#  - Latitude du centroïde : approximation de la rigueur de l'hiver, donc de
#    la saison de glace naturelle.
#  - Distance à la côte : la continentalité accentue le froid hivernal.
#  - Densité de population : accès aux infrastructures et aux ligues.
#  - Distance à l'équipe de la LNH la plus proche : exposition au hockey pro.
#
# SORTIES : 3 figures, 4 tableaux, 1 journal de résultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

suppressPackageStartupMessages({
  library(spdep)
  library(spatialreg)
})

message("\n=== 10 — MODÉLISATION SPATIALE ===")

jrn <- journal_resultats("10_resultats_modelisation.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 10. MODÉLISATION SPATIALE")
jrn$ecrire("=====================================================")

fichier_unites <- chemin_sortie("unites_normalisees.rds")
if (!file.exists(fichier_unites)) {
  stop("Lancer d'abord 06_normalisation.R.", call. = FALSE)
}

unites <- readRDS(fichier_unites) |>
  st_transform(CRS_NA) |>
  filter(!is.na(Population), !is.na(TauxPar100k))


# =============================================================================
# 1. CONSTRUCTION DES VARIABLES EXPLICATIVES
# =============================================================================

centroides <- suppressWarnings(st_centroid(st_geometry(unites)))
coords     <- st_coordinates(centroides)

# --- Latitude (en degrés, donc via un retour en coordonnées géographiques) ---
centroides_geo <- st_transform(st_sfc(centroides, crs = CRS_NA), CRS_GEO)
unites$Latitude <- st_coordinates(centroides_geo)[, 2]

# --- Aire et densité de population ------------------------------------------
# L'aire est calculée dans une projection ÉQUIVALENTE : c'est la raison pour
# laquelle CRS_NA a été choisi (une projection conforme donnerait des aires
# fausses, avec une erreur croissante vers le nord).
unites$Aire_km2      <- as.numeric(st_area(unites)) / 1e6
unites$DensitePop    <- unites$Population / unites$Aire_km2

# --- Distance à la côte (continentalité) ------------------------------------
cote <- ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(CRS_NA) |>
  st_union()

unites$DistCote_km <- as.numeric(st_distance(centroides, cote)) / 1000

# --- Distance à l'équipe de la LNH la plus proche ---------------------------
equipes <- read_csv(chemin_donnees("equipes_lnh.csv"), show_col_types = FALSE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO) |>
  st_transform(CRS_NA)

matrice_distances  <- st_distance(centroides, equipes)
unites$DistLNH_km  <- apply(matrice_distances, 1, min) / 1000

# --- Variable dépendante ----------------------------------------------------
# Le taux varie de 0 à 47,8 : sa distribution est très asymétrique, ce qui
# violerait l'hypothèse de normalité des résidus. On passe donc au logarithme.
#
# PROBLÈME : 9 unités ont un taux de 0 (aucun joueur), et log(0) = -Inf. On
# ajoute une constante égale à la MOITIÉ DU PLUS PETIT TAUX NON NUL observé.
# Ce choix est courant, mais il reste arbitraire : c'est pourquoi la section 6
# refait l'estimation avec un modèle de comptage qui traite les zéros
# naturellement, sans aucune constante.
constante <- min(unites$TauxPar100k[unites$TauxPar100k > 0]) / 2
unites$LogTaux <- log(unites$TauxPar100k + constante)

nb_zeros <- sum(unites$TauxPar100k == 0)

jrn$ecrire("Unités : ", nrow(unites))
jrn$ecrire("Unités sans aucun joueur : ", nb_zeros)
jrn$ecrire("Constante ajoutée avant le log : ", signif(constante, 4))
jrn$ecrire("")
jrn$ecrire("Variable dépendante : log(joueurs par 100 000 hab. + constante)")

FORMULE <- LogTaux ~ Latitude + log(DensitePop) + DistCote_km + DistLNH_km

sauver_tableau(
  unites |>
    select(Code = postal, Unite = NomUnite, Pays, Population, NbJoueurs,
           TauxPar100k, LogTaux, Latitude, Aire_km2, DensitePop,
           DistCote_km, DistLNH_km),
  "10_table_variables_modele.csv"
)


# --- Diagnostic de colinéarité ----------------------------------------------
# Deux variables très corrélées rendent les coefficients instables et
# ininterprétables. On calcule le facteur d'inflation de la variance (VIF)
# sans dépendance externe : VIF_j = 1 / (1 - R2_j), où R2_j provient de la
# régression de X_j sur toutes les autres explicatives.

donnees_modele <- unites |>
  st_drop_geometry() |>
  mutate(LogDensitePop = log(DensitePop))

explicatives <- c("Latitude", "LogDensitePop", "DistCote_km", "DistLNH_km")

vif <- sapply(explicatives, function(v) {
  autres  <- setdiff(explicatives, v)
  formule <- as.formula(paste(v, "~", paste(autres, collapse = " + ")))
  r2      <- summary(lm(formule, data = donnees_modele))$r.squared
  1 / (1 - r2)
})

correlations <- cor(donnees_modele[, explicatives])

jrn$capturer(round(correlations, 3),
             "MATRICE DE CORRÉLATION DES VARIABLES EXPLICATIVES")
jrn$capturer(round(vif, 2), "FACTEURS D'INFLATION DE LA VARIANCE (VIF)")
jrn$ecrire("Règle usuelle : VIF > 5 signale une colinéarité problématique,")
jrn$ecrire("VIF > 10 la rend rédhibitoire.")
if (max(vif) > 5) {
  jrn$ecrire("ATTENTION : au moins une variable dépasse le seuil de 5.")
  jrn$ecrire("Les coefficients concernés doivent être interprétés avec")
  jrn$ecrire("prudence : leur signe peut s'inverser d'un échantillon à l'autre.")
} else {
  jrn$ecrire("Tous les VIF sont sous le seuil de 5 : pas de problème de")
  jrn$ecrire("colinéarité. Les coefficients sont interprétables séparément.")
}

sauver_tableau(
  tibble::tibble(Variable = names(vif), VIF = round(as.numeric(vif), 3)),
  "10_table_vif.csv"
)


# =============================================================================
# 2. MATRICE DE VOISINAGE
# =============================================================================
# Même choix que dans le module 07 (k plus proches voisins), pour que les
# résultats des deux modules soient directement comparables.

K_RETENU <- 5
nb <- knn2nb(knearneigh(coords, k = K_RETENU), sym = TRUE)
lw <- nb2listw(nb, style = "W")

jrn$ecrire("")
jrn$ecrire("Matrice de voisinage : k = ", K_RETENU,
           " plus proches voisins (identique au module 07)")


# =============================================================================
# 3. MODÈLE OLS ET DIAGNOSTIC SPATIAL
# =============================================================================

modele_ols <- lm(FORMULE, data = donnees_modele)

jrn$capturer(summary(modele_ols), "MODÈLE 1 — MOINDRES CARRÉS ORDINAIRES (OLS)")

# --- Test de Moran sur les résidus : l'étape décisive -----------------------
moran_residus <- lm.morantest(modele_ols, lw)
jrn$capturer(moran_residus, "TEST DE MORAN SUR LES RÉSIDUS DE L'OLS")

residus_autocorreles <- moran_residus$p.value < 0.05

jrn$ecrire("")
if (residus_autocorreles) {
  jrn$ecrire("VERDICT : les résidus de l'OLS sont spatialement autocorrélés")
  jrn$ecrire("(I = ", round(moran_residus$estimate[[1]], 4),
             ", p = ", format.pval(moran_residus$p.value, digits = 3), ").")
  jrn$ecrire("")
  jrn$ecrire("CONSÉQUENCE : l'OLS est INVALIDE ici. L'hypothèse")
  jrn$ecrire("d'indépendance des erreurs est violée, donc les écarts-types")
  jrn$ecrire("sont sous-estimés et les p-values trop optimistes. Il reste une")
  jrn$ecrire("structure spatiale que les variables explicatives ne captent")
  jrn$ecrire("pas. Il faut passer à un modèle spatial.")
} else {
  jrn$ecrire("VERDICT : les résidus de l'OLS ne sont pas autocorrélés.")
  jrn$ecrire("Les variables explicatives suffisent à absorber la structure")
  jrn$ecrire("spatiale ; un modèle spatial n'est pas indispensable.")
}


# --- Choix du type de modèle spatial ----------------------------------------
# Les tests du multiplicateur de Lagrange (rebaptisés tests du score de Rao
# dans les versions récentes de spdep) indiquent laquelle des deux formes
# spatiales est la plus appropriée. On gère les deux noms de fonction pour
# rester compatible avec différentes versions du package.

tests_lm <- tryCatch({
  if ("lm.RStests" %in% getNamespaceExports("spdep")) {
    spdep::lm.RStests(modele_ols, lw, test = "all")
  } else {
    spdep::lm.LMtests(modele_ols, lw, test = "all")
  }
}, error = function(e) NULL)

if (!is.null(tests_lm)) {
  jrn$capturer(summary(tests_lm),
               "TESTS DU MULTIPLICATEUR DE LAGRANGE (score de Rao)")
  jrn$ecrire("LECTURE : comparer les versions ROBUSTES (adjRSerr / adjRSlag).")
  jrn$ecrire("Celle qui est significative alors que l'autre ne l'est pas")
  jrn$ecrire("désigne la forme spatiale à retenir.")
} else {
  jrn$ecrire("")
  jrn$ecrire("Les tests LM ne sont pas disponibles dans cette version du")
  jrn$ecrire("package. On se rabat sur la comparaison des AIC ci-dessous,")
  jrn$ecrire("qui répond à la même question de façon plus fruste.")
}


# =============================================================================
# 4. MODÈLES SPATIAUX
# =============================================================================
# Deux spécifications concurrentes :
#
#  - MODÈLE À ERREUR SPATIALE (SEM) : y = XB + u, u = lambda*Wu + e
#    L'autocorrélation vient de facteurs OMIS et spatialement structurés
#    (culture du hockey, politiques provinciales, climat non mesure).
#
#  - MODÈLE À DÉCALAGE SPATIAL (SAR lag) : y = rho*Wy + XB + e
#    La valeur d'une unité dépend directement de celle de ses voisines
#    (effet de contagion, de diffusion).
#
# Le choix n'est pas que technique : les deux racontent une histoire
# différente sur le mécanisme à l'œuvre.

modele_sem <- errorsarlm(FORMULE, data = donnees_modele, listw = lw)
modele_lag <- lagsarlm(FORMULE,  data = donnees_modele, listw = lw)

jrn$capturer(summary(modele_sem), "MODÈLE 2 — ERREUR SPATIALE (SEM)")
jrn$capturer(summary(modele_lag), "MODÈLE 3 — DÉCALAGE SPATIAL (SAR lag)")

comparaison <- tibble::tibble(
  Modele = c("OLS", "Erreur spatiale (SEM)", "Décalage spatial (SAR lag)"),
  AIC = c(AIC(modele_ols), AIC(modele_sem), AIC(modele_lag)),
  LogVraisemblance = c(as.numeric(logLik(modele_ols)),
                       as.numeric(logLik(modele_sem)),
                       as.numeric(logLik(modele_lag)))
) |>
  mutate(
    AIC = round(AIC, 2),
    LogVraisemblance = round(LogVraisemblance, 2),
    EcartAIC = round(AIC - min(AIC), 2)
  ) |>
  arrange(AIC)

sauver_tableau(comparaison, "10_table_comparaison_modeles.csv")
jrn$capturer(as.data.frame(comparaison),
             "COMPARAISON DES MODÈLES (AIC le plus faible = meilleur)")

meilleur_nom <- comparaison$Modele[1]
meilleur <- switch(
  meilleur_nom,
  "OLS" = modele_ols,
  "Erreur spatiale (SEM)" = modele_sem,
  "Décalage spatial (SAR lag)" = modele_lag
)

jrn$ecrire("")
jrn$ecrire("Modèle retenu : ", meilleur_nom)

# --- Vérification : l'autocorrélation a-t-elle disparu ? --------------------
# C'est le contrôle qui valide (ou invalide) tout le raisonnement. Si les
# résidus du modèle spatial sont encore autocorrélés, le problème n'est pas
# résolu.

if (!identical(meilleur_nom, "OLS")) {
  moran_residus_spatial <- moran.test(residuals(meilleur), lw)
  jrn$capturer(moran_residus_spatial,
               paste0("TEST DE MORAN SUR LES RÉSIDUS DU MODÈLE RETENU (",
                      meilleur_nom, ")"))

  jrn$ecrire("")
  jrn$ecrire("CONTRÔLE FINAL")
  jrn$ecrire("Résidus OLS            : I = ",
             round(moran_residus$estimate[[1]], 4),
             " (p = ", format.pval(moran_residus$p.value, digits = 3), ")")
  jrn$ecrire("Résidus modèle spatial : I = ",
             round(moran_residus_spatial$estimate[[1]], 4),
             " (p = ", format.pval(moran_residus_spatial$p.value, digits = 3), ")")
  if (moran_residus_spatial$p.value >= 0.05) {
    jrn$ecrire("=> L'autocorrélation résiduelle a été ABSORBÉE. Le modèle")
    jrn$ecrire("   spatial est correctement spécifié de ce point de vue, et")
    jrn$ecrire("   ses coefficients sont interprétables.")
  } else {
    jrn$ecrire("=> De l'autocorrélation SUBSISTE. Il manque probablement une")
    jrn$ecrire("   variable explicative structurée spatialement, ou la")
    jrn$ecrire("   matrice de voisinage est mal spécifiée.")
  }
}


# --- Tableau de coefficients prêts à citer ----------------------------------
extraire_coefficients <- function(modele, nom) {
  co <- summary(modele)$Coef
  if (is.null(co)) co <- summary(modele)$coefficients
  tibble::tibble(
    Modele    = nom,
    Terme     = rownames(co),
    Estime    = round(co[, 1], 4),
    EcartType = round(co[, 2], 4),
    ValeurP   = signif(co[, 4], 3),
    Signif    = dplyr::case_when(
      co[, 4] < 0.001 ~ "***",
      co[, 4] < 0.01  ~ "**",
      co[, 4] < 0.05  ~ "*",
      co[, 4] < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )
}

coefficients_tous <- bind_rows(
  extraire_coefficients(modele_ols, "OLS"),
  extraire_coefficients(modele_sem, "SEM"),
  extraire_coefficients(modele_lag, "SAR lag")
)

sauver_tableau(coefficients_tous, "10_table_coefficients.csv")
jrn$capturer(as.data.frame(coefficients_tous),
             "COEFFICIENTS DES TROIS MODÈLES")


# --- Carte des résidus ------------------------------------------------------
# Cartographier les résidus est le complément visuel du test de Moran : on
# voit OÙ le modèle se trompe, et si ces erreurs se regroupent.

unites_residus <- unites |>
  mutate(
    ResidusOLS = as.numeric(residuals(modele_ols)),
    ResidusSpatial = as.numeric(residuals(meilleur))
  )

residus_long <- bind_rows(
  unites_residus |>
    mutate(Residu = ResidusOLS, Modele = "a) Résidus de l'OLS") |>
    select(Residu, Modele, geometry),
  unites_residus |>
    mutate(Residu = ResidusSpatial,
           Modele = paste0("b) Résidus du modèle spatial (", meilleur_nom, ")")) |>
    select(Residu, Modele, geometry)
)

limite <- max(abs(residus_long$Residu), na.rm = TRUE)

carte_residus <- tm_shape(residus_long) +
  tm_polygons(
    fill = "Residu",
    fill.scale = tm_scale_intervals(
      breaks = seq(-limite, limite, length.out = 8),
      midpoint = 0, values = "brewer.rd_bu"
    ),
    fill.legend = tm_legend(title = "Résidu", orientation = "landscape"),
    col = "white", lwd = 0.3
  ) +
  tm_facets(by = "Modele", ncol = 2) +
  tm_title("Résidus du modèle avant et après prise en compte de l'espace") +
  tm_credits(
    paste0("Des résidus regroupés spatialement signalent un modèle mal",
           " spécifié.\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.5
  )

sauver_carte(carte_residus, "10_carte_residus.png", largeur = 14, hauteur = 6)


# --- Graphique des coefficients ---------------------------------------------
coef_graphique <- coefficients_tous |>
  filter(Terme %in% c("Latitude", "log(DensitePop)",
                      "DistCote_km", "DistLNH_km")) |>
  mutate(
    Bas  = Estime - 1.96 * EcartType,
    Haut = Estime + 1.96 * EcartType
  )

graph_coefficients <- ggplot(coef_graphique,
                             aes(x = Estime, y = Terme, colour = Modele)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_pointrange(aes(xmin = Bas, xmax = Haut),
                  position = position_dodge(width = 0.55),
                  fatten = 2.5) +
  scale_colour_manual(values = c("OLS" = "#d95f0e", "SEM" = "#2c7fb8",
                                 "SAR lag" = "#31a354")) +
  labs(
    title = "Coefficients estimés selon la spécification du modèle",
    subtitle = paste("Barres = intervalle de confiance à 95 %.",
                     "Un intervalle qui croise 0 indique un effet",
                     "\nnon significatif. Noter le RÉTRÉCISSEMENT des effets",
                     "quand l'espace est pris en compte."),
    x = "Coefficient estimé", y = NULL, colour = "Modèle",
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

sauver_graphique(graph_coefficients, "10_graph_coefficients.png",
                 largeur = 9, hauteur = 6)


# =============================================================================
# 5. CONTRÔLE DE ROBUSTESSE — MODÈLE DE COMPTAGE
# =============================================================================
# La transformation logarithmique a exigé une constante arbitraire pour gérer
# les 9 unités à zéro joueur. Un modèle binomial négatif sur le NOMBRE de
# joueurs, avec log(population) en OFFSET, évite complètement ce problème :
# il modélise directement un taux, traite les zéros naturellement et tient
# compte de la surdispersion.
# Si les conclusions tiennent avec cette approche très différente, elles ne
# dépendent pas du choix de la constante.

if (requireNamespace("MASS", quietly = TRUE)) {

  modele_nb <- MASS::glm.nb(
    NbJoueurs ~ Latitude + log(DensitePop) + DistCote_km + DistLNH_km +
      offset(log(Population)),
    data = donnees_modele
  )

  jrn$capturer(summary(modele_nb),
               "MODÈLE 4 — BINOMIAL NÉGATIF AVEC OFFSET (contrôle de robustesse)")

  moran_nb <- moran.test(residuals(modele_nb, type = "pearson"), lw)
  jrn$capturer(moran_nb,
               "TEST DE MORAN SUR LES RÉSIDUS DE PEARSON DU MODÈLE DE COMPTAGE")

  # Comparaison des SIGNES entre les deux approches.
  #
  # NUANCE IMPORTANTE : un changement de signe n'a de sens que pour une
  # variable dont l'effet est SIGNIFICATIF. Un coefficient statistiquement
  # indistinguable de zéro peut basculer d'un signe à l'autre d'une
  # spécification à l'autre sans que cela révèle quoi que ce soit : son signe
  # n'est pas une information. On ne signale donc une instabilité que si la
  # variable est significative dans au moins un des deux modèles.
  termes <- c("Latitude", "log(DensitePop)", "DistCote_km", "DistLNH_km")

  p_ols <- summary(modele_ols)$coefficients[termes, 4]
  p_nb  <- summary(modele_nb)$coefficients[termes, 4]

  accord <- tibble::tibble(
    Variable    = termes,
    SigneOLS    = ifelse(sign(coef(modele_ols)[termes]) > 0, "+", "-"),
    ValeurP_OLS = signif(p_ols, 3),
    SigneBinNeg = ifelse(sign(coef(modele_nb)[termes]) > 0, "+", "-"),
    ValeurP_BinNeg = signif(p_nb, 3)
  ) |>
    mutate(
      MemeSigne = SigneOLS == SigneBinNeg,
      Significatif = p_ols < 0.05 | p_nb < 0.05,
      Verdict = dplyr::case_when(
        MemeSigne & Significatif ~ "Robuste (significatif, même signe)",
        MemeSigne & !Significatif ~ "Non significatif, signe stable",
        !MemeSigne & !Significatif ~ "Non significatif, signe instable (sans portée)",
        TRUE ~ "PROBLÈME : significatif mais signe instable"
      )
    )

  sauver_tableau(accord, "10_table_robustesse_signes.csv")
  jrn$capturer(as.data.frame(accord),
               "ROBUSTESSE — OLS SUR LOG DU TAUX vs BINOMIAL NÉGATIF SUR COMPTAGE")

  instables_reels <- accord |> filter(!MemeSigne, Significatif)
  robustes        <- accord |> filter(MemeSigne, Significatif)

  jrn$ecrire("")
  if (nrow(instables_reels) == 0) {
    jrn$ecrire("AUCUNE conclusion significative ne change de signe entre les")
    jrn$ecrire("deux spécifications. Les résultats établis ne dépendent donc")
    jrn$ecrire("pas de la constante ajoutée avant le logarithme.")
    if (nrow(robustes) > 0) {
      jrn$ecrire("Variables robustes : ",
                 paste(robustes$Variable, collapse = ", "), ".")
    }
    instables_sans_portee <- accord |> filter(!MemeSigne, !Significatif)
    if (nrow(instables_sans_portee) > 0) {
      jrn$ecrire("")
      jrn$ecrire("Les variables ",
                 paste(instables_sans_portee$Variable, collapse = " et "),
                 " changent de signe,")
      jrn$ecrire("mais elles ne sont significatives dans AUCUN modèle (p de ",
                 min(c(instables_sans_portee$ValeurP_OLS,
                       instables_sans_portee$ValeurP_BinNeg)), " à ",
                 max(c(instables_sans_portee$ValeurP_OLS,
                       instables_sans_portee$ValeurP_BinNeg)), ").")
      jrn$ecrire("Leur coefficient est indistinguable de zéro : son signe")
      jrn$ecrire("n'est pas une information et ce basculement est attendu.")
      jrn$ecrire("Il ne faut ni s'en inquiéter, ni en tirer d'interprétation.")
    }
  } else {
    jrn$ecrire("PROBLÈME RÉEL : ",
               paste(instables_reels$Variable, collapse = ", "),
               " est significatif mais change de signe.")
    jrn$ecrire("Cette conclusion est fragile et ne doit PAS être présentée")
    jrn$ecrire("comme un résultat établi dans le rapport.")
  }
}


# =============================================================================
# 6. CONTRÔLE DE ROBUSTESSE — RÉGRESSION GÉOGRAPHIQUEMENT PONDÉRÉE (GWR)
# =============================================================================
# La GWR estime un jeu de coefficients PAR unité : elle teste si la relation
# est stationnaire dans l'espace (l'effet du climat est-il le même partout ?).
#
# Elle est présentée ici comme un contrôle, et non comme le modèle final : avec
# seulement 64 unités très hétérogènes en taille, la GWR risque fortement le
# surajustement. On tranche par l'AICc, et on rapporte le verdict quel qu'il
# soit — y compris s'il est défavorable à la GWR.

if (requireNamespace("spgwr", quietly = TRUE)) {

  bande <- tryCatch(
    spgwr::gwr.sel(FORMULE, data = donnees_modele, coords = coords,
                   adapt = TRUE, verbose = FALSE),
    error = function(e) NULL
  )

  if (!is.null(bande)) {
    modele_gwr <- spgwr::gwr(FORMULE, data = donnees_modele, coords = coords,
                             adapt = bande, hatmatrix = TRUE)

    jrn$ecrire("")
    jrn$ecrire("-----------------------------------------------------")
    jrn$ecrire(" RÉGRESSION GÉOGRAPHIQUEMENT PONDÉRÉE (GWR)")
    jrn$ecrire("-----------------------------------------------------")
    jrn$ecrire("Fenêtre adaptative retenue : ", round(bande, 4),
               " (environ ", round(bande * nrow(donnees_modele)),
               " unités sur ", nrow(donnees_modele), ")")

    aicc_gwr <- modele_gwr$results$AICc
    aicc_ols <- AIC(modele_ols) +
      (2 * (length(coef(modele_ols)) + 1) *
         (length(coef(modele_ols)) + 2)) /
      (nrow(donnees_modele) - length(coef(modele_ols)) - 2)

    jrn$ecrire("Nombre effectif de paramètres : ",
               round(modele_gwr$results$edf, 1), " degrés de liberté résiduels")
    jrn$ecrire("AICc de la GWR : ", round(aicc_gwr, 2))
    jrn$ecrire("AICc de l'OLS  : ", round(aicc_ols, 2))
    jrn$ecrire("")

    if (aicc_gwr > aicc_ols) {
      jrn$ecrire("VERDICT : la GWR est REJETÉE. Son AICc est PLUS ÉLEVÉ que")
      jrn$ecrire("celui du modèle global, malgré une souplesse bien supérieure.")
      jrn$ecrire("La fenêtre optimale ne retient que quelques unités voisines,")
      jrn$ecrire("ce qui est le signe classique du surajustement : le modèle")
      jrn$ecrire("épouse le bruit local plutôt qu'une variation réelle.")
      jrn$ecrire("")
      jrn$ecrire("CONCLUSION : rien ne permet d'affirmer que la relation varie")
      jrn$ecrire("dans l'espace. Les provinces et états sont trop peu nombreux")
      jrn$ecrire("et trop hétérogènes en superficie pour une GWR. Les cartes")
      jrn$ecrire("de coefficients locaux ne sont donc PAS produites : elles")
      jrn$ecrire("seraient visuellement convaincantes mais statistiquement")
      jrn$ecrire("vides. Une GWR aurait du sens sur des unités plus fines et")
      jrn$ecrire("plus nombreuses (divisions de recensement, comtes).")
    } else {
      jrn$ecrire("VERDICT : la GWR améliore l'ajustement. La relation n'est")
      jrn$ecrire("donc pas stationnaire dans l'espace.")

      etendues <- sapply(explicatives, function(v) {
        colonne <- gsub("[()]", ".", v)
        if (colonne %in% names(modele_gwr$SDF)) {
          rg <- range(modele_gwr$SDF[[colonne]], na.rm = TRUE)
          paste0("[", round(rg[1], 3), " ; ", round(rg[2], 3), "]")
        } else NA_character_
      })
      jrn$capturer(data.frame(Variable = names(etendues),
                              EtendueLocale = as.character(etendues)),
                   "ÉTENDUE DES COEFFICIENTS LOCAUX")
    }
  }
}


# =============================================================================
# 7. SYNTHÈSE INTERPRÉTATIVE
# =============================================================================

jrn$ecrire("")
jrn$ecrire("=====================================================")
jrn$ecrire(" SYNTHÈSE")
jrn$ecrire("=====================================================")

coef_final <- summary(meilleur)$Coef
if (is.null(coef_final)) coef_final <- summary(meilleur)$coefficients

for (terme in c("Latitude", "log(DensitePop)", "DistCote_km", "DistLNH_km")) {
  if (terme %in% rownames(coef_final)) {
    est <- coef_final[terme, 1]
    p   <- coef_final[terme, 4]
    sens <- ifelse(est > 0, "AUGMENTE", "DIMINUE")
    verdict <- ifelse(p < 0.05, "significatif", "NON significatif")
    jrn$ecrire("- ", terme, " : le taux ", sens,
               " avec cette variable (", verdict,
               ", p = ", format.pval(p, digits = 3), ")")
  }
}

jrn$ecrire("")
jrn$ecrire("LIMITES À ÉNONCER DANS LE RAPPORT")
jrn$ecrire("1. Le lieu de NAISSANCE n'est pas le lieu de DÉVELOPPEMENT. Un")
jrn$ecrire("   joueur né à Toronto et formé en Saskatchewan est compté comme")
jrn$ecrire("   torontois. Ce biais est structurel : aucune méthode statistique")
jrn$ecrire("   ne peut le corriger avec les seules données disponibles.")
jrn$ecrire("2. La latitude est une approximation grossière du climat. Une")
jrn$ecrire("   température moyenne de janvier, extraite d'une couche")
jrn$ecrire("   matricielle (WorldClim), serait bien plus directe.")
jrn$ecrire("3. La distance à la côte traite la baie d'Hudson comme un océan")
jrn$ecrire("   modérateur, alors qu'elle est gelée une bonne partie de")
jrn$ecrire("   l'année. La continentalité du Manitoba est donc sous-estimée.")
jrn$ecrire("4. n = ", nrow(unites), " unités seulement. C'est peu pour une")
jrn$ecrire("   régression à 4 variables explicatives, et cela limite la")
jrn$ecrire("   puissance de tous les tests.")
jrn$ecrire("5. Corrélation n'est pas causalité : ces modèles décrivent des")
jrn$ecrire("   associations spatiales, ils ne démontrent aucun mécanisme.")

jrn$fermer()
message("=== 10 terminé ===")
