# =============================================================================
# 10_modelisation.R — Modelisation spatiale (OLS, SAR/SEM, GWR)
# =============================================================================
# PROBLEME ADRESSE
# Les modules 06 a 09 DECRIVENT la geographie du hockey. Ce module cherche a
# l'EXPLIQUER : quels facteurs geographiques predisent le taux de production de
# joueurs d'une province ou d'un etat ?
#
# LA SEQUENCE CANONIQUE EN ANALYSE SPATIALE
#  1. Modele OLS classique.
#  2. Test de Moran sur les RESIDUS. S'ils sont autocorreles, l'OLS est
#     invalide : ses ecarts-types sont sous-estimes et ses tests trop
#     permissifs. C'est l'etape que l'on saute le plus souvent, et c'est
#     precisement celle qui justifie tout le reste.
#  3. Tests du multiplicateur de Lagrange pour choisir entre un modele a
#     decalage spatial (lag) et un modele a erreur spatiale (error).
#  4. Estimation du modele spatial, comparaison par AIC.
#  5. Verification : l'autocorrelation des residus a-t-elle disparu ?
#  6. Controles de robustesse : modele de comptage, puis GWR.
#
# VARIABLES EXPLICATIVES (toutes calculees hors ligne, sans telechargement)
#  - Latitude du centroide : approximation de la rigueur de l'hiver, donc de
#    la saison de glace naturelle.
#  - Distance a la cote : la continentalite accentue le froid hivernal.
#  - Densite de population : acces aux infrastructures et aux ligues.
#  - Distance a l'equipe de la LNH la plus proche : exposition au hockey pro.
#
# SORTIES : 3 figures, 4 tableaux, 1 journal de resultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

suppressPackageStartupMessages({
  library(spdep)
  library(spatialreg)
})

message("\n=== 10 — MODELISATION SPATIALE ===")

jrn <- journal_resultats("10_resultats_modelisation.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 10. MODELISATION SPATIALE")
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

# --- Latitude (en degres, donc via un retour en coordonnees geographiques) ---
centroides_geo <- st_transform(st_sfc(centroides, crs = CRS_NA), CRS_GEO)
unites$Latitude <- st_coordinates(centroides_geo)[, 2]

# --- Aire et densite de population ------------------------------------------
# L'aire est calculee dans une projection EQUIVALENTE : c'est la raison pour
# laquelle CRS_NA a ete choisi (une projection conforme donnerait des aires
# fausses, avec une erreur croissante vers le nord).
unites$Aire_km2      <- as.numeric(st_area(unites)) / 1e6
unites$DensitePop    <- unites$Population / unites$Aire_km2

# --- Distance a la cote (continentalite) ------------------------------------
cote <- ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(CRS_NA) |>
  st_union()

unites$DistCote_km <- as.numeric(st_distance(centroides, cote)) / 1000

# --- Distance a l'equipe de la LNH la plus proche ---------------------------
equipes <- read_csv(chemin_donnees("equipes_lnh.csv"), show_col_types = FALSE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS_GEO) |>
  st_transform(CRS_NA)

matrice_distances  <- st_distance(centroides, equipes)
unites$DistLNH_km  <- apply(matrice_distances, 1, min) / 1000

# --- Variable dependante ----------------------------------------------------
# Le taux varie de 0 a 47,8 : sa distribution est tres asymetrique, ce qui
# violerait l'hypothese de normalite des residus. On passe donc au logarithme.
#
# PROBLEME : 9 unites ont un taux de 0 (aucun joueur), et log(0) = -Inf. On
# ajoute une constante egale a la MOITIE DU PLUS PETIT TAUX NON NUL observe.
# Ce choix est courant, mais il reste arbitraire : c'est pourquoi la section 6
# refait l'estimation avec un modele de comptage qui traite les zeros
# naturellement, sans aucune constante.
constante <- min(unites$TauxPar100k[unites$TauxPar100k > 0]) / 2
unites$LogTaux <- log(unites$TauxPar100k + constante)

nb_zeros <- sum(unites$TauxPar100k == 0)

jrn$ecrire("Unites : ", nrow(unites))
jrn$ecrire("Unites sans aucun joueur : ", nb_zeros)
jrn$ecrire("Constante ajoutee avant le log : ", signif(constante, 4))
jrn$ecrire("")
jrn$ecrire("Variable dependante : log(joueurs par 100 000 hab. + constante)")

FORMULE <- LogTaux ~ Latitude + log(DensitePop) + DistCote_km + DistLNH_km

sauver_tableau(
  unites |>
    select(Code = postal, Unite = NomUnite, Pays, Population, NbJoueurs,
           TauxPar100k, LogTaux, Latitude, Aire_km2, DensitePop,
           DistCote_km, DistLNH_km),
  "10_table_variables_modele.csv"
)


# --- Diagnostic de colinearite ----------------------------------------------
# Deux variables tres correlees rendent les coefficients instables et
# ininterpretables. On calcule le facteur d'inflation de la variance (VIF)
# sans dependance externe : VIF_j = 1 / (1 - R2_j), ou R2_j provient de la
# regression de X_j sur toutes les autres explicatives.

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
             "MATRICE DE CORRELATION DES VARIABLES EXPLICATIVES")
jrn$capturer(round(vif, 2), "FACTEURS D'INFLATION DE LA VARIANCE (VIF)")
jrn$ecrire("Regle usuelle : VIF > 5 signale une colinearite problematique,")
jrn$ecrire("VIF > 10 la rend redhibitoire.")
if (max(vif) > 5) {
  jrn$ecrire("ATTENTION : au moins une variable depasse le seuil de 5.")
  jrn$ecrire("Les coefficients concernes doivent etre interpretes avec")
  jrn$ecrire("prudence : leur signe peut s'inverser d'un echantillon a l'autre.")
} else {
  jrn$ecrire("Tous les VIF sont sous le seuil de 5 : pas de probleme de")
  jrn$ecrire("colinearite. Les coefficients sont interpretables separement.")
}

sauver_tableau(
  tibble::tibble(Variable = names(vif), VIF = round(as.numeric(vif), 3)),
  "10_table_vif.csv"
)


# =============================================================================
# 2. MATRICE DE VOISINAGE
# =============================================================================
# Meme choix que dans le module 07 (k plus proches voisins), pour que les
# resultats des deux modules soient directement comparables.

K_RETENU <- 5
nb <- knn2nb(knearneigh(coords, k = K_RETENU), sym = TRUE)
lw <- nb2listw(nb, style = "W")

jrn$ecrire("")
jrn$ecrire("Matrice de voisinage : k = ", K_RETENU,
           " plus proches voisins (identique au module 07)")


# =============================================================================
# 3. MODELE OLS ET DIAGNOSTIC SPATIAL
# =============================================================================

modele_ols <- lm(FORMULE, data = donnees_modele)

jrn$capturer(summary(modele_ols), "MODELE 1 — MOINDRES CARRES ORDINAIRES (OLS)")

# --- Test de Moran sur les residus : l'etape decisive -----------------------
moran_residus <- lm.morantest(modele_ols, lw)
jrn$capturer(moran_residus, "TEST DE MORAN SUR LES RESIDUS DE L'OLS")

residus_autocorreles <- moran_residus$p.value < 0.05

jrn$ecrire("")
if (residus_autocorreles) {
  jrn$ecrire("VERDICT : les residus de l'OLS sont spatialement autocorreles")
  jrn$ecrire("(I = ", round(moran_residus$estimate[[1]], 4),
             ", p = ", format.pval(moran_residus$p.value, digits = 3), ").")
  jrn$ecrire("")
  jrn$ecrire("CONSEQUENCE : l'OLS est INVALIDE ici. L'hypothese")
  jrn$ecrire("d'independance des erreurs est violee, donc les ecarts-types")
  jrn$ecrire("sont sous-estimes et les p-values trop optimistes. Il reste une")
  jrn$ecrire("structure spatiale que les variables explicatives ne captent")
  jrn$ecrire("pas. Il faut passer a un modele spatial.")
} else {
  jrn$ecrire("VERDICT : les residus de l'OLS ne sont pas autocorreles.")
  jrn$ecrire("Les variables explicatives suffisent a absorber la structure")
  jrn$ecrire("spatiale ; un modele spatial n'est pas indispensable.")
}


# --- Choix du type de modele spatial ----------------------------------------
# Les tests du multiplicateur de Lagrange (rebaptises tests du score de Rao
# dans les versions recentes de spdep) indiquent laquelle des deux formes
# spatiales est la plus appropriee. On gere les deux noms de fonction pour
# rester compatible avec differentes versions du package.

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
  jrn$ecrire("designe la forme spatiale a retenir.")
} else {
  jrn$ecrire("")
  jrn$ecrire("Les tests LM ne sont pas disponibles dans cette version du")
  jrn$ecrire("package. On se rabat sur la comparaison des AIC ci-dessous,")
  jrn$ecrire("qui repond a la meme question de facon plus fruste.")
}


# =============================================================================
# 4. MODELES SPATIAUX
# =============================================================================
# Deux specifications concurrentes :
#
#  - MODELE A ERREUR SPATIALE (SEM) : y = XB + u, u = lambda*Wu + e
#    L'autocorrelation vient de facteurs OMIS et spatialement structures
#    (culture du hockey, politiques provinciales, climat non mesure).
#
#  - MODELE A DECALAGE SPATIAL (SAR lag) : y = rho*Wy + XB + e
#    La valeur d'une unite depend directement de celle de ses voisines
#    (effet de contagion, de diffusion).
#
# Le choix n'est pas que technique : les deux racontent une histoire
# differente sur le mecanisme a l'oeuvre.

modele_sem <- errorsarlm(FORMULE, data = donnees_modele, listw = lw)
modele_lag <- lagsarlm(FORMULE,  data = donnees_modele, listw = lw)

jrn$capturer(summary(modele_sem), "MODELE 2 — ERREUR SPATIALE (SEM)")
jrn$capturer(summary(modele_lag), "MODELE 3 — DECALAGE SPATIAL (SAR lag)")

comparaison <- tibble::tibble(
  Modele = c("OLS", "Erreur spatiale (SEM)", "Decalage spatial (SAR lag)"),
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
             "COMPARAISON DES MODELES (AIC le plus faible = meilleur)")

meilleur_nom <- comparaison$Modele[1]
meilleur <- switch(
  meilleur_nom,
  "OLS" = modele_ols,
  "Erreur spatiale (SEM)" = modele_sem,
  "Decalage spatial (SAR lag)" = modele_lag
)

jrn$ecrire("")
jrn$ecrire("Modele retenu : ", meilleur_nom)

# --- Verification : l'autocorrelation a-t-elle disparu ? --------------------
# C'est le controle qui valide (ou invalide) tout le raisonnement. Si les
# residus du modele spatial sont encore autocorreles, le probleme n'est pas
# resolu.

if (!identical(meilleur_nom, "OLS")) {
  moran_residus_spatial <- moran.test(residuals(meilleur), lw)
  jrn$capturer(moran_residus_spatial,
               paste0("TEST DE MORAN SUR LES RESIDUS DU MODELE RETENU (",
                      meilleur_nom, ")"))

  jrn$ecrire("")
  jrn$ecrire("CONTROLE FINAL")
  jrn$ecrire("Residus OLS            : I = ",
             round(moran_residus$estimate[[1]], 4),
             " (p = ", format.pval(moran_residus$p.value, digits = 3), ")")
  jrn$ecrire("Residus modele spatial : I = ",
             round(moran_residus_spatial$estimate[[1]], 4),
             " (p = ", format.pval(moran_residus_spatial$p.value, digits = 3), ")")
  if (moran_residus_spatial$p.value >= 0.05) {
    jrn$ecrire("=> L'autocorrelation residuelle a ete ABSORBEE. Le modele")
    jrn$ecrire("   spatial est correctement specifie de ce point de vue, et")
    jrn$ecrire("   ses coefficients sont interpretables.")
  } else {
    jrn$ecrire("=> De l'autocorrelation SUBSISTE. Il manque probablement une")
    jrn$ecrire("   variable explicative structuree spatialement, ou la")
    jrn$ecrire("   matrice de voisinage est mal specifiee.")
  }
}


# --- Tableau de coefficients pretes a citer ---------------------------------
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
             "COEFFICIENTS DES TROIS MODELES")


# --- Carte des residus ------------------------------------------------------
# Cartographier les residus est le complement visuel du test de Moran : on
# voit OU le modele se trompe, et si ces erreurs se regroupent.

unites_residus <- unites |>
  mutate(
    ResidusOLS = as.numeric(residuals(modele_ols)),
    ResidusSpatial = as.numeric(residuals(meilleur))
  )

residus_long <- bind_rows(
  unites_residus |>
    mutate(Residu = ResidusOLS, Modele = "a) Residus de l'OLS") |>
    select(Residu, Modele, geometry),
  unites_residus |>
    mutate(Residu = ResidusSpatial,
           Modele = paste0("b) Residus du modele spatial (", meilleur_nom, ")")) |>
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
    fill.legend = tm_legend(title = "Residu", orientation = "landscape"),
    col = "white", lwd = 0.3
  ) +
  tm_facets(by = "Modele", ncol = 2) +
  tm_title("Residus du modele avant et apres prise en compte de l'espace") +
  tm_credits(
    paste0("Des residus regroupes spatialement signalent un modele mal",
           " specifie.\nAuteur : ", AUTEURS),
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
    title = "Coefficients estimes selon la specification du modele",
    subtitle = paste("Barres = intervalle de confiance a 95 %.",
                     "Un intervalle qui croise 0 indique un effet",
                     "\nnon significatif. Noter le RETRECISSEMENT des effets",
                     "quand l'espace est pris en compte."),
    x = "Coefficient estime", y = NULL, colour = "Modele",
    caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

sauver_graphique(graph_coefficients, "10_graph_coefficients.png",
                 largeur = 9, hauteur = 6)


# =============================================================================
# 5. CONTROLE DE ROBUSTESSE — MODELE DE COMPTAGE
# =============================================================================
# La transformation logarithmique a exige une constante arbitraire pour gerer
# les 9 unites a zero joueur. Un modele binomial negatif sur le NOMBRE de
# joueurs, avec log(population) en OFFSET, evite completement ce probleme :
# il modelise directement un taux, traite les zeros naturellement et tient
# compte de la surdispersion.
# Si les conclusions tiennent avec cette approche tres differente, elles ne
# dependent pas du choix de la constante.

if (requireNamespace("MASS", quietly = TRUE)) {

  modele_nb <- MASS::glm.nb(
    NbJoueurs ~ Latitude + log(DensitePop) + DistCote_km + DistLNH_km +
      offset(log(Population)),
    data = donnees_modele
  )

  jrn$capturer(summary(modele_nb),
               "MODELE 4 — BINOMIAL NEGATIF AVEC OFFSET (controle de robustesse)")

  moran_nb <- moran.test(residuals(modele_nb, type = "pearson"), lw)
  jrn$capturer(moran_nb,
               "TEST DE MORAN SUR LES RESIDUS DE PEARSON DU MODELE DE COMPTAGE")

  # Comparaison des SIGNES entre les deux approches.
  #
  # NUANCE IMPORTANTE : un changement de signe n'a de sens que pour une
  # variable dont l'effet est SIGNIFICATIF. Un coefficient statistiquement
  # indistinguable de zero peut basculer d'un signe a l'autre d'une
  # specification a l'autre sans que cela revele quoi que ce soit : son signe
  # n'est pas une information. On ne signale donc une instabilite que si la
  # variable est significative dans au moins un des deux modeles.
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
        MemeSigne & Significatif ~ "Robuste (significatif, meme signe)",
        MemeSigne & !Significatif ~ "Non significatif, signe stable",
        !MemeSigne & !Significatif ~ "Non significatif, signe instable (sans portee)",
        TRUE ~ "PROBLEME : significatif mais signe instable"
      )
    )

  sauver_tableau(accord, "10_table_robustesse_signes.csv")
  jrn$capturer(as.data.frame(accord),
               "ROBUSTESSE — OLS SUR LOG DU TAUX vs BINOMIAL NEGATIF SUR COMPTAGE")

  instables_reels <- accord |> filter(!MemeSigne, Significatif)
  robustes        <- accord |> filter(MemeSigne, Significatif)

  jrn$ecrire("")
  if (nrow(instables_reels) == 0) {
    jrn$ecrire("AUCUNE conclusion significative ne change de signe entre les")
    jrn$ecrire("deux specifications. Les resultats etablis ne dependent donc")
    jrn$ecrire("pas de la constante ajoutee avant le logarithme.")
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
      jrn$ecrire("mais elles ne sont significatives dans AUCUN modele (p de ",
                 min(c(instables_sans_portee$ValeurP_OLS,
                       instables_sans_portee$ValeurP_BinNeg)), " a ",
                 max(c(instables_sans_portee$ValeurP_OLS,
                       instables_sans_portee$ValeurP_BinNeg)), ").")
      jrn$ecrire("Leur coefficient est indistinguable de zero : son signe")
      jrn$ecrire("n'est pas une information et ce basculement est attendu.")
      jrn$ecrire("Il ne faut ni s'en inquieter, ni en tirer d'interpretation.")
    }
  } else {
    jrn$ecrire("PROBLEME REEL : ",
               paste(instables_reels$Variable, collapse = ", "),
               " est significatif mais change de signe.")
    jrn$ecrire("Cette conclusion est fragile et ne doit PAS etre presentee")
    jrn$ecrire("comme un resultat etabli dans le rapport.")
  }
}


# =============================================================================
# 6. CONTROLE DE ROBUSTESSE — REGRESSION GEOGRAPHIQUEMENT PONDEREE (GWR)
# =============================================================================
# La GWR estime un jeu de coefficients PAR unite : elle teste si la relation
# est stationnaire dans l'espace (l'effet du climat est-il le meme partout ?).
#
# Elle est presentee ici comme un controle, et non comme le modele final : avec
# seulement 64 unites tres heterogenes en taille, la GWR risque fortement le
# surajustement. On tranche par l'AICc, et on rapporte le verdict quel qu'il
# soit — y compris s'il est defavorable a la GWR.

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
    jrn$ecrire(" REGRESSION GEOGRAPHIQUEMENT PONDEREE (GWR)")
    jrn$ecrire("-----------------------------------------------------")
    jrn$ecrire("Fenetre adaptative retenue : ", round(bande, 4),
               " (environ ", round(bande * nrow(donnees_modele)),
               " unites sur ", nrow(donnees_modele), ")")

    aicc_gwr <- modele_gwr$results$AICc
    aicc_ols <- AIC(modele_ols) +
      (2 * (length(coef(modele_ols)) + 1) *
         (length(coef(modele_ols)) + 2)) /
      (nrow(donnees_modele) - length(coef(modele_ols)) - 2)

    jrn$ecrire("Nombre effectif de parametres : ",
               round(modele_gwr$results$edf, 1), " degres de liberte residuels")
    jrn$ecrire("AICc de la GWR : ", round(aicc_gwr, 2))
    jrn$ecrire("AICc de l'OLS  : ", round(aicc_ols, 2))
    jrn$ecrire("")

    if (aicc_gwr > aicc_ols) {
      jrn$ecrire("VERDICT : la GWR est REJETEE. Son AICc est PLUS ELEVE que")
      jrn$ecrire("celui du modele global, malgre une souplesse bien superieure.")
      jrn$ecrire("La fenetre optimale ne retient que quelques unites voisines,")
      jrn$ecrire("ce qui est le signe classique du surajustement : le modele")
      jrn$ecrire("epouse le bruit local plutot qu'une variation reelle.")
      jrn$ecrire("")
      jrn$ecrire("CONCLUSION : rien ne permet d'affirmer que la relation varie")
      jrn$ecrire("dans l'espace. Les provinces et etats sont trop peu nombreux")
      jrn$ecrire("et trop heterogenes en superficie pour une GWR. Les cartes")
      jrn$ecrire("de coefficients locaux ne sont donc PAS produites : elles")
      jrn$ecrire("seraient visuellement convaincantes mais statistiquement")
      jrn$ecrire("vides. Une GWR aurait du sens sur des unites plus fines et")
      jrn$ecrire("plus nombreuses (divisions de recensement, comtes).")
    } else {
      jrn$ecrire("VERDICT : la GWR ameliore l'ajustement. La relation n'est")
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
                   "ETENDUE DES COEFFICIENTS LOCAUX")
    }
  }
}


# =============================================================================
# 7. SYNTHESE INTERPRETATIVE
# =============================================================================

jrn$ecrire("")
jrn$ecrire("=====================================================")
jrn$ecrire(" SYNTHESE")
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
jrn$ecrire("LIMITES A ENONCER DANS LE RAPPORT")
jrn$ecrire("1. Le lieu de NAISSANCE n'est pas le lieu de DEVELOPPEMENT. Un")
jrn$ecrire("   joueur ne a Toronto et forme en Saskatchewan est compte comme")
jrn$ecrire("   torontois. Ce biais est structurel : aucune methode statistique")
jrn$ecrire("   ne peut le corriger avec les seules donnees disponibles.")
jrn$ecrire("2. La latitude est une approximation grossiere du climat. Une")
jrn$ecrire("   temperature moyenne de janvier, extraite d'une couche")
jrn$ecrire("   matricielle (WorldClim), serait bien plus directe.")
jrn$ecrire("3. La distance a la cote traite la baie d'Hudson comme un ocean")
jrn$ecrire("   moderateur, alors qu'elle est gelee une bonne partie de")
jrn$ecrire("   l'annee. La continentalite du Manitoba est donc sous-estimee.")
jrn$ecrire("4. n = ", nrow(unites), " unites seulement. C'est peu pour une")
jrn$ecrire("   regression a 4 variables explicatives, et cela limite la")
jrn$ecrire("   puissance de tous les tests.")
jrn$ecrire("5. Correlation n'est pas causalite : ces modeles decrivent des")
jrn$ecrire("   associations spatiales, ils ne demontrent aucun mecanisme.")

jrn$fermer()
message("=== 10 termine ===")
