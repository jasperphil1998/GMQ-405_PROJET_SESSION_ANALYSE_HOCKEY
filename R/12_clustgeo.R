# =============================================================================
# 12_clustgeo.R — Classification ascendante hierarchique avec contrainte
#                 spatiale (ClustGeo)
# =============================================================================
# >>> MODULE DE XAVIER LAFRANCE — CHANTIER EN COURS <<<
#
# ORIGINE : dans archive/Projet_Hockey_script_ORIGINAL.R, la SECTION 6 se
# resume a son titre :
#
#     # SECTION 6 — ClustGeo ----
#
# Ce fichier est le point de depart : une trame COMPLETE ET EXECUTABLE, calquee
# sur la recette du manuel du cours (section 8.2.1, "Calcul de la methode
# ClustGeo"), appliquee aux donnees du projet. Tu peux la lancer telle quelle,
# regarder les sorties, puis changer ce qui doit l'etre.
#
# LES QUATRE DECISIONS QUI TE REVIENNENT sont marquees  # CHOIX XAVIER  :
#   1. quelles variables entrent dans la matrice semantique ;
#   2. combien de classes (K) ;
#   3. quelle valeur d'alpha (arbitrage entre coherence thematique et
#      coherence spatiale) ;
#   4. comment nommer et interpreter les classes obtenues.
#
# CE QUE FAIT LA METHODE
# La classification ascendante hierarchique ordinaire regroupe les unites
# uniquement selon leurs VALEURS : deux provinces aux profils identiques
# finissent ensemble meme si elles sont aux antipodes, ce qui donne des classes
# eparpillees sur la carte. ClustGeo (Chavent et coll., 2018) melange deux
# matrices de distance :
#   D0 = dissimilarite SEMANTIQUE (les variables)
#   D1 = dissimilarite SPATIALE   (la distance entre centroides)
# et le parametre alpha regle le dosage :
#   alpha = 0   -> classification classique, aucune contrainte spatiale
#   alpha = 1   -> classification purement geographique, les variables ne
#                  comptent plus
#   entre les deux -> des classes a la fois homogenes ET geographiquement
#                  compactes. C'est tout l'interet de la methode.
#
# PREREQUIS : lancer 06_normalisation.R avant (il produit
# sorties/unites_normalisees.rds).
#
# SORTIES : 3 figures, 2 tableaux, 1 journal de resultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 12 — CLASSIFICATION SPATIALE (ClustGeo) ===")

if (!requireNamespace("ClustGeo", quietly = TRUE)) {
  # Voir la note dans 11_stkde.R : run_all.R lit MODULE_IGNORE pour ne pas
  # afficher "ok" sur un module qui n'a rien produit.
  MODULE_IGNORE <- TRUE
  message("Module 12 IGNORE : le paquet ClustGeo n'est pas installe.\n",
          "  install.packages(\"ClustGeo\")\n",
          "  AUCUNE sortie de classification ne sera produite.")
} else {

suppressPackageStartupMessages(library(ClustGeo))

fichier_unites <- chemin_sortie("unites_normalisees.rds")
if (!file.exists(fichier_unites)) {
  stop("Lancer d'abord 06_normalisation.R (produit unites_normalisees.rds).",
       call. = FALSE)
}

jrn <- journal_resultats("12_resultats_clustgeo.txt")
jrn$ecrire("=====================================================")
jrn$ecrire(" ANALYSE SPATIALE — 12. CLASSIFICATION SPATIALE")
jrn$ecrire(" Methode ClustGeo (manuel, section 8.2.1)")
jrn$ecrire("=====================================================")


# =============================================================================
# 1. DONNEES ET VARIABLES ----
# =============================================================================
# Lecture de l'objet contenant les variables sémantiques
unites <- readRDS(fichier_unites) |>
  st_transform(CRS_NA) |>
  filter(!is.na(TauxPar100k), !is.na(Population))

# Les quatre variables sémantiques retenues décrivent des aspects différents 
# du hockey local :
#   TauxPar100k : combien de joueurs une population donnée produit
#   PtsPar100k  : combien de production offensive elle produit
#   PtsMoyen    : le calibre moyen des joueurs produits (independant du volume)
#   PartElite   : la capacite a produire des joueurs de tout premier plan

unites <- unites |>
  mutate(PartElite = ifelse(NbJoueurs > 0, NbElite / NbJoueurs * 100, 0))

VARS_SEMANTIQUES <- c("TauxPar100k", "PtsPar100k", "PtsMoyen", "PartElite")

donnees <- unites |>
  st_drop_geometry() |>
  select(all_of(VARS_SEMANTIQUES))

# Toute unite avec une valeur manquante ferait echouer dist() : on les ecarte
# explicitement plutot que de laisser R produire des NA silencieux.
complet <- stats::complete.cases(donnees)
if (any(!complet)) {
  jrn$ecrire("Unites ecartees (valeur manquante) : ", sum(!complet), " -> ",
             paste(unites$postal[!complet], collapse = ", "))
  unites  <- unites[complet, ]
  donnees <- donnees[complet, ]
}

jrn$ecrire("Unites classees : ", nrow(unites))
jrn$ecrire("Variables : ", paste(VARS_SEMANTIQUES, collapse = ", "))

jrn$capturer(summary(donnees), "SOMMAIRE DES VARIABLES AVANT NORMALISATION")


# =============================================================================
# 2. LES DEUX MATRICES DE DISTANCE ----
# =============================================================================
# Centrage (moyenne = 0) et réduction des données (variance = 1)
donnees_zscore <- data.frame(scale(donnees))

# D0 : matrice sémantique, dissimilarité selon les variables
Matrice.Semantique <- dist(donnees_zscore, method = "euclidean")

# D1 : matrice spatiale, distance euclidienne entre centroides.
# Elle est calculée dans CRS_NA (projection metrique)
xy <- st_coordinates(st_centroid(st_geometry(unites)))
Matrice.Spatiale <- dist(xy, method = "euclidean")


# =============================================================================
# 3. CHOIX DU PARAMETRE ALPHA ----
# =============================================================================
# choicealpha() calcule, pour chaque valeur d'alpha, la part d'inertie
# expliquee par la matrice semantique (Q0) et par la matrice spatiale (Q1).
# On cherche le point ou l'on gagne beaucoup de coherence spatiale en perdant
# peu de coherence thematique.

K_CLASSES <- 5

alphas <- seq(0, 1, 0.05)

resultat_alpha <- choicealpha(
  D0 = Matrice.Semantique,   # matrice semantique
  D1 = Matrice.Spatiale,     # matrice spatiale
  range.alpha = alphas,      # valeurs de alpha
  K = K_CLASSES,             # nombre de classes
  wt = NULL, scale = TRUE, graph = FALSE
)

df_alpha <- data.frame(resultat_alpha$Q)
df_alpha$alpha <- alphas

sauver_tableau(
  df_alpha |>
    select(alpha, Q0, Q1) |>
    mutate(across(c(Q0, Q1), ~ round(.x, 4))),
  "12_table_choix_alpha.csv"
)

graph_alpha <- ggplot(df_alpha, aes(x = alpha)) +
  geom_line(aes(y = Q0, colour = "Matrice semantique (variables)"),
            linewidth = 0.9) +
  geom_point(aes(y = Q0, colour = "Matrice semantique (variables)"),
             size = 2.4) +
  geom_line(aes(y = Q1, colour = "Matrice spatiale (geographie)"),
            linewidth = 0.9) +
  geom_point(aes(y = Q1, colour = "Matrice spatiale (geographie)"),
             size = 2.4) +
  scale_colour_manual(values = c("Matrice semantique (variables)" = "black",
                                 "Matrice spatiale (geographie)"  = "#b2182b")) +
  labs(
    title = "Choix du parametre alpha de ClustGeo",
    subtitle = paste0("Part d'inertie expliquee par chaque matrice, pour K = ",
                      K_CLASSES, " classes.\nLe bon alpha est celui ou la ",
                      "courbe rouge monte encore vite alors que la noire ",
                      "n'a pas encore chute."),
    x = "Parametre alpha", y = "Pseudo-inertie expliquee",
    colour = NULL, caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

sauver_graphique(graph_alpha, "12_graph_choix_alpha.png",
                 largeur = 9, hauteur = 6)

# Suggestion automatique : le plus grand alpha qui coute moins de 10 % de
# l'inertie semantique par rapport a alpha = 0. C'est une regle simple, pas
# une verite : REGARDE LE GRAPHIQUE avant de trancher.
q0_reference <- df_alpha$Q0[df_alpha$alpha == 0]
candidats <- df_alpha$alpha[df_alpha$Q0 >= 0.90 * q0_reference]
alpha_suggere <- max(candidats)

jrn$capturer(
  df_alpha |>
    mutate(across(c(Q0, Q1), ~ round(.x, 4))) |>
    select(alpha, Q0, Q1) |>
    as.data.frame(),
  "INERTIE EXPLIQUEE SELON ALPHA"
)
jrn$ecrire("")
jrn$ecrire("Alpha suggere automatiquement : ", alpha_suggere)
jrn$ecrire("(le plus grand alpha qui conserve au moins 90 % de l'inertie")
jrn$ecrire(" semantique obtenue a alpha = 0)")

# CHOIX XAVIER no 3 — la valeur d'alpha retenue.
# Le manuel retient 0,30 pour son jeu de donnees. Ici on part de la suggestion
# automatique ; remplace-la par une valeur fixe une fois que tu as regarde
# 12_graph_choix_alpha.png, et justifie-la dans le rapport.
ALPHA <- alpha_suggere

jrn$ecrire("Alpha retenu : ", ALPHA)


# =============================================================================
# 4. CLASSIFICATION
# =============================================================================

arbre_clustgeo <- hclustgeo(
  D0 = Matrice.Semantique,
  D1 = Matrice.Spatiale,
  alpha = ALPHA
)

unites$Classe <- as.character(cutree(arbre_clustgeo, k = K_CLASSES))

# Classification SANS contrainte spatiale, pour comparaison. C'est elle qui
# montre l'apport de la methode : si les deux donnent la meme chose, alpha ne
# sert a rien ; si la version spatiale est nettement plus compacte sur la
# carte, la contrainte a joue.
arbre_sans_contrainte <- hclustgeo(D0 = Matrice.Semantique, alpha = 0)
unites$ClasseSansEspace <- as.character(
  cutree(arbre_sans_contrainte, k = K_CLASSES)
)

jrn$capturer(table(unites$Classe),
             paste0("NOMBRE D'UNITES PAR CLASSE (alpha = ", ALPHA, ")"))
jrn$capturer(table(unites$ClasseSansEspace),
             "NOMBRE D'UNITES PAR CLASSE (alpha = 0, sans contrainte)")

# --- Profil moyen des classes -----------------------------------------------
# C'est le tableau qui permet de NOMMER les classes.

profils <- unites |>
  st_drop_geometry() |>
  group_by(Classe) |>
  summarise(
    NbUnites    = n(),
    Population  = sum(Population),
    NbJoueurs   = sum(NbJoueurs),
    TauxPar100k = round(mean(TauxPar100k), 2),
    PtsPar100k  = round(mean(PtsPar100k), 1),
    PtsMoyen    = round(mean(PtsMoyen), 1),
    PartElite   = round(mean(PartElite), 2),
    .groups     = "drop"
  ) |>
  arrange(desc(TauxPar100k))

sauver_tableau(profils, "12_table_profils_classes.csv")
jrn$capturer(as.data.frame(profils),
             "PROFIL MOYEN DE CHAQUE CLASSE")

# CHOIX XAVIER no 4 — nommer les classes.
# Le tableau ci-dessus donne les moyennes de chaque classe. A toi de leur
# donner un nom parlant ("Prairies a fort taux", "Sun Belt", ...) et de le
# reporter dans le rapport. Une classification qu'on ne sait pas nommer est
# une classification qu'on n'a pas comprise.
jrn$ecrire("")
jrn$ecrire("A FAIRE : nommer chaque classe a partir du tableau ci-dessus.")

# Composition detaillee, pour savoir quelles provinces tombent ensemble
composition <- unites |>
  st_drop_geometry() |>
  arrange(Classe, desc(TauxPar100k)) |>
  select(Classe, Code = postal, Unite = NomUnite, Pays,
         TauxPar100k, PtsMoyen, PartElite)

jrn$capturer(as.data.frame(composition), "COMPOSITION DETAILLEE DES CLASSES")


# =============================================================================
# 5. CARTOGRAPHIE
# =============================================================================

carte_clustgeo <- tm_shape(unites) +
  tm_polygons(
    fill = "Classe",
    fill.scale = tm_scale_categorical(values = "brewer.set2"),
    fill.legend = tm_legend(title = "Classe"),
    col = "white", lwd = 0.4
  ) +
  tm_title(paste0("Classification spatiale des provinces et etats (ClustGeo, ",
                  "alpha = ", ALPHA, ")")) +
  tm_credits(
    paste0("Variables : ", paste(VARS_SEMANTIQUES, collapse = ", "), ".",
           "\nK = ", K_CLASSES, " classes. Projection : Albers equivalente.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.6
  )

sauver_carte(carte_clustgeo, "12_carte_clustgeo.png")

# Carte de comparaison : avec et sans contrainte spatiale
comparaison_sf <- bind_rows(
  unites |>
    mutate(Groupe = Classe,
           Methode = paste0("b) Avec contrainte spatiale (alpha = ",
                            ALPHA, ")")) |>
    select(Groupe, Methode, geometry),
  unites |>
    mutate(Groupe = ClasseSansEspace,
           Methode = "a) Sans contrainte spatiale (alpha = 0)") |>
    select(Groupe, Methode, geometry)
)

carte_comparaison_clustgeo <- tm_shape(comparaison_sf) +
  tm_polygons(
    fill = "Groupe",
    fill.scale = tm_scale_categorical(values = "brewer.set2"),
    fill.legend = tm_legend(title = "Classe", orientation = "landscape"),
    col = "white", lwd = 0.3
  ) +
  tm_facets(by = "Methode", ncol = 2) +
  tm_title("Apport de la contrainte spatiale dans la classification") +
  tm_credits(
    paste0("A gauche, les classes peuvent etre eparpillees ; a droite, elles ",
           "sont geographiquement compactes.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.5
  )

sauver_carte(carte_comparaison_clustgeo, "12_carte_comparaison_contrainte.png",
             largeur = 14, hauteur = 6)


# =============================================================================
# 6. PISTES POUR LA SUITE
# =============================================================================

jrn$ecrire("")
jrn$ecrire("=====================================================")
jrn$ecrire(" PISTES POUR LA SUITE")
jrn$ecrire("=====================================================")
jrn$ecrire("1. SKATER (spdep::skater), couvert par le manuel a la section")
jrn$ecrire("   8.1, est l'autre grande methode de classification sous")
jrn$ecrire("   contrainte spatiale. Contrairement a ClustGeo, elle garantit")
jrn$ecrire("   que chaque classe est un bloc CONNEXE sur la carte. Comparer")
jrn$ecrire("   les deux ferait une bonne sous-section de rapport.")
jrn$ecrire("2. Les variables du module 10 (latitude, densite de population,")
jrn$ecrire("   distance a la cote, distance a une equipe de la LNH) peuvent")
jrn$ecrire("   entrer dans la matrice semantique : la classification")
jrn$ecrire("   deviendrait alors une typologie de CONTEXTES et non seulement")
jrn$ecrire("   de resultats.")
jrn$ecrire("3. Le module 07 montre que les provinces sont peu nombreuses")
jrn$ecrire("   (n = 64). Une classification sur la grille hexagonale du")
jrn$ecrire("   module 07 donnerait beaucoup plus d'unites.")

jrn$fermer()

}   # fin du bloc conditionnel sur ClustGeo

message("=== 12 termine ===")
