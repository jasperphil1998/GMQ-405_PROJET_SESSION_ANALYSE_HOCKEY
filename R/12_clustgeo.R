# =============================================================================
# 12_clustgeo.R — Classification ascendante hiérarchique avec contrainte
#                 spatiale (ClustGeo)
# =============================================================================
# MÉTHODE : La classification ascendante hiérarchique ordinaire regroupe les 
# unités uniquement selon leurs valeurs : deux provinces aux profils identiques
# finissent ensemble même si elles sont aux antipodes, ce qui donne des classes
# éparpillées sur la carte. ClustGeo mélange deux matrices de distance :
#   D0 = dissimilarité sémantique (les variables)
#   D1 = dissimilarité spatiale   (la distance entre centroïdes)
# et le paramètre alpha règle le dosage :
#   alpha = 0   -> classification classique, aucune contrainte spatiale
#   alpha = 1   -> classification purement géographique, les variables ne
#                  comptent plus
#   entre les deux -> des classes à la fois homogènes et géographiquement
#                  compactes. C'est tout l'intérêt de la méthode.
#
# PRÉREQUIS : lancer 06_normalisation.R avant (il produit
# sorties/unites_normalisees.rds).
#
# SORTIES : 3 figures, 2 tableaux, 1 journal de résultats.
# =============================================================================

if (!exists("RACINE")) source(file.path("R", "00_config.R"))

message("\n=== 12 — CLASSIFICATION SPATIALE (ClustGeo) ===")

if (!requireNamespace("ClustGeo", quietly = TRUE)) {
  # Voir la note dans 11_stkde.R : run_all.R lit MODULE_IGNORE pour ne pas
  # afficher "ok" sur un module qui n'a rien produit.
  MODULE_IGNORE <- TRUE
  message("Module 12 IGNORÉ : le paquet ClustGeo n'est pas installé.\n",
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
jrn$ecrire(" Méthode ClustGeo (manuel, section 8.2.1)")
jrn$ecrire("=====================================================")


# =============================================================================
# 1. DONNÉES ET VARIABLES ----
# =============================================================================
# Lecture de l'objet contenant les variables sémantiques
unites <- readRDS(fichier_unites) |>
  st_transform(CRS_NA) |>
  filter(!is.na(TauxPar100k), !is.na(Population))

# Les quatre variables sémantiques retenues décrivent des aspects différents 
# du hockey local :
#   TauxPar100k : combien de joueurs une population donnée produit
#   PtsPar100k  : combien de production offensive elle produit
#   PtsMoyen    : le calibre moyen des joueurs produits (indépendant du volume)
#   PartElite   : la capacité à produire des joueurs de tout premier plan

unites <- unites |>
  mutate(PartElite = ifelse(NbJoueurs > 0, NbElite / NbJoueurs * 100, 0))

VARS_SEMANTIQUES <- c("TauxPar100k", "PtsPar100k", "PtsMoyen", "PartElite")

donnees <- unites |>
  st_drop_geometry() |>
  select(all_of(VARS_SEMANTIQUES))

# Toute unité avec une valeur manquante ferait échouer dist() : on les écarte
# explicitement plutôt que de laisser R produire des NA silencieux.
complet <- stats::complete.cases(donnees)
if (any(!complet)) {
  jrn$ecrire("Unités écartées (valeur manquante) : ", sum(!complet), " -> ",
             paste(unites$postal[!complet], collapse = ", "))
  unites  <- unites[complet, ]
  donnees <- donnees[complet, ]
}

jrn$ecrire("Unités classées : ", nrow(unites))
jrn$ecrire("Variables : ", paste(VARS_SEMANTIQUES, collapse = ", "))

jrn$capturer(summary(donnees), "SOMMAIRE DES VARIABLES AVANT NORMALISATION")


# =============================================================================
# 2. LES DEUX MATRICES DE DISTANCE ----
# =============================================================================
# Centrage (moyenne = 0) et réduction des données (variance = 1)
donnees_zscore <- data.frame(scale(donnees))

# D0 : matrice sémantique, dissimilarité selon les variables
Matrice.Semantique <- dist(donnees_zscore, method = "euclidean")

# D1 : matrice spatiale, distance euclidienne entre centroïdes.
# Elle est calculée dans CRS_NA (projection métrique)
xy <- st_coordinates(st_centroid(st_geometry(unites)))
Matrice.Spatiale <- dist(xy, method = "euclidean")


# =============================================================================
# 3. CHOIX DU PARAMÈTRE ALPHA ----
# =============================================================================
# choicealpha() calcule, pour chaque valeur d'alpha, la part d'inertie
# expliquée par la matrice sémantique (Q0) et par la matrice spatiale (Q1).
# On cherche le point où l'on gagne beaucoup de cohérence spatiale en perdant
# peu de cohérence thématique.

K_CLASSES <- 5

alphas <- seq(0, 1, 0.05)

resultat_alpha <- choicealpha(
  D0 = Matrice.Semantique,   # matrice sémantique
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
  geom_line(aes(y = Q0, colour = "Matrice sémantique (variables)"),
            linewidth = 0.9) +
  geom_point(aes(y = Q0, colour = "Matrice sémantique (variables)"),
             size = 2.4) +
  geom_line(aes(y = Q1, colour = "Matrice spatiale (géographie)"),
            linewidth = 0.9) +
  geom_point(aes(y = Q1, colour = "Matrice spatiale (géographie)"),
             size = 2.4) +
  scale_colour_manual(values = c("Matrice sémantique (variables)" = "black",
                                 "Matrice spatiale (géographie)"  = "#b2182b")) +
  labs(
    title = "Choix du paramètre alpha de ClustGeo",
    subtitle = paste0("Part d'inertie expliquée par chaque matrice, pour K = ",
                      K_CLASSES),
    x = "Paramètre alpha", y = "Pseudo-inertie expliquée",
    colour = NULL, caption = CREDITS
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

sauver_graphique(graph_alpha, "12_graph_choix_alpha.png",
                 largeur = 9, hauteur = 6)

# Choix de la valeur de alpha. Correspond à la valeur d'alpha là où la droite
# de la matrice sémantique et celle de la matrice spatiale se croisent dans 
# la figure précédente.
ALPHA <- 0.23 

jrn$capturer(
  df_alpha |>
    mutate(across(c(Q0, Q1), ~ round(.x, 4))) |>
    select(alpha, Q0, Q1) |>
    as.data.frame(),
  "INERTIE EXPLIQUÉE SELON ALPHA"
)

jrn$ecrire("Alpha retenu : ", ALPHA)


# =============================================================================
# 4. CLASSIFICATION ----
# =============================================================================

arbre_clustgeo <- hclustgeo(
  D0 = Matrice.Semantique,
  D1 = Matrice.Spatiale,
  alpha = ALPHA
)

unites$Classe <- as.character(cutree(arbre_clustgeo, k = K_CLASSES))

# Classification sans contrainte spatiale, pour comparaison. 
arbre_sans_contrainte <- hclustgeo(D0 = Matrice.Semantique, alpha = 0)
unites$ClasseSansEspace <- as.character(
  cutree(arbre_sans_contrainte, k = K_CLASSES)
)

jrn$capturer(table(unites$Classe),
             paste0("NOMBRE D'UNITÉS PAR CLASSE (alpha = ", ALPHA, ")"))
jrn$capturer(table(unites$ClasseSansEspace),
             "NOMBRE D'UNITÉS PAR CLASSE (alpha = 0, sans contrainte)")

## --- Profil moyen des classes -----------------------------------------------
# Tableau des valeurs moyennes des variables pour les 5 classes obtenues

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

# Composition détaillée, pour savoir quelles provinces tombent ensemble
composition <- unites |>
  st_drop_geometry() |>
  arrange(Classe, desc(TauxPar100k)) |>
  select(Classe, Code = postal, Unite = NomUnite, Pays,
         TauxPar100k, PtsPar100k, PtsMoyen, PartElite)

jrn$capturer(as.data.frame(composition), "COMPOSITION DÉTAILLÉE DES CLASSES")


# =============================================================================
# 5. CARTOGRAPHIE ----
# =============================================================================

carte_clustgeo <- tm_shape(unites) +
  tm_polygons(
    fill = "Classe",
    fill.scale = tm_scale_categorical(values = "brewer.set2"),
    fill.legend = tm_legend(title = "Classe"),
    col = "white", lwd = 0.4
  ) +
  tm_title(paste0("Classification spatiale des provinces et états (ClustGeo, ",
                  "alpha = ", ALPHA, ")")) +
  tm_credits(
    paste0("Variables : ", paste(VARS_SEMANTIQUES, collapse = ", "), ".",
           "\nK = ", K_CLASSES, " classes. Projection : Albers équivalente.",
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
    paste0("À gauche, les classes peuvent être éparpillées ; à droite, elles ",
           "sont géographiquement compactes.",
           "\nAuteur : ", AUTEURS),
    position = tm_pos_in("left", "bottom"), size = 0.5
  )

sauver_carte(carte_comparaison_clustgeo, "12_carte_comparaison_contrainte.png",
             largeur = 14, hauteur = 6)


jrn$fermer()

}   # fin du bloc conditionnel sur ClustGeo

message("=== 12 terminé ===")
