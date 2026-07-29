# Section ANALYSE SPATIALE — bac à sable

Ajout au projet GMQ-405 pour approfondir le volet **analyse spatiale**. Le
script principal du projet (`Projet_Hockey_script.R`) fait de la **cartographie
descriptive** : il représente des effectifs bruts. Cette section ajoute de la
**statistique spatiale** : elle normalise, teste, modélise.

**Rien ici ne touche au script principal ni au dossier `figures/`.** Toutes les
sorties vont dans `analyse_spatiale/sorties/`.

---

## Cette section vit sur sa propre branche

Tout ce dossier est versionné sur la branche **`analyse-spatiale`**, et non sur
`main`. L'isolement est donc assuré par la branche, pas par un fichier d'exclusion :

- `main` reste **exactement** dans l'état où Philippe et Xavier l'ont laissée —
  aucun fichier du projet d'origine n'est modifié par cette section ;
- tout est bien sur GitHub, donc consultable et sauvegardé ;
- la fusion dans `main` reste une **décision séparée**, à prendre plus tard.

### Naviguer entre les deux

```bash
git checkout analyse-spatiale
```

```bash
git checkout main
```

Sur `main`, le dossier `analyse_spatiale/` disparaît simplement de l'arborescence
— c'est normal, il n'existe que sur l'autre branche.

### Plus tard : fusionner dans `main` (si vous le décidez)

À faire uniquement en accord avec l'équipe, puisque ça touche la branche
partagée :

```bash
git checkout main && git merge analyse-spatiale && git push
```

Ou, plus propre pour un travail d'équipe, ouvrir une *pull request* sur GitHub
depuis la branche `analyse-spatiale` : les coéquipiers peuvent alors relire
avant que ça entre dans `main`.

### Ce qui est versionné, et ce qui ne l'est pas

Les figures, tableaux et journaux de `sorties/` **sont** versionnés : le README
y renvoie, et ça permet de consulter les résultats sur GitHub sans exécuter R
(6,4 Mo au total).

C'est un choix différent de celui du projet principal, qui exclut `figures/`
via son `.gitignore`. Pour aligner cette section sur la convention du projet,
ajouter `sorties/` dans `analyse_spatiale/.gitignore`.

Seule exception actuelle : `sorties/*.rds`, un artefact binaire intermédiaire
sans intérêt pour un lecteur humain, qui se régénère en quelques secondes.

---

## Lancer

Prérequis : ouvrir le **dossier** du projet (pas juste un fichier `.R`), comme
pour le script principal. Les chemins sont relatifs à la racine du projet.

```bash
Rscript analyse_spatiale/run_all.R
```

Ou dans la console R :

```r
source("analyse_spatiale/run_all.R")
```

Durée totale : environ 20 secondes. Aucun géocodage n'est relancé (le cache
existant est réutilisé) et aucun téléchargement n'est nécessaire.

Les modules peuvent aussi se lancer un par un. Seule contrainte : **01 doit
tourner avant 02 et 05**, car il produit `sorties/unites_normalisees.rds`.

### Paquets

Requis, en plus de ceux du projet : `spdep`, `spatialreg`, `spatstat.geom`,
`spatstat.explore`.

```r
install.packages(c("spdep", "spatialreg", "spatstat.geom", "spatstat.explore"))
```

Optionnels (les sections concernées sont simplement ignorées s'ils manquent) :
`ggrepel`, `MASS`, `spgwr`.

---

## Les cinq modules

### 00_config.R
Chargement, localisation de la racine du projet, définition des projections,
préparation des données. Sourcé par tous les autres. Ne produit rien.

### 01 — Normalisation
**Ce que ça corrige :** les cartes d'effectifs bruts sont surtout des cartes de
la population. « L'Ontario produit le plus de joueurs » est un résultat
démographique, pas géographique.

Calcule un **taux** (joueurs par 100 000 habitants) et un **quotient de
localisation** (QL = part des joueurs / part de la population).

**Résultat marquant :** la Saskatchewan passe du **4ᵉ rang brut au 1ᵉʳ rang en
taux** (47,8 joueurs/100 000 hab., QL = 24,8). Le Manitoba passe du 6ᵉ au 2ᵉ.
L'Ontario recule du 1ᵉʳ au 4ᵉ. La figure
`01_carte_comparaison_brut_taux.png` juxtapose les deux lectures.

### 02 — Autocorrélation spatiale
**Ce que ça ajoute :** la démonstration statistique qu'il existe une structure
spatiale, là où le script principal ne pouvait que la suggérer visuellement.

- **Moran global** : I = 0,320, p = 3,3 × 10⁻⁹ → structure spatiale bien réelle
- **Test de sensibilité à k** (3 à 10 voisins) : la conclusion tient pour toutes
  les valeurs, donc elle ne dépend pas d'un choix arbitraire de voisinage
- **LISA** : grappe chaude = Prairies + C.-B. + territoires ; grappe froide =
  Sun Belt américaine ; le Nunavut ressort en valeur atypique
- **Getis-Ord Gi\*** : version « points chauds », plus lisible en rapport
- **Grille hexagonale de 200 km** : refait l'analyse sur des unités indépendantes
  des frontières administratives, pour discuter le **MAUP**

**Résultat le plus intéressant, et c'est un résultat négatif :** les points
moyens par joueur ne montrent **aucune** autocorrélation (I = −0,03, p = 0,67).
Le lieu de naissance structure le fait d'**atteindre** la LNH, pas le
**calibre** une fois rendu. Cela nuance directement les cartes de points totaux
du script principal, qui mélangent volume et calibre.

Deux précautions sont écrites dans le journal de sortie : la contiguïté
« reine » laisse l'Î.-P.-É. et Hawaï sans voisin (d'où le choix des k plus
proches voisins), et le QL donne exactement le même I que le taux puisqu'il en
est une transformation linéaire — inutile de tester les deux.

### 03 — Semis de points
**Ce que ça corrige :** les cercles proportionnels saturent dès que les villes
sont nombreuses et proches. Une surface de densité reste lisible.

- **Densité de noyau pondérée** (σ = 100 km, correction de bordure) par nombre
  de joueurs, puis par points en carrière
- **Surface de risque relatif** (Kelsall-Diggle) : probabilité qu'un joueur né à
  un endroit donné atteigne 500 points
- **Fonction L de Ripley** avec enveloppe par simulation

**Résultat :** le sud de l'Ontario atteint ~16,6 % de joueurs d'élite contre
6,5 % en moyenne nationale.

L'intérêt méthodologique du risque relatif : comme c'est un **rapport de deux
densités**, l'effet de la population s'annule au numérateur et au dénominateur.
Aucune donnée démographique n'est nécessaire. C'est le résultat le plus solide
du module.

À l'inverse, la fonction L est rapportée **avec une mise en garde explicite** :
son hypothèse nulle suppose que les localités pourraient se répartir
uniformément sur le Canada, ce qui est faux. Rejeter cette hypothèse ne démontre
rien sur le hockey — ça redémontre que les Canadiens vivent en ville. Le test
n'est conservé que pour quantifier l'**échelle** du regroupement (écart maximal
au hasard à 302 km) et pour illustrer qu'un test spatial ne vaut que ce que vaut
son hypothèse nulle. Un artefact de « répulsion » vers 700 km, dû à la forme
allongée du Canada, est signalé automatiquement.

### 04 — Centrographie temporelle
**Ce que ça ajoute :** le projet a une dimension spatiale et une dimension
temporelle, mais ne les croisait jamais.

Calcule le **centre de gravité** et la **distance-type** par décennie de
naissance.

**Résultats :**
- **Monde** : le centre se déplace de **2201 km vers l'est** et la dispersion
  passe de 1040 à 3752 km → internationalisation de la ligue
- **Canada seul** : dérive de **693 km vers l'ouest**, avec un rho de Spearman
  de **−0,80 (p = 0,0016)** → la dérive est systématique, pas un va-et-vient

À l'échelle mondiale, le centre est calculé **sur la sphère** (vecteurs
cartésiens 3D) : on ne peut pas moyenner des longitudes en degrés pour des
points répartis sur plusieurs continents.

Deux mises en garde figurent dans le journal : le centre de gravité tombe dans
l'Atlantique, donc il ne désigne aucun lieu réel (seuls son déplacement et la
dispersion ont un sens) ; et la dérive canadienne (693 km) reste inférieure à la
dispersion (1481 km), donc c'est un déplacement de la moyenne, pas un
déménagement du hockey.

### 05 — Modélisation spatiale
**Ce que ça ajoute :** l'explication, et pas seulement la description. C'est le
module qui répond le plus directement à l'intitulé « modélisation ».

Séquence canonique complète :

| Étape | Résultat |
|---|---|
| 1. OLS | R² = 0,51 |
| 2. Moran sur les résidus | I = 0,126, p = 0,0019 → **OLS invalide** |
| 3. Choix de la forme spatiale | tests du score de Rao |
| 4. Estimation | SEM (AIC 249,1) préféré au SAR lag (251,4) et à l'OLS (251,8) |
| 5. Moran sur les résidus du SEM | I = −0,017, p = 0,51 → **autocorrélation absorbée** |
| 6. Robustesse : binomial négatif | signes des effets significatifs confirmés |
| 7. Robustesse : GWR | **rejetée** (AICc 258,3 > 257,1) |

Variables explicatives, toutes calculées hors ligne : latitude du centroïde,
distance à la côte (continentalité), densité de population, distance à
l'équipe de la LNH la plus proche. Les VIF sont tous sous 5 (pas de
colinéarité).

**Effets significatifs :** latitude (+) et densité de population (+). La
distance à la côte et la distance à une équipe de la LNH ne sont significatives
dans aucun modèle.

Deux choix méthodologiques valent d'être défendus en rapport :

- **La GWR est rejetée, et c'est rapporté comme tel.** Sa fenêtre optimale ne
  retient que ~2 unités sur 64 et son AICc est pire que celui du modèle global :
  du surajustement caractérisé. Les cartes de coefficients locaux ne sont
  **volontairement pas produites** — elles seraient visuellement convaincantes
  et statistiquement vides.
- **Le contrôle des signes est conditionnel à la significativité.** La distance
  à la côte et la distance à la LNH changent de signe entre les deux
  spécifications, mais elles ne sont significatives dans aucun modèle : un
  coefficient indistinguable de zéro peut basculer sans que ça signifie quoi que
  ce soit. Signaler ça comme une « instabilité » serait une fausse alerte.

---

## Choix techniques transversaux

**Projections.** Le script principal reste en EPSG:4326 (degrés) partout. C'est
acceptable pour afficher des symboles, mais invalide pour toute distance, aire
ou densité : un degré de longitude vaut ~78 km à Toronto et ~52 km à
Yellowknife. Cette section projette systématiquement :

| Usage | CRS | Pourquoi |
|---|---|---|
| Amérique du Nord | Albers équivalente | conserve les **aires** → densités justes |
| Canada | EPSG:3347 | projection officielle de Statistique Canada |
| Atlantique Nord | LAEA centrée 55N/45O | Canada et Europe dans le même champ |

**Matrice de voisinage.** k = 5 plus proches voisins, partout (modules 02 et 05),
pour que les résultats soient comparables entre les deux. La contiguïté
« reine » a été écartée parce qu'elle laisse des unités insulaires sans aucun
voisin, ce qui invalide les tests.

**Interprétations générées, pas rédigées d'avance.** Les textes d'interprétation
des journaux de sortie sont calculés à partir des valeurs obtenues (voir par
exemple `qualifier_I()` dans le module 02). Si les données changent, les
conclusions écrites changent avec elles. Aucun commentaire n'affirme un résultat
que le calcul ne montre pas.

---

## Données ajoutées

### `data/population_provinces_etats.csv`
64 unités : 13 provinces et territoires (Recensement 2021) + 51 États incluant
le District de Columbia (Census 2020).

⚠️ **À vérifier avant de citer dans le rapport.** Ces chiffres ont été saisis
manuellement et n'ont pas été extraits automatiquement d'une source officielle.
Ils sont cohérents et plausibles, mais un chiffre de recensement cité dans un
travail universitaire doit être confirmé à la source
(Statistique Canada, tableau 98-10-0001 ; US Census Bureau, P.L. 94-171).

### `data/equipes_lnh.csv`
32 équipes de la LNH avec les coordonnées de leur ville. Sert à calculer la
distance à l'équipe la plus proche. Coordonnées approximatives (centre-ville,
pas l'amphithéâtre exact) : suffisant pour une variable exprimée en centaines
de kilomètres.

---

## Limites à énoncer dans le rapport

1. **Le lieu de naissance n'est pas le lieu de développement.** Un joueur né à
   Toronto et formé en Saskatchewan compte comme torontois. Ce biais est
   structurel et aucune méthode statistique ne le corrige avec ces données.
   C'est la limite la plus importante de tout le projet, script principal
   inclus.
2. **La latitude est une approximation grossière du climat.** Une température
   moyenne de janvier extraite d'une couche matricielle (WorldClim via
   `geodata` + `terra::extract`) serait bien plus directe. C'est la principale
   piste d'amélioration du module 05.
3. **La baie d'Hudson est traitée comme un océan modérateur** alors qu'elle est
   gelée une bonne partie de l'année : la continentalité du Manitoba est
   sous-estimée.
4. **n = 64 unités.** C'est peu pour une régression à 4 variables, et ça limite
   la puissance de tous les tests. Passer aux divisions de recensement (~293)
   et aux comtés américains (~3143) donnerait des centaines d'unités et
   rendrait la GWR pertinente — mais exige des fichiers de découpage externes
   (`cancensus`, `tigris`).
5. **La comparaison d'échelles du module 02 n'est pas un MAUP pur** : les trois
   lignes de la synthèse diffèrent par l'échelle *et* par la variable.
   Normaliser les hexagones exigerait une grille de population matricielle.
6. **Corrélation n'est pas causalité.** Ces modèles décrivent des associations
   spatiales, ils ne démontrent aucun mécanisme.

---

## Sorties

40 fichiers dans `sorties/` : 19 figures PNG à 300 dpi (mêmes réglages que le
script principal), 15 tableaux CSV, 5 journaux `.txt` et 1 fichier `.rds`
intermédiaire (réutilisé par les modules 02 et 05).

Les journaux `*_resultats_*.txt` contiennent les sorties complètes des tests
statistiques avec leur interprétation. Ce sont eux qu'il faut lire en premier :
ils sont rédigés pour pouvoir être cités directement dans le rapport sans avoir
à relancer le code.
