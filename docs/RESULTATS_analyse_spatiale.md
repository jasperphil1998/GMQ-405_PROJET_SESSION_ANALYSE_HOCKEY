# Résultats détaillés — volet statistique (modules 06 à 10)

Les modules 02 à 05 font de la **cartographie descriptive** : ils représentent
des effectifs bruts. Les modules 06 à 10 ajoutent de la **statistique
spatiale** : ils normalisent, testent, modélisent.

Toutes les sorties de ces cinq modules vont dans `sorties/` et sont
**versionnées** : les journaux `*_resultats_*.txt` contiennent les résultats
complets des tests avec leur interprétation, et doivent pouvoir être cités dans
le rapport sans relancer R.

> Ce document était le README du dossier `analyse_spatiale/`, qui vivait sur sa
> propre branche. Depuis la fusion, tout est dans `R/` et les modules ont été
> renumérotés (01→06 … 05→10). Voir [`FUSION.md`](FUSION.md).

---

## Lancer

```r
source("run_all.R"); lancer_modules(c("06", "07", "08", "09", "10"))
```

Durée : environ 15 secondes. Aucun géocodage n'est relancé (le cache existant
est réutilisé) et aucun téléchargement n'est nécessaire.

Les modules peuvent aussi se lancer un par un. Seule contrainte : **06 doit
tourner avant 07, 10 et 12**, car il produit `sorties/unites_normalisees.rds`.

Paquets requis en plus du socle : `spdep`, `spatialreg`, `spatstat.geom`,
`spatstat.explore`. Optionnels : `ggrepel`, `MASS`, `spgwr`.

---

## Les cinq modules

### R/00_config.R
Chargement, localisation de la racine du projet, définition des projections,
préparation des données. Sourcé par tous les autres. Ne produit rien.

### 06 — Normalisation
**Ce que ça corrige :** les cartes d'effectifs bruts sont surtout des cartes de
la population. « L'Ontario produit le plus de joueurs » est un résultat
démographique, pas géographique.

Calcule un **taux** (joueurs par 100 000 habitants) et un **quotient de
localisation** (QL = part des joueurs / part de la population).

**Résultat marquant :** la Saskatchewan passe du **4ᵉ rang brut au 1ᵉʳ rang en
taux** (47,8 joueurs/100 000 hab., QL = 24,8). Le Manitoba passe du 6ᵉ au 2ᵉ.
L'Ontario recule du 1ᵉʳ au 4ᵉ. La figure
`06_carte_comparaison_brut_taux.png` juxtapose les deux lectures.

### 07 — Autocorrélation spatiale
**Ce que ça ajoute :** la démonstration statistique qu'il existe une structure
spatiale, là où les modules descriptifs ne pouvaient que la suggérer
visuellement.

- **Moran global** : I = 0,320, p = 3,3 × 10⁻⁹ → structure spatiale bien réelle
- **Moran par permutations** (999 simulations, `moran.mc`) : c'est la procédure
  privilégiée par le manuel du cours, et celle qu'il faut citer. Le module
  vérifie automatiquement que les deux procédures concordent — l'approximation
  analytique suppose la normalité, hypothèse fragile à n = 64
- **Test de sensibilité à k** (3 à 10 voisins) : la conclusion tient pour toutes
  les valeurs, donc elle ne dépend pas d'un choix arbitraire de voisinage
- **LISA** : grappe chaude = Prairies + C.-B. + territoires ; grappe froide =
  Sun Belt américaine ; le Nunavut ressort en valeur atypique
- **Getis-Ord Gi\*** : version « points chauds », plus lisible en rapport,
  calculée en version analytique **et** par permutations (`localG_perm`)
- **Grille hexagonale de 200 km** : refait l'analyse sur des unités indépendantes
  des frontières administratives, pour discuter le **MAUP**

**Résultat le plus intéressant, et c'est un résultat négatif :** les points
moyens par joueur ne montrent **aucune** autocorrélation (I = −0,03, p = 0,67).
Le lieu de naissance structure le fait d'**atteindre** la LNH, pas le
**calibre** une fois rendu. Cela nuance directement les cartes de points totaux
du module 04, qui mélangent volume et calibre.

Deux précautions sont écrites dans le journal de sortie : la contiguïté
« reine » laisse l'Î.-P.-É. et Hawaï sans voisin (d'où le choix des k plus
proches voisins), et le QL donne exactement le même I que le taux puisqu'il en
est une transformation linéaire — inutile de tester les deux.

### 08 — Semis de points
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

### 09 — Centrographie temporelle
**Ce que ça ajoute :** le projet a une dimension spatiale et une dimension
temporelle, mais ne les croisait jamais.

Calcule le **centre moyen**, la **distance standard** et l'**ellipse de
déviation standard** par décennie de naissance — les trois paramètres de la
section 3.2 du manuel.

**Résultats :**
- **Monde** : le centre se déplace de **2201 km vers l'est** et la dispersion
  passe de 1040 à 3752 km → internationalisation de la ligue
- **Canada seul** : dérive de **693 km vers l'ouest**, avec un rho de Spearman
  de **−0,80 (p = 0,0016)** → la dérive est systématique, pas un va-et-vient
- **Ellipses** : l'azimut du grand axe reste entre **73° et 82°** sur toute la
  période, c'est-à-dire un étirement est-ouest le long du corridor habité ; le
  grand axe passe de **1 070 km** (années 1890) à **1 777 km** (années 2000).
  L'expansion est donc surtout longitudinale, ce que le cercle de distance
  standard ne pouvait pas montrer

À l'échelle mondiale, le centre est calculé **sur la sphère** (vecteurs
cartésiens 3D) : on ne peut pas moyenner des longitudes en degrés pour des
points répartis sur plusieurs continents.

Trois mises en garde figurent dans le journal : le centre de gravité tombe dans
l'Atlantique, donc il ne désigne aucun lieu réel (seuls son déplacement et la
dispersion ont un sens) ; la dérive canadienne (693 km) reste inférieure à la
dispersion (1481 km), donc c'est un déplacement de la moyenne, pas un
déménagement du hockey ; et la **taille** d'une ellipse dépend de la formule
employée (Yuill, ArcGIS Pro, CrimeStat…), donc les ellipses produites ici sont
comparables entre elles mais pas avec celles d'un autre logiciel.

### 10 — Modélisation spatiale
**Ce que ça ajoute :** l'explication, et pas seulement la description. C'est le
module qui répond le plus directement à l'intitulé « modélisation ».

Séquence canonique complète :

| Étape | Résultat |
|---|---|
| 1. OLS | R² = 0,51 |
| 2. Moran sur les résidus | I = 0,126, p = 0,0019 → **OLS invalide** |
| 3. Choix de la forme spatiale | tests du score de Rao |
| 4. Estimation | SEM (AIC 249,1) préféré au SAR lag (251,4) et à l'OLS (255,6) |
| 5. Moran sur les résidus du SEM | I = −0,017, p = 0,51 → **autocorrélation absorbée** |
| 6. Robustesse : binomial négatif | signes des effets significatifs confirmés |
| 7. Robustesse : GWR | **rejetée** (AICc plus élevé que celui du modèle global) |

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

**Projections.** Les modules descriptifs restent en EPSG:4326 (degrés). C'est
acceptable pour afficher des symboles, mais invalide pour toute distance, aire
ou densité : un degré de longitude vaut ~78 km à Toronto et ~52 km à
Yellowknife. Ces cinq modules projettent systématiquement :

| Usage | CRS | Pourquoi |
|---|---|---|
| Amérique du Nord | Albers équivalente | conserve les **aires** → densités justes |
| Canada | EPSG:3347 | projection officielle de Statistique Canada |
| Atlantique Nord | LAEA centrée 55N/45O | Canada et Europe dans le même champ |

**Matrice de voisinage.** k = 5 plus proches voisins, partout (modules 07 et 10),
pour que les résultats soient comparables entre les deux. La contiguïté
« reine » a été écartée parce qu'elle laisse des unités insulaires sans aucun
voisin, ce qui invalide les tests.

**Tests par permutations.** Là où le manuel les privilégie (Moran global,
Getis-Ord), les deux versions sont calculées et leur concordance est vérifiée
automatiquement. C'est la version par permutations qu'il faut citer.

**Interprétations générées, pas rédigées d'avance.** Les textes d'interprétation
des journaux de sortie sont calculés à partir des valeurs obtenues (voir par
exemple `qualifier_I()` dans le module 07). Si les données changent, les
conclusions écrites changent avec elles. Aucun commentaire n'affirme un résultat
que le calcul ne montre pas.

---

## Données ajoutées

### `data/population_provinces_etats.csv`
64 unités : 13 provinces et territoires (Recensement de 2021) + 51 États incluant
le District de Columbia (2020 Census).

✅ **Vérifié à la source le 2026-08-08.** Les valeurs avaient d'abord été saisies
manuellement ; elles ont depuis été confrontées une à une aux tableaux officiels.
**Aucun écart sur les 64 unités**, et les deux totaux se reconstituent exactement :
36 991 981 pour le Canada et 331 449 281 pour les États-Unis, ce qui confirme du
même coup qu'aucune unité ne manque et qu'aucune n'est comptée deux fois. Le
fichier porte désormais les colonnes `Source`, `URL_source` et `DateVerification`.

Sources exactes (formulation prête à citer : voir `docs/REFERENCES.md`, section C) :

- **Canada** — Statistique Canada, tableau 98-10-0001-01, *Chiffres de population
  et des logements : Canada, provinces et territoires*, Recensement de 2021,
  diffusé le 2022-02-09. <https://doi.org/10.25318/9810000101-fra>
- **États-Unis** — U.S. Census Bureau, *Table 2. Resident Population for the 50
  States, the District of Columbia, and Puerto Rico: 2020 Census*, population au
  1ᵉʳ avril 2020.
  <https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/apportionment-2020-table02.pdf>

⚠️ **Deux réserves de méthode à écrire dans le rapport.**

1. **Les deux recensements n'ont pas la même date de référence** (mai 2021 au
   Canada, 1ᵉʳ avril 2020 aux États-Unis). L'écart d'un an gonfle très
   légèrement les taux américains par rapport aux taux canadiens. L'effet est
   négligeable devant les rapports de 1 à 20 observés entre la Saskatchewan et
   les États du Sud, mais il faut le mentionner.
2. **La population d'aujourd'hui sert à normaliser des joueurs nés sur un
   siècle.** C'est la limite de fond de tout le module 06, indépendante de la
   qualité des chiffres : la Saskatchewan pesait démographiquement bien plus
   lourd en 1930 qu'en 2021, ce qui gonfle son taux. Cette limite est déjà
   énoncée plus bas et reste la plus importante des deux.

Pour l'échelle des **pays** (module 06, partie A), la source est tout autre : le
champ `pop_est` du fond de carte Natural Earth (`POP_YEAR = 2019`), une
estimation compilée par Natural Earth à partir de sources tierces. Ce n'est pas
une donnée de recensement et cela ne doit pas être présenté comme telle.

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
   C'est la limite la plus importante de tout le projet, volet descriptif
   inclus.
2. **La latitude est une approximation grossière du climat.** Une température
   moyenne de janvier extraite d'une couche matricielle (WorldClim via
   `geodata` + `terra::extract`) serait bien plus directe. C'est la principale
   piste d'amélioration du module 10.
3. **La baie d'Hudson est traitée comme un océan modérateur** alors qu'elle est
   gelée une bonne partie de l'année : la continentalité du Manitoba est
   sous-estimée.
4. **n = 64 unités.** C'est peu pour une régression à 4 variables, et ça limite
   la puissance de tous les tests. Passer aux divisions de recensement (~293)
   et aux comtés américains (~3143) donnerait des centaines d'unités et
   rendrait la GWR pertinente — mais exige des fichiers de découpage externes
   (`cancensus`, `tigris`).
5. **La comparaison d'échelles du module 07 n'est pas un MAUP pur** : les trois
   lignes de la synthèse diffèrent par l'échelle *et* par la variable.
   Normaliser les hexagones exigerait une grille de population matricielle.
6. **64 lieux de naissance n'ont pas de coordonnées** dans le cache de
   géocodage (signalé par le module 01). Les joueurs concernés sont exclus de
   toutes les analyses spatiales.
7. **Corrélation n'est pas causalité.** Ces modèles décrivent des associations
   spatiales, ils ne démontrent aucun mécanisme.

---

## Sorties

Dans `sorties/` : 19 figures PNG à 300 dpi (mêmes réglages que les modules
descriptifs), 16 tableaux CSV, 5 journaux `.txt` et 1 fichier `.rds`
intermédiaire (réutilisé par les modules 07, 10 et 12, non versionné).

Les journaux `*_resultats_*.txt` contiennent les sorties complètes des tests
statistiques avec leur interprétation. Ce sont eux qu'il faut lire en premier :
ils sont rédigés pour pouvoir être cités directement dans le rapport sans avoir
à relancer le code.
