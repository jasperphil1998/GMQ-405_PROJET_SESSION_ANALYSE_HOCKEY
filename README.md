# Projet Hockey — Analyse géomatique des joueurs de la LNH

Analyse de la provenance géographique des joueurs de la LNH : d'où viennent-ils,
comment cette provenance a-t-elle évolué, et cette géographie est-elle
statistiquement structurée ?

Projet réalisé dans le cadre du cours **GMQ-405**.

## Auteurs

- Philippe Filion
- Xavier Lafrance
- Xavier St-Arnaud

---

## Ce qui a changé : le projet est maintenant modulaire

Le projet vivait auparavant sur cinq branches parallèles, dont deux structures
incompatibles : un script unique de 1 689 lignes (`Projet_Hockey_script.R`) et
un dossier `analyse_spatiale/` de scripts séparés. **Tout est maintenant réuni
dans un seul dossier `R/` de douze modules numérotés.**

Le script d'origine est conservé **intégralement** dans
[`archive/Projet_Hockey_script_ORIGINAL.R`](archive/Projet_Hockey_script_ORIGINAL.R).
Il n'est plus exécuté, mais il reste la référence : chaque module indique en
en-tête de quelle section il provient. Voir [`archive/README.md`](archive/README.md).

Détail complet de la fusion : [`docs/FUSION.md`](docs/FUSION.md).

---

## Démarrage rapide

```bash
Rscript install_packages.R
```

```bash
Rscript run_all.R
```

Ou dans la console R, après avoir ouvert le **dossier** du projet :

```r
source("run_all.R")
```

Pour ne relancer qu'une partie :

```r
source("run_all.R"); lancer_modules(c("06", "07", "10"))
```

---

## Structure

```text
GMQ-405_PROJET_SESSION_ANALYSE_HOCKEY/
├── run_all.R                   # Lance toute la chaîne (ou une sélection)
├── install_packages.R          # Installe les dépendances (à lancer une fois)
├── R/                          # Les douze modules
│   ├── 00_config.R             #   configuration commune (sourcée par tous)
│   ├── 01_geocodage.R          #   géocodage incrémental
│   ├── 02_graphiques.R         #   11 graphiques descriptifs
│   ├── 03_cartes_pays.R        #   2 cartes mondiales par pays
│   ├── 04_cartes_villes.R      #   18 cartes par ville de naissance
│   ├── 05_tableaux.R           #   6 tableaux pour le rapport
│   ├── 06_normalisation.R      #   taux et quotient de localisation
│   ├── 07_autocorrelation.R    #   Moran, LISA, Getis-Ord, MAUP
│   ├── 08_semis_points.R       #   densité de noyau, risque relatif, Ripley
│   ├── 09_centrographie.R      #   centres de gravité par décennie
│   ├── 10_modelisation.R       #   OLS, SEM, SAR, binomial négatif, GWR
│   ├── 11_stkde.R              #   ← chantier de Xavier Lafrance
│   └── 12_clustgeo.R           #   ← chantier de Xavier Lafrance
├── archive/
│   ├── Projet_Hockey_script_ORIGINAL.R   # le script d'origine, intact
│   └── README.md
├── data/
│   ├── GMQ-405_Hockey_Players_complet_lieux_modernes.csv
│   ├── equipes_lnh.csv                   # 32 équipes + coordonnées
│   ├── population_provinces_etats.csv    # 64 unités, recensements 2020-2021
│   ├── geocodage/                        # cache de géocodage
│   └── source/                           # fichiers sources originaux
├── docs/
│   ├── FUSION.md                         # ce qui a été fusionné et pourquoi
│   ├── GUIDE_XAVIER.md                   # pour reprendre les modules 11 et 12
│   ├── RESULTATS_analyse_spatiale.md     # résultats détaillés des modules 06-10
│   └── REFERENCES.md                     # bibliographie
├── figures/                              # sorties descriptives (NON versionnées)
└── sorties/                              # sorties statistiques (versionnées)
```

### Deux dossiers de sortie, et c'est voulu

| Dossier | Contenu | Versionné ? | Pourquoi |
|---|---|---|---|
| `figures/` | graphiques et cartes descriptives (modules 02-05, 11) | non | volumineux, se régénère en quelques minutes |
| `sorties/` | résultats des tests statistiques (modules 06-10, 12) | **oui** | on doit pouvoir citer un *p*-value dans le rapport sans relancer R |

Les fichiers `sorties/*_resultats_*.txt` contiennent les sorties complètes des
tests **avec leur interprétation rédigée**. Ce sont eux qu'il faut lire en
premier.

---

## Les douze modules

### Volet descriptif — « d'où viennent les joueurs ? »

| Module | Ce qu'il produit | Origine |
|---|---|---|
| **01** Géocodage | met à jour le cache de coordonnées | section 3 du script d'origine |
| **02** Graphiques | 11 figures : top pays, décennies, positions, pénalités, régression matchs/points | SECTION 1 |
| **03** Cartes par pays | 2 cartes mondiales à cercles proportionnels | SECTION 2 |
| **04** Cartes par ville | 18 cartes : monde, 7 régions canadiennes, 6 américaines, Europe, nordiques, Russie ×2 | SECTION 3 |
| **05** Tableaux | 6 CSV prêts à insérer dans le rapport | SECTION 4 |

### Volet statistique — « cette géographie est-elle réelle ? »

| Module | Ce qu'il démontre | Résultat marquant |
|---|---|---|
| **06** Normalisation | les effectifs bruts sont surtout une carte de la population | la Saskatchewan passe du **4ᵉ rang brut au 1ᵉʳ en taux** (QL = 24,8) |
| **07** Autocorrélation | la structure spatiale est statistiquement réelle | I de Moran = 0,320, *p* < 10⁻⁸ — mais **aucune** autocorrélation des points moyens par joueur |
| **08** Semis de points | densité continue plutôt que cercles saturés | le sud de l'Ontario atteint ~16,6 % de joueurs d'élite contre 6,5 % au national |
| **09** Centrographie | croise enfin l'espace et le temps | le centre mondial dérive de **2 201 km vers l'est** ; au Canada, **693 km vers l'ouest** (ρ = −0,80) |
| **10** Modélisation | explique, et pas seulement décrit | SEM retenu ; latitude (+) et densité de population (+) significatives ; **GWR rejetée** |

Interprétations complètes et mises en garde :
[`docs/RESULTATS_analyse_spatiale.md`](docs/RESULTATS_analyse_spatiale.md).

### Chantiers en cours

| Module | État | Responsable |
|---|---|---|
| **11** STKDE | vérifié à l'exécution, trois `# TODO XAVIER` ouverts | Xavier Lafrance |
| **12** ClustGeo | trame exécutable, quatre `# CHOIX XAVIER` à trancher | Xavier Lafrance |

Voir [`docs/GUIDE_XAVIER.md`](docs/GUIDE_XAVIER.md).

⏱️ **Le module 11 coûte 5 min 20 s et 2,2 Go de RAM** — huit fois plus que les
onze autres réunis. Pour une exécution rapide, le sauter :

```r
source("run_all.R"); lancer_modules(c("01","02","03","04","05","06","07","08","09","10","12"))
```

---

## Lire le bilan de `run_all.R`

| État | Signification |
|---|---|
| `ok` | le module a tourné **et produit ses sorties** |
| `IGNORE` | il s'est sauté faute d'un paquet optionnel — **aucune sortie** |
| `ECHEC` | il a planté ; les suivants ont continué |

La distinction `ok` / `IGNORE` compte : un module sauté affichait auparavant
`ok`, et on ne s'en apercevait qu'en cherchant une sortie qui n'existait pas.

---

## Dépendances entre modules

La plupart des modules sont indépendants. Trois contraintes seulement :

- **01 avant 04, 08, 09 et 11** — ils ont besoin des coordonnées ;
- **06 avant 07, 10 et 12** — il produit `sorties/unites_normalisees.rds` ;
- `run_all.R` respecte cet ordre tout seul.

Un module qui échoue n'arrête pas les autres : `run_all.R` affiche un bilan
final avec l'état de chacun.

---

## Conventions de code

**Pipe natif `|>`.** Tout le projet l'utilise, jamais `%>%`. C'est le choix pris
par l'équipe (commit « Remplacer %>% par |> ») et il évite une dépendance à
magrittr. Attention : la fonction de droite doit être un **appel**, donc
`x |> st_make_valid()` et non `x |> st_make_valid`.

**Méthodes du cours.** Les méthodes et les packages suivent le manuel
d'Apparicio et Gelb, *Méthodes d'analyse spatiale : un grand bol d'R* :
`sf` et `tmap` (chap. 1), `spdep` (chap. 2), `spatstat` (chap. 3-4),
`spatialreg` et `spgwr` (chap. 7), `ClustGeo` (chap. 8). Chaque module renvoie
à la section correspondante.

**Syntaxe tmap 4.** Le projet utilise `tm_scale_*`, `tm_legend()`, `tm_title()`,
`fill` / `col` séparés. Les tournures tmap 3 (`alpha =`, `scale =` nu,
`style =` dans `tm_polygons`) ont été converties.

**Projections.** Les cartes descriptives restent en EPSG:4326 (acceptable pour
afficher des symboles). Dès qu'un calcul porte sur une **distance**, une
**aire** ou une **densité**, on projette : Albers équivalente pour l'Amérique du
Nord, EPSG:3347 pour le Canada, LAEA pour l'Atlantique Nord. Un degré de
longitude vaut ~78 km à Toronto et ~52 km à Yellowknife : la précaution n'est
pas cosmétique.

**Interprétations générées, pas rédigées d'avance.** Les textes des journaux de
sortie sont calculés à partir des valeurs obtenues. Si les données changent, les
conclusions écrites changent avec elles.

---

## Données

| Fichier | Contenu |
|---|---|
| `data/GMQ-405_Hockey_Players_complet_lieux_modernes.csv` | 8 802 joueurs (jeu de données principal) |
| `data/geocodage/lieux_naissance_geocodes_lieux_modernes.csv` | cache de 2 324 lieux géocodés |
| `data/equipes_lnh.csv` | 32 équipes de la LNH avec coordonnées de leur ville |
| `data/population_provinces_etats.csv` | 64 unités : 13 provinces et territoires + 51 états |

Champs requis dans le jeu principal : `Player Name`, `Pos.`, `Birthdate`
(jour-mois-année), `Birthplace`, `Country`, `GP`, `G`, `A`, `Pts`, `PIM`.

> ✅ **Populations vérifiées à la source le 2026-08-08.** Les 64 valeurs de
> `population_provinces_etats.csv` ont été confrontées une à une aux tableaux
> officiels : **aucun écart**, et les deux totaux tombent juste (Canada
> 36 991 981 ; États-Unis 331 449 281). Le fichier porte maintenant les colonnes
> `Source`, `URL_source` et `DateVerification`. Citations complètes dans
> [docs/REFERENCES.md](docs/REFERENCES.md), section C.
>
> ⚠️ **Attention, l'échelle des pays est une autre histoire.** Le module 06,
> partie A, n'utilise pas ce fichier : il prend le champ `pop_est` du fond de
> carte Natural Earth, qui est une *estimation* compilée par Natural Earth
> (`POP_YEAR = 2019`), pas un chiffre de recensement. À signaler comme tel dans
> le rapport.

> ℹ️ Le module 01 signale que **64 lieux du cache n'ont pas de coordonnées**
> (Shawinigan, Chambly, Charlottetown, quelques villes européennes…). Ces
> joueurs sont exclus de toutes les analyses spatiales. C'est un point à
> corriger ou, à défaut, à mentionner dans les limites du rapport.

---

## Installation

```bash
Rscript install_packages.R
```

Le script installe les paquets du CRAN, puis `rnaturalearthhires` depuis
r-universe (il n'est **pas** sur le CRAN). Selon l'environnement, l'installation
de `sf` peut demander des dépendances géospatiales système.

**Requis** : `readr`, `dplyr`, `tidyr`, `stringr`, `lubridate`, `ggplot2`,
`scales`, `sf`, `tmap`, `rnaturalearth`, `rnaturalearthdata`, `spdep`,
`spatialreg`, `spatstat.geom`, `spatstat.explore`.

**Optionnels** (les modules concernés sont simplement ignorés s'ils manquent) :
`rnaturalearthhires`, `tidygeocoder`, `ggrepel`, `MASS`, `spgwr`, `ClustGeo`,
`sparr`, `terra`, `gifski`, `classInt`, `viridis`.

---

## Exécution dans VS Code

Prérequis : **R**, **VS Code** avec l'extension **R** (`REditorSupport.r`), et le
paquet `languageserver` (installé par `install_packages.R`).

1. Ouvrir le **dossier** du projet (`File > Open Folder`) — pas seulement un
   fichier `.R`. C'est ce qui fait fonctionner les chemins relatifs.
2. Lancer `install_packages.R` une première fois.
3. Ouvrir un module et l'exécuter avec **Ctrl+Entrée** (bloc) ou
   **Ctrl+Shift+S** (fichier entier). Chaque module se charge tout seul :
   il source `00_config.R` s'il n'est pas déjà chargé.

Aucun chemin absolu, aucun `setwd()` : le projet fonctionne tel quel sur
n'importe quel poste une fois le dépôt cloné.

### Dépannage : « The terminal failed to launch » / R introuvable

L'extension R détecte normalement R automatiquement. Si le terminal refuse de
démarrer, indiquez où se trouve **votre** installation de R dans vos réglages
**UTILISATEUR** (pas ceux du projet) :

1. `Ctrl + ,` → icône `{}` en haut à droite → `settings.json` (User).
2. Ajouter, en adaptant à votre version :

   ```json
   "r.rterm.windows": "C:\\Program Files\\R\\R-4.4.2\\bin\\x64\\R.exe",
   "r.rpath.windows":  "C:\\Program Files\\R\\R-4.4.2\\bin\\x64\\R.exe"
   ```

3. Recharger VS Code (`Ctrl + Shift + P` → *Developer: Reload Window*).

---

## Limites à énoncer dans le rapport

1. **Le lieu de naissance n'est pas le lieu de développement.** Un joueur né à
   Toronto et formé en Saskatchewan compte comme torontois. Ce biais est
   structurel et aucune méthode statistique ne le corrige avec ces données.
   C'est la limite la plus importante de tout le projet.
2. **La latitude est une approximation grossière du climat** (module 10). Une
   température moyenne de janvier extraite d'une couche matricielle serait bien
   plus directe.
3. **La baie d'Hudson est traitée comme un océan modérateur** alors qu'elle est
   gelée une bonne partie de l'année : la continentalité du Manitoba est
   sous-estimée.
4. **n = 64 unités.** C'est peu pour une régression à 4 variables, et ça limite
   la puissance de tous les tests.
5. **La fonction L de Ripley** (module 08) est rapportée avec une mise en garde
   explicite : son hypothèse nulle est fausse d'avance. Voir le journal de
   sortie.
6. **Corrélation n'est pas causalité.** Ces modèles décrivent des associations
   spatiales, ils ne démontrent aucun mécanisme.
