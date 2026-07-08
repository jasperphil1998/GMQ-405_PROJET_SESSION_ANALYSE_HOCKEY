# Projet Hockey — Analyse géomatique des joueurs de la LNH

Ce projet vise à analyser la provenance géographique des joueurs de la LNH à partir d’un jeu de données contenant leurs informations de naissance, leurs statistiques en carrière et leur position. Le script produit des graphiques statistiques, des cartes thématiques et des tableaux de sortie permettant d’étudier la distribution spatiale des joueurs, la production offensive par région et la concentration des joueurs d’élite.

Projet réalisé dans le cadre du cours **GMQ-405**.

## Auteurs

- Philippe Filion
- Xavier Lafrance
- Xavier St-Arnaud

## Objectifs du projet

Le projet cherche principalement à répondre aux questions suivantes :

- Quels pays produisent le plus de joueurs de la LNH?
- Comment la provenance des joueurs évolue-t-elle selon les décennies de naissance?
- Quelles villes ont produit le plus de joueurs ou le plus de points en carrière?
- Où se concentrent les joueurs de 1000 points et plus?
- Quels pays cumulent le plus de minutes de pénalité?
- Existe-t-il des régions plus productives au Canada, aux États-Unis et en Europe?

## Fonctionnalités principales

Le script `Projet_Hockey_script.R` permet de :

1. importer et préparer les données des joueurs;
2. nettoyer les dates de naissance et créer des variables d’analyse;
3. regrouper les joueurs par pays, ville, décennie et groupe géographique;
4. produire des graphiques statistiques avec `ggplot2`;
5. géocoder les lieux de naissance avec `tidygeocoder`;
6. créer des objets spatiaux avec `sf`;
7. produire des cartes thématiques avec `tmap`;
8. exporter les figures et tableaux dans le dossier `figures/`.

## Structure du projet

```text
GMQ-405_PROJET_SESSION_ANALYSE_HOCKEY/
├── Projet_Hockey_script.R      # Script principal d'analyse
├── install_packages.R          # Installe toutes les dépendances (à lancer une fois)
├── README.md
├── .vscode/
│   └── settings.json           # Config VS Code pour R
├── data/
│   ├── GMQ-405_Hockey_Players_complet_lieux_modernes.csv   # Données utilisées
│   ├── geocodage/
│   │   └── lieux_naissance_geocodes_lieux_modernes.csv     # Cache de géocodage
│   └── source/                 # Fichiers sources originaux (non utilisés par le script)
└── figures/                    # Sorties générées (non versionné, recréé à l'exécution)
```

Les dossiers `figures/` et `data/geocodage/` sont créés automatiquement par le script s'ils n'existent pas déjà. Le dossier `figures/` n'est volontairement pas versionné (voir `.gitignore`) : il est régénéré à chaque exécution.

## Données nécessaires

Le script utilise le fichier CSV suivant :

```text
data/GMQ-405_Hockey_Players_complet_lieux_modernes.csv
```

Le jeu de données doit contenir au minimum les champs suivants :

| Champ | Description |
|---|---|
| `Player Name` | Nom du joueur |
| `Pos.` | Position principale du joueur |
| `Birthdate` | Date de naissance, idéalement au format jour-mois-année |
| `Birthplace` | Lieu de naissance |
| `Country` | Pays de naissance |
| `GP` | Matchs joués en carrière |
| `G` | Buts en carrière |
| `A` | Passes en carrière |
| `Pts` | Points en carrière |
| `PIM` | Minutes de pénalité |

Un fichier de géocodage déjà préparé est aussi utilisé pour éviter de relancer le géocodage à chaque exécution :

```text
data/geocodage/lieux_naissance_geocodes_lieux_modernes.csv
```

Si ce fichier n’existe pas, la section de géocodage du script peut être relancée pour créer un nouveau fichier `lieux_naissance_geocodes.csv`. Le géocodage utilise OpenStreetMap et peut donc prendre du temps selon le nombre de lieux à traiter.

## Librairies R utilisées

Le script utilise les librairies suivantes :

```r
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidygeocoder)
library(scales)
```

Une section du script charge aussi `rnaturalearthhires`. Cette librairie peut être facultative selon les cartes produites et l’installation locale de R. Si elle n’est pas disponible, il est possible de la retirer du script si les fonds de carte standards de `rnaturalearth` suffisent.

## Installation des dépendances

**Le plus simple** : ouvrir `install_packages.R` et l'exécuter (« Source »), ou dans un terminal :

```bash
Rscript install_packages.R
```

Ce script installe automatiquement tous les packages requis, y compris `rnaturalearthhires`
(qui n'est **pas** sur le CRAN et provient du dépôt r-universe).

Sinon, manuellement dans R :

```r
install.packages(c(
  "readr", "dplyr", "tidyr", "stringr", "lubridate", "ggplot2",
  "sf", "tmap", "rnaturalearth", "rnaturalearthdata", "tidygeocoder", "scales"
))
# Package supplémentaire hors CRAN (fonds de carte détaillés) :
install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev")
```

Selon l'environnement utilisé, l'installation de `sf` peut nécessiter des dépendances géospatiales supplémentaires.

## Exécution du script (VS Code)

Prérequis : **R**, **VS Code** avec l'extension **R** (`REditorSupport.r`), et le package `languageserver` (installé par `install_packages.R`).

1. Cloner le dépôt, puis ouvrir le **dossier** du projet dans VS Code (`File > Open Folder`).
   > Important : ouvrir le dossier, pas seulement le fichier `.R`. C'est ce qui permet aux
   > chemins relatifs (`data/...`) de fonctionner sans configuration.
2. Lancer `install_packages.R` une première fois pour installer les dépendances.
3. Ouvrir `Projet_Hockey_script.R` et exécuter le code :
   - ligne par ligne / par bloc avec **Ctrl+Entrée**, ou
   - tout le fichier avec **Ctrl+Shift+S** (Run Source).

Le script n'utilise **aucun chemin absolu** : tout est relatif au dossier du projet, donc il
fonctionne tel quel sur n'importe quel ordinateur une fois le dépôt cloné. Aucune modification
de `setwd()` n'est nécessaire.

## Traitements réalisés

### Nettoyage et préparation

Le script commence par convertir les dates de naissance, extraire l’année de naissance et créer une variable de décennie. Il crée aussi une catégorie `Elite1000`, qui distingue les joueurs ayant atteint 1000 points ou plus en carrière.

Les pays sont ensuite regroupés en quatre grands groupes géographiques :

- Canada;
- USA;
- Europe;
- Autres.

### Graphiques statistiques

Les graphiques produits permettent d’analyser :

- les 15 principaux pays de naissance;
- le nombre de joueurs par décennie de naissance;
- l’évolution de la provenance des joueurs par décennie;
- les 20 principaux lieux de naissance;
- la moyenne de points par pays;
- la distribution des points selon la position;
- la relation entre matchs joués et points;
- les villes ayant produit le plus de points;
- les joueurs de 1000 points et plus par pays;
- les pays avec le plus de minutes de pénalité.

### Cartographie

Les cartes sont produites avec `tmap` à partir de fonds de carte provenant de `rnaturalearth`. Le script réalise des cartes à plusieurs échelles :

- monde;
- pays;
- villes de naissance;
- Canada;
- Québec;
- Ontario;
- Alberta;
- Colombie-Britannique;
- provinces atlantiques;
- régions des États-Unis;
- Europe;
- pays nordiques;
- Russie européenne;
- Russie asiatique.

Certaines corrections de noms de pays sont effectuées afin de faciliter la jointure avec le fond de carte, par exemple :

- `USA` devient `United States of America`;
- `Czech Republic` devient `Czechia`;
- `England`, `Scotland`, `Wales` et `Northern Ireland` sont regroupés avec `United Kingdom`.

## Fichiers exportés

### Graphiques

Les principaux graphiques exportés dans `figures/` sont :

```text
graph_top15_pays.png
graph_joueurs_decennie.png
graph_evolution_geo_proportion.png
graph_top20_villes.png
graph_moyenne_pts_pays.png
graph_boxplot_position.png
graph_relation_gp_pts.png
graph_top20_villes_points.png
graph_elite_pays.png
graph_pays_minutes_penalite.png
graph_points_position.png
```

### Cartes

Les principales cartes exportées dans `figures/` sont :

```text
carte_joueurs_pays.png
carte_elite_pays.png
carte_top_villes.png
carte_villes_elite.png
carte_villes_points.png
carte_villes_points_quebec.png
carte_villes_points_ontario.png
carte_villes_points_alberta.png
carte_villes_points_bc.png
carte_villes_points_atlantique.png
carte_villes_points_us_northeast.png
carte_villes_points_us_midwest.png
carte_villes_points_us_south.png
carte_villes_points_us_mountain_southwest.png
carte_villes_points_us_pacific.png
carte_villes_points_us_alaska_hawaii.png
carte_villes_points_europe.png
carte_villes_points_nordiques.png
carte_villes_points_russie_europeenne.png
carte_villes_points_russie_asiatique.png
```

### Tableaux CSV

Les tableaux suivants sont aussi exportés dans `figures/` :

```text
table_joueurs_pays.csv
table_top20_villes.csv
table_top20_villes_points.csv
table_joueurs_1000pts.csv
```

## Notes importantes

- Le géocodage peut être long et dépend d’un service externe. Il est recommandé d’utiliser le fichier de géocodage sauvegardé lorsque possible.
- Les lieux de naissance doivent être uniformisés pour obtenir de bons résultats cartographiques.
- Le script utilise plusieurs boîtes de zoom manuelles pour les cartes régionales. Ces limites peuvent être ajustées au besoin selon la zone à mettre en valeur.
- Certaines sections recréent des objets déjà produits plus tôt dans le script. Cela facilite l’exécution par blocs, mais le script pourrait être optimisé pour éviter les répétitions.
- Les cartes et graphiques sont exportés en PNG avec une résolution de 300 dpi, ce qui convient pour un rapport universitaire.

## Résultat attendu

À la fin de l’exécution, le dossier `figures/` contient l’ensemble des graphiques, cartes et tableaux nécessaires pour appuyer l’analyse de la provenance géographique des joueurs de la LNH.

Ce projet permet donc de combiner analyse statistique, traitement de données spatiales et cartographie thématique afin de mieux comprendre la géographie du hockey professionnel nord-américain.
