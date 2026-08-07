# Guide de reprise — modules 11 et 12

**Pour Xavier Lafrance.** Ton travail n'a pas été réécrit : il a été déplacé et
encadré. Ce document dit où il est, ce qui a changé, et ce qu'il reste à faire.

---

## En trente secondes

| Avant | Maintenant |
|---|---|
| `Projet_Hockey_script.R`, SECTION 5 (STKDE) | `R/11_stkde.R` |
| `Projet_Hockey_script.R`, SECTION 6 (titre seul) | `R/12_clustgeo.R` |
| Le script complet, intact | `archive/Projet_Hockey_script_ORIGINAL.R` |

Pour travailler sur ton module :

```r
source("R/11_stkde.R")
```

Il se charge tout seul — il source `R/00_config.R` si ce n'est pas déjà fait.
Pas besoin de lancer toute la chaîne.

Pour lancer tes deux modules d'un coup :

```r
source("run_all.R"); lancer_modules(c("11", "12"))
```

---

## Module 11 — STKDE

### État : vérifié, il tourne

Exécuté de bout en bout après installation de `sparr`, `gifski` et `viridis`.
Il produit :

- `figures/stkde_densite_temporelle.png`
- `figures/stkde_joueurs.gif` — 150 images

⏱️ **Compter environ 5 minutes et 2,2 Go de RAM.** À lui seul, ce module coûte
huit fois plus que les onze autres réunis (271 s sur 308 s pour un run
complet).

### Où passe le temps — mesuré, pas supposé

Le module se chronomètre lui-même et affiche la répartition en fin
d'exécution :

```
densite calculee en 199 s
rendu des 150 images en 79 s
REPARTITION : densite 199 s | rendu 79 s
```

Soit **73 % dans `spattemp.density()`** et **26 % dans le rendu des images**.

### `sres` : ne pas le baisser

C'est contre-intuitif, donc voici les chiffres. La fenêtre est le monde entier,
soit 34 735 km de large en projection cylindrique équivalente :

| `sres` | Taille de cellule | Cellules par largeur de bande (h = 100 km) |
|---|---|---|
| 250 | 139 km | **0,7** — le noyau n'est plus résolu |
| **500 (actuel)** | 69 km | **1,4** — déjà limite |
| 1000 | 35 km | 2,9 — confortable |

À `sres = 500` la grille est **déjà sous-résolue** par rapport au lissage
spatial. Baisser cette valeur n'est pas un compromis vitesse/qualité, c'est une
erreur de méthode : tu lisserais sur 100 km une grille dont la maille fait
139 km.

Recadrer la fenêtre n'aide pas beaucoup non plus : les lieux de naissance
couvrent 84 % de la largeur du monde et 62 % de sa surface. De Vancouver à
Vladivostok, ils sont réellement partout.

### `tres` : là, il y a un gain gratuit

| `tres` | Années par image | Images par largeur de bande (λ = 10 ans) |
|---|---|---|
| **150 (actuel)** | 0,85 | **11,7** — suréchantillonné d'un facteur ~12 |
| 64 | 2,0 | 5,0 — largement suffisant |

Le lissage temporel vaut 10 ans. Produire une image tous les 10 mois ne montre
rien de plus qu'une image tous les 2 ans : l'information supplémentaire a été
lissée par construction. Passer à `tres = 64` diviserait **à la fois** le calcul
de densité et le rendu par environ 2,3, pour un coût scientifique nul.

Estimation : **271 s → environ 115 s.**

```r
RESOLUTION_SPATIALE   <- 500   # ne pas baisser (voir le tableau ci-dessus)
RESOLUTION_TEMPORELLE <- 150   # -> 64 : deux fois plus rapide, sans perte
```

Je ne l'ai **pas changé moi-même** : c'est un paramètre de modélisation, et le
modifier change tes sorties. À toi de trancher, et de le justifier en rapport.

### Et le GPU ?

Question posée, réponse mesurée : **non, ça ne sert à rien ici.**

- `spattemp.density()` n'a même pas d'argument de parallélisation CPU, alors que
  ses voisines `bivariate.density()` et `LIK.spattemp()` en ont un. Elle appelle
  `density.ppp` de spatstat, du C mono-thread.
- `tmap`, `gifski`, `sf`, `spdep` : tous sur CPU, aucun backend CUDA.
- Les paquets R à backend GPU qui existent (`torch`, `tensorflow`) ne se
  branchent sur aucun de ceux-là. `gpuR` et `cuda.ml` sont archivés du CRAN.

Accélérer le STKDE par GPU voudrait dire réimplémenter l'estimateur en
opérations tensorielles. C'est faisable — un noyau gaussien spatio-temporel
séparable est une convolution — mais tu échangerais « j'ai utilisé `sparr`,
estimateur publié de Davies et Hazelton, cité au manuel » contre « j'ai réécrit
l'estimateur moi-même », et toute divergence numérique deviendrait ta charge de
preuve. Le coût pédagogique dépasse largement les quelques minutes gagnées.

**Si le temps devient vraiment gênant**, dans l'ordre de rentabilité :
1. `tres` 150 → 64 (facteur 2,3, gratuit) ;
2. paralléliser le rendu sur les 16 threads de la machine — `tmap_animation`
   écrit ses 150 images **séquentiellement** avec `tmap_save()`, donc écrire les
   PNG avec `parallel::parLapply()` puis appeler `gifski::gifski()` sur la liste
   de fichiers récupère l'essentiel des 79 s.

### Deux choses que j'ai remarquées, sans y toucher

Ce sont tes réglages d'origine, donc je les ai laissés — mais regarde le GIF et
juge :

1. **`tm_title(size = 8)`** : l'année occupe environ la moitié de la hauteur de
   chaque image, la carte est écrasée en dessous. `size = 2` ou `3` rééquilibre.
2. **`tm_legend(show = FALSE)`** : il n'y a aucune échelle de densité sur
   l'animation. Le lecteur voit des couleurs changer sans savoir ce qu'elles
   valent. Comme l'échelle est commune à toutes les images (`color_breaks`),
   l'afficher une fois serait juste, et rendrait le GIF citable en rapport.

### Ce qui n'a pas changé

**Tout le calcul.** Mêmes noms de variables (`prov_joueur_stkde`,
`prov_joueur_stkde_sf.ppp`, `ppp_jittered`, `dens_vals`, `all_rasts`,
`color_breaks`, `time_frames`, `all_maps`), même enchaînement, mêmes valeurs
de paramètres, même `stopifnot()` de vérification. Tu dois t'y retrouver
immédiatement.

### Ce qui a changé, et pourquoi

1. **Le bloc de géocodage a été retiré.** Il faisait exactement le même travail
   que la SECTION 3 du script (même fichier cache, même méthode `arcgis`), donc
   il ne géocodait jamais rien. C'est maintenant le module `R/01_geocodage.R`.
   Si tu ajoutes des joueurs au CSV, lance `source("R/01_geocodage.R")` avant.

2. **Les paramètres sont remontés en haut du fichier** :

   ```r
   H_SPATIAL   <- 100000   # ta valeur
   LAMBDA_TEMP <- 10       # ta valeur
   ESTIMER_BANDWIDTH <- FALSE
   ```

3. **`LIK.spattemp` est désactivé par défaut.** Il met plusieurs dizaines de
   minutes et bloquait `run_all.R`. Mets `ESTIMER_BANDWIDTH <- TRUE` quand tu
   veux le lancer.

4. **`tlim` n'est plus codé en dur.** `c(0, 128)` devenait faux dès que le CSV
   changeait ; c'est maintenant `range(dt_num)`.

5. **`set.seed(2026)` avant l'échantillonnage des densités.** Ton
   `sample(terra::values(x), size = 100)` tirait au hasard sans graine : la
   palette de couleurs changeait à chaque exécution, donc deux GIF produits le
   même jour n'étaient pas comparables.

6. **`tm_raster()` passé en syntaxe tmap 4.** `breaks =` et `palette =` sont
   des arguments tmap 3 ; ils deviennent
   `col.scale = tm_scale_intervals(breaks = …, values = …)`.

7. **Sortie renommée** : `figures/stkde_joueurs_5.gif` → `stkde_joueurs.gif`
   (le `_5` venait du numéro de section).

8. **Le module s'ignore proprement** si `sparr`, `gifski` ou `viridis` manquent,
   au lieu de faire planter la chaîne. Dans ce cas `run_all.R` affiche
   **`IGNORE`** (et non `ok`) dans son bilan, avec un avertissement explicite :
   un module sauté n'a produit aucune sortie, et il ne faut pas croire le
   contraire.

   ```r
   install.packages(c("sparr", "gifski", "viridis"))
   ```

### Les trois `# TODO XAVIER`

**1. La variable temporelle.** Ta note d'origine :

> *« Vaudrait plus la peine que la variable temporelle soit la première année
> de jeu dans la LNH plutôt que l'année de naissance. »*

Tu as raison sur le fond, mais le CSV ne contient pas la première saison. Deux
options : l'ajouter au fichier source, ou l'approximer par
`AnneeNaissance + 20`. L'approximation biaiserait les débuts et fins de
période — à mentionner dans le rapport si tu la retiens.

**2. Les largeurs de bande.** `H_SPATIAL` (100 km) et `LAMBDA_TEMP` (10 ans)
sont des valeurs rondes choisies à la main, pas optimisées. Lance
`LIK.spattemp` une fois, note les valeurs, reporte-les. **Tant que ce n'est pas
fait, le rapport doit dire que les largeurs sont fixées *a priori*.**

**3. Semis jitteré ou non ?** `LIK.spattemp` reçoit `ppp_jittered`, mais
`spattemp.density` reçoit `prov_joueur_stkde_sf.ppp` (non jitteré). Les deux
devraient être cohérents. Le jittering est indispensable dès qu'un calcul
dépend des distances entre paires ; pour une densité de noyau c'est moins
critique. À toi de trancher — je ne l'ai pas fait à ta place parce que ça
change le résultat.

---

## Module 12 — ClustGeo

Dans le script d'origine, la SECTION 6 se résumait à son titre :

```r
# SECTION 6 — ClustGeo ----
```

`R/12_clustgeo.R` est une **trame complète et exécutable**, calquée sur la
recette du manuel (section 8.2.1) et appliquée aux 64 provinces et états du
projet. Lance-la, regarde les sorties, puis change ce qui doit l'être.

```r
source("R/06_normalisation.R")   # produit unites_normalisees.rds
source("R/12_clustgeo.R")
```

### Ce qu'elle fait déjà

1. Construit les deux matrices de distance : **D0** sémantique (4 variables
   centrées-réduites) et **D1** spatiale (distance entre centroïdes, en
   projection métrique).
2. Balaie alpha de 0 à 1 avec `choicealpha()` et produit le graphique d'inertie
   (`sorties/12_graph_choix_alpha.png`).
3. Suggère un alpha automatiquement, classe avec `hclustgeo()`, coupe l'arbre à
   K = 5.
4. Produit aussi la classification **sans** contrainte spatiale (alpha = 0), et
   une carte comparative des deux. C'est cette comparaison qui démontre
   l'intérêt de la méthode.
5. Sort le profil moyen de chaque classe et la composition détaillée.

### Les quatre `# CHOIX XAVIER`

1. **Les variables de D0.** Actuellement `TauxPar100k`, `PtsPar100k`,
   `PtsMoyen`, `PartElite`. Tu peux ajouter celles du module 10 (latitude,
   densité de population, distance à la côte, distance à une équipe de la
   LNH) : la classification deviendrait une typologie de *contextes* et plus
   seulement de *résultats*. Évite deux variables quasi identiques, elles
   compteraient double.
2. **K, le nombre de classes.** 5, comme dans le manuel. Avec 64 unités, entre
   4 et 6 est raisonnable.
3. **Alpha.** La suggestion automatique est une règle simple (le plus grand
   alpha qui conserve 90 % de l'inertie sémantique), pas une vérité. **Regarde
   `12_graph_choix_alpha.png`**, choisis, et justifie dans le rapport. Le manuel
   retient 0,30 pour son jeu de données.
4. **Nommer les classes.** Le tableau `12_table_profils_classes.csv` donne les
   moyennes de chacune. Donne-leur des noms parlants. Une classification qu'on
   ne sait pas nommer est une classification qu'on n'a pas comprise.

### Pour aller plus loin

**SKATER** (`spdep::skater`) est l'autre grande méthode du chapitre 8 du
manuel, et elle y occupe plus de place que ClustGeo. Différence essentielle :
SKATER garantit que chaque classe est un bloc **connexe** sur la carte, ce que
ClustGeo ne fait pas. Comparer les deux ferait une bonne sous-section.

---

## Ce que tu dois savoir sur le reste du projet

**Le pipe.** Tout est en `|>`, jamais `%>%`. Attention, la fonction de droite
doit être un appel : `x |> st_make_valid()` et non `x |> st_make_valid`.

**Les fonctions communes** sont dans `R/00_config.R` :

| Fonction | Ce qu'elle donne |
|---|---|
| `charger_hockey()` | les 8 802 joueurs, dates et variables déjà préparées |
| `charger_lieux_geocodes()` | le cache de coordonnées |
| `construire_villes_sf()` | un objet `sf` agrégé par lieu de naissance |
| `charger_monde()` | le fond de carte mondial |
| `sauver_graphique_fig()` / `sauver_carte_fig()` | écrit dans `figures/` |
| `sauver_graphique()` / `sauver_carte()` / `sauver_tableau()` | écrit dans `sorties/` |
| `journal_resultats()` | ouvre un journal `.txt` pour archiver des résultats |

Elles sont toutes **mises en cache** : appeler `charger_hockey()` dans trois
modules ne relit le CSV qu'une fois.

**Les constantes** : `CREDITS`, `AUTEURS`, `COULEURS_GEO`, `CRS_GEO`, `CRS_CA`,
`CRS_NA`, `CRS_ATL`, `CRS_MONDE`, `SEUIL_ELITE_PTS`.

**Deux dossiers de sortie** : `figures/` (non versionné, descriptif) et
`sorties/` (versionné, statistique). Ton GIF va dans `figures/`, exactement
comme avant.

---

## Si tu préfères revenir en arrière

Le script d'origine est intact dans
`archive/Projet_Hockey_script_ORIGINAL.R`, et ta branche `XavierL` existe
toujours :

```bash
git checkout XavierL
```

Rien n'a été supprimé.
