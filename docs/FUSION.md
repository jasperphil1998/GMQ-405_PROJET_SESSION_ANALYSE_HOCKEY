# Journal de la fusion des branches

Ce document explique ce qui a été fusionné, ce qui a été déplacé et ce qui a
été corrigé. Il sert à retrouver son travail après la réorganisation.

---

## 1. L'état de départ

Le dépôt comptait cinq branches, dont deux structures de projet incompatibles.

| Branche | Contenu propre | Rapport aux autres |
|---|---|---|
| `main` | script principal, `%>%` converti en `\|>` | base commune |
| `origin/main` | `main` + section 5 (titre) + une ligne `# test` | le plus avancé côté `main` |
| `XavierL` | **+ le corps de la section 5 (STKDE)** et le titre de la section 6 | la version la plus complète du script |
| `analyse-spatiale` | **+ le dossier `analyse_spatiale/`** (5 modules statistiques) mais un script principal figé avant la conversion de pipe | structure parallèle |
| `origin/phil` | travail sur les couleurs et les points des graphiques | déjà présent ailleurs (voir §4) |
| `test_branche` | — | ancêtre de tout le reste |

Le problème de fond : `analyse-spatiale` et `XavierL` avaient divergé à partir
du commit `2dc7ca0`. Chacune faisait avancer une moitié du projet sans voir
l'autre.

---

## 2. Ce qui a été fait, dans l'ordre

```text
2dc7ca0 ─┬─ main ── 8c10e8b ─┬─ XavierL (STKDE)       ─┐
         │                   └─ origin/main (# test)  ─┤
         └─ analyse-spatiale (dossier statistique)  ───┴─→  fusion
```

1. Branche `fusion` créée depuis `analyse-spatiale`.
2. `XavierL` fusionnée — **aucun conflit** : `analyse-spatiale` n'avait pas
   touché au script depuis leur ancêtre commun, donc git a simplement pris la
   version de Xavier.
3. `origin/main` fusionnée — un seul conflit trivial, la ligne `# test` en fin
   de fichier.
4. `origin/phil` marquée comme intégrée (voir §4).
5. `test_branche` : déjà un ancêtre, rien à faire.

**Vérification** : les huit références (`main`, `XavierL`, `origin/main`,
`origin/phil`, `origin/XavierL`, `origin/analyse-spatiale`, `test_branche`,
`analyse-spatiale`) sont toutes des ancêtres de `fusion`. Rien n'a été perdu.

---

## 3. Où est passé mon travail ?

### Le script principal → `archive/` + modules `R/01` à `R/05`

`Projet_Hockey_script.R` est conservé **intégralement et sans modification**
dans `archive/Projet_Hockey_script_ORIGINAL.R`. Voir
[`archive/README.md`](../archive/README.md) pour la table de correspondance
section par section.

### Le dossier `analyse_spatiale/` → racine du projet

| Avant | Après |
|---|---|
| `analyse_spatiale/R/00_config.R` | fusionné dans `R/00_config.R` |
| `analyse_spatiale/R/01_normalisation.R` | `R/06_normalisation.R` |
| `analyse_spatiale/R/02_autocorrelation.R` | `R/07_autocorrelation.R` |
| `analyse_spatiale/R/03_semis_points.R` | `R/08_semis_points.R` |
| `analyse_spatiale/R/04_centrographie.R` | `R/09_centrographie.R` |
| `analyse_spatiale/R/05_modelisation.R` | `R/10_modelisation.R` |
| `analyse_spatiale/run_all.R` | `run_all.R` (réécrit) |
| `analyse_spatiale/data/*.csv` | `data/` |
| `analyse_spatiale/sorties/0N_*` | `sorties/` avec le préfixe décalé (01→06 … 05→10) |
| `analyse_spatiale/README.md` | `docs/RESULTATS_analyse_spatiale.md` |
| `analyse_spatiale/REFERENCES.md` | `docs/REFERENCES.md` |

Le décalage des numéros était nécessaire : les modules descriptifs occupent
maintenant 01 à 05. Les fichiers de sortie ont été renommés en conséquence,
donc `01_carte_ql_pays.png` s'appelle désormais `06_carte_ql_pays.png`.

Tous les déplacements ont été faits avec `git mv` : l'historique de chaque
fichier est préservé et `git log --follow` fonctionne.

---

## 4. Le cas de `origin/phil`

`origin/phil` (commit `924fb60`, « Rajouter les points et les couleurs dans les
graphiques ») porte le **même travail** que le commit `2dc7ca0` (« chagement
philou ») déjà présent sur la ligne principale. Les deux commits sont deux
enregistrements de la même session de travail sur deux branches différentes.

Vérification faite ligne à ligne, après normalisation des pipes : **aucune
ligne de `origin/phil` n'est absente de la branche de fusion.**

La branche a donc été fusionnée avec la stratégie `ours` : le graphe montre
qu'elle est intégrée (les futures fusions seront propres), sans réintroduire un
doublon du même code.

Effet de bord utile : ça évite aussi de réintroduire la coquille `gi`, qui
existait du côté `analyse-spatiale` mais pas du côté `phil`.

---

## 5. Corrections faites au passage

Toutes portent sur des problèmes qui existaient **avant** la fusion.

| Problème | Où | Correction |
|---|---|---|
| `gi` isolé ligne 661 : `object 'gi' not found` | script d'origine | supprimé |
| `View()` bloque l'exécution par `Rscript` | lignes 33, 659 | remplacé par `message()` |
| Carte de l'Ontario écrite **deux fois** à l'identique | lignes 949-1036 | doublon supprimé |
| `villes_points_geo` reconstruit **6 fois** à l'identique | SECTION 3 | calculé une fois, mis en cache |
| `ne_countries()` appelé une douzaine de fois | tout le script | mis en cache |
| Bloc de carte régionale recopié **14 fois** | SECTION 3 | factorisé en `carte_villes_region()` |
| `tm_symbols(scale = …)` : argument tmap 3 ignoré par tmap 4 | toutes les cartes à cercles | `size.scale = tm_scale_continuous(values.scale = …)` |
| `tm_symbols(alpha = …)` : idem | toutes les cartes à cercles | `fill_alpha =` |
| `scale_y_continuous(trans = …)` : argument déprécié dans ggplot2 4 | graphique 11 | `transform =` |
| Palette du STKDE différente à chaque exécution | section 5 | `set.seed(2026)` avant l'échantillonnage |
| `tlim = c(0, 128)` codé en dur | section 5 | déduit des données |
| Prairies (MB, SK) sans aucune carte régionale | SECTION 3 | carte ajoutée |
| Mention « Source / Auteur » recopiée ~40 fois | tout le script | constante `CREDITS` |
| `recode()` des positions cherchait `LW`/`RW`, absents des données (qui utilisent `L`, `R`, `F`, `W`) : **3 408 joueurs sur 8 802 gardaient un code brut** dans les graphiques 6 et 11 | SECTION 1 | codes réels recodés |

---

## 6. Alignement sur les méthodes du cours

Le manuel de référence est Apparicio et Gelb, *Méthodes d'analyse spatiale :
un grand bol d'R*. Les paquets utilisés y étaient déjà conformes (`sf`, `tmap`,
`spdep`, `spatstat`, `spatialreg`, `spgwr`). Trois ajouts vont plus loin dans
cette direction :

1. **`moran.mc` (999 permutations)** ajouté à côté de `moran.test`, module 07.
   Le manuel privilégie nettement la version par permutations (28 occurrences
   contre 5) : l'approximation analytique suppose la normalité, hypothèse
   fragile avec 64 unités et une variable très dissymétrique. Les deux sont
   rapportées et leur concordance est vérifiée automatiquement.
2. **`localG_perm`** ajouté à côté de `localG`, module 07, pour la même raison.
3. **Ellipse de déviation standard** ajoutée au module 09 (manuel, section
   3.2.2). Le cercle de distance standard ne montre pas l'**orientation** du
   semis ; l'ellipse si. Résultat : un azimut de 73° à 82° selon la décennie,
   c'est-à-dire un étirement est-ouest le long du corridor habité — et un grand
   axe qui passe de 1 070 km (années 1890) à 1 777 km (années 2000).

Le vocabulaire a aussi été aligné : « distance standard » plutôt que
« distance-type », terme du manuel.

### Le pipe

Tout le projet utilise `|>`, y compris les modules statistiques qui étaient
encore en `%>%`. C'est la convention que l'équipe avait déjà adoptée sur `main`
(commit « Remplacer %>% par |> ») ; il n'y avait aucune raison que la moitié du
projet reste en magrittr. Aucun usage ne dépendait de la sémantique propre à
`%>%` (pas de placeholder `.`, pas de `{ }`), la conversion est donc sans
effet sur les résultats.

---

## 7. État de la chaîne

`Rscript run_all.R` produit un bilan avec trois états possibles, et la
distinction compte :

| État | Signification |
|---|---|
| `ok` | le module a tourné **et produit ses sorties** |
| `IGNORE` | le module s'est sauté lui-même faute d'un paquet optionnel — **aucune sortie produite** |
| `ECHEC` | le module a planté ; les suivants ont continué |

Cette distinction a été ajoutée après coup, parce que le premier bilan affichait
`ok` pour un module qui s'était entièrement sauté : on ne s'en apercevait qu'en
cherchant une sortie qui n'existait pas. Un module ignoré est maintenant signalé
explicitement en fin d'exécution.

**Modules 01 à 10 et 12** : vérifiés à l'exécution, y compris depuis un état
froid (`figures/` et `sorties/*.rds` supprimés). Environ 40 secondes au total.

**Module 11 (STKDE)** : vérifié à l'exécution après installation de `sparr`,
`gifski` et `viridis`. Il produit bien `figures/stkde_densite_temporelle.png`
et `figures/stkde_joueurs.gif` (150 images).

⏱️ **Mais il coûte à lui seul 5 min 20 s et 2,2 Go de RAM**, contre 40 secondes
pour les onze autres modules réunis. C'est `spattemp.density()` avec
`sres = 500` et `tres = 150`, soit 37,5 millions de cellules. Si le temps
d'exécution devient gênant, baisser ces deux résolutions dans l'en-tête du
module divise le coût d'autant — mais c'est une décision de Xavier, pas un
réglage à changer par commodité.

---

## 8. Ce qu'il reste à décider

- **Fusionner `fusion` dans `main`** — c'est une décision d'équipe, pas une
  décision technique. La commande est dans le README.
- **Supprimer les anciennes branches** une fois la fusion faite, pour que
  personne ne reparte d'une version périmée.
- **Les 64 lieux sans coordonnées** signalés par le module 01 : à corriger ou à
  mentionner dans les limites du rapport.
