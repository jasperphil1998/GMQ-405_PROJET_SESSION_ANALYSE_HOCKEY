# Archive — le script d'origine

## `Projet_Hockey_script_ORIGINAL.R`

C'est le script principal du projet **tel qu'il était** au moment de la fusion,
dans son état le plus avancé : la version de la branche `XavierL`
(commit `4a28724`, « Ajout du STKDE »), qui contient le travail cumulé des
trois coéquipiers.

**Ce fichier n'est plus exécuté.** Il est conservé comme sauvegarde et comme
référence : c'est la source dont proviennent les modules `R/01` à `R/05` et
`R/11`.

### Ne pas le modifier

Si vous voulez changer quelque chose, changez le module correspondant dans
`R/`. Ce fichier doit rester le témoin fidèle de l'état d'avant la fusion —
c'est toute son utilité. Le jour où un doute surgit sur ce que faisait le code
d'origine, c'est ici qu'on vient vérifier.

### Ce qu'il contient, et où c'est passé

| Section du script d'origine | Module correspondant |
|---|---|
| 1-2. Librairies, dossier de travail | `R/00_config.R` |
| 3-5. Importation, nettoyage, groupes géographiques | `R/00_config.R` (`charger_hockey()`) |
| SECTION 1 — Graphiques (11 figures) | `R/02_graphiques.R` |
| SECTION 2 — Cartes par pays | `R/03_cartes_pays.R` |
| SECTION 3 — Cartes par ville, dont le géocodage | `R/01_geocodage.R` + `R/04_cartes_villes.R` |
| SECTION 4 — Tableaux de sortie | `R/05_tableaux.R` |
| SECTION 5 — STKDE | `R/11_stkde.R` |
| SECTION 6 — ClustGeo (titre seul, aucun code) | `R/12_clustgeo.R` |

### Il ne s'exécute plus tel quel

Trois raisons, indépendantes de la fusion :

1. **Ligne 661 : `gi`.** Une coquille laissée par une frappe accidentelle entre
   deux blocs. R interprète ça comme un objet nommé `gi`, qui n'existe pas, et
   s'arrête sur `object 'gi' not found`. Corrigé dans les modules.
2. **`View()` aux lignes 33 et 659.** Ces appels ouvrent une fenêtre
   interactive : le script ne peut donc pas tourner avec `Rscript`. Remplacés
   par des `message()` dans les modules.
3. **La carte de l'Ontario apparaît deux fois** (lignes 949-992 et 993-1036),
   deux blocs identiques écrivant le même fichier. Le doublon a été supprimé.

Ces trois points sont documentés ici parce qu'ils expliquent des différences
visibles entre l'archive et les modules — pas pour critiquer le travail
d'origine, qui a produit l'essentiel du contenu du projet.

