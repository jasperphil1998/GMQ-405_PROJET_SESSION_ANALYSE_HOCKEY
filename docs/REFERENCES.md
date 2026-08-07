# Références scientifiques — Projet GMQ-405 (analyse géomatique des joueurs de la LNH)

Bibliographie de départ pour le rapport. Deux blocs :

- **A. Littérature de contenu** — ce que d'autres ont déjà trouvé sur la géographie
  de la production de joueurs de hockey. Sert à situer nos résultats et à écrire
  l'introduction et la discussion.
- **B. Littérature méthodologique** — la source à citer pour chaque méthode
  employée dans les modules 06 à 10. Sert à justifier les choix techniques.

**Statut de vérification.** Les entrées marquées ✅ ont été vérifiées en ligne
(journal, volume, pages, DOI). Celles marquées ⚠️ sont des classiques cités de
mémoire : la référence existe, mais **confirmez la pagination exacte** avant de
la mettre dans le rapport.

---

## A. Littérature de contenu : géographie et hockey

### A.1 — Effet du lieu de naissance (« birthplace effect »)

C'est le cœur théorique du projet. Ces travaux établissent que la taille de la
communauté de naissance influence la probabilité d'atteindre le sport
professionnel — exactement ce que mesurent nos taux et quotients de localisation
(module 06).

✅ **Côté, J., MacDonald, D. J., Baker, J., & Abernethy, B. (2006).** When "where" is
more important than "when": Birthplace and birthdate effects on the achievement
of sporting expertise. *Journal of Sports Sciences*, 24(10), 1065–1073.
<https://pubmed.ncbi.nlm.nih.gov/17115521/>
→ **La référence fondatrice.** Montre que les villes de taille moyenne
surproduisent des joueurs professionnels (LNH, NBA, MLB, PGA). Explique
directement pourquoi la Saskatchewan devance l'Ontario dans notre module 06.

✅ **Baker, J., & Logan, A. J. (2007).** Developmental contexts and sporting success:
Birth date and birthplace effects in National Hockey League draftees 2000–2005.
*British Journal of Sports Medicine*, 41(8), 515–517.
<https://pubmed.ncbi.nlm.nih.gov/17331975/>
→ Applique la même logique aux repêchés de la LNH, avec une fenêtre temporelle
proche de la nôtre.

✅ **Farah, L., Schorer, J., Baker, J., & Wattie, N. (2018).** Heterogeneity in
community size effects: Exploring variations in the production of National
Hockey League draftees between Canadian cities. *Frontiers in Psychology*, 9,
2746. <https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2018.02746/full>
→ **Très utile pour la discussion.** Montre que l'effet « taille de communauté »
n'est pas homogène d'une province à l'autre : il varie fortement à l'intérieur
d'une même classe de population. C'est un appui direct à notre résultat de
grappes régionales (LISA, module 07) plutôt qu'à un simple effet démographique.

✅ **Hancock, D. J., Vierimaa, M., & Newman, A. (2022).** The geography of talent
development. *Frontiers in Sports and Active Living*, 4, 1031227.
<https://www.frontiersin.org/journals/sports-and-active-living/articles/10.3389/fspor.2022.1031227/full>
→ Synthèse récente sur les « talent hotspots ». Bonne source pour le cadre
conceptuel de l'introduction.

⚠️ **Curtis, J. E., & Birch, J. S. (1987).** Size of community of origin and
recruitment to professional and Olympic hockey in North America. *Sociology of
Sport Journal*, 4(3), 229–244.
<https://journals.humankinetics.com/view/journals/ssj/4/3/article-p229.xml>
→ L'antécédent historique de toute cette littérature, spécifique au hockey.

### A.2 — Géographie de la production et de la migration des joueurs

✅ **O'Connell, S. (2015).** The production and migration geographies of professional
hockey: 1970–2010. *Papers in Applied Geography*, 1(4), 391–403.
<https://doi.org/10.1080/23754931.2015.1095790>
→ **La référence la plus proche de notre projet.** Analyse la production de
joueurs de la LNH par pays, par État et par comté, **normalisée par la
population** — donc la même démarche que notre module 06, à une échelle plus
fine. À citer pour justifier le passage de l'effectif brut au taux.

✅ **Kaida, L., & Kitchen, P. (2020).** It's cold and there's something to do: The
changing geography of Canadian National Hockey League players' hometowns.
*International Review for the Sociology of Sport*, 55(3).
<https://journals.sagepub.com/doi/abs/10.1177/1012690218789045>
→ **Directement pertinent pour le module 09 (centrographie temporelle) et le
module 10 (latitude comme proxy du climat).** Traite du déplacement dans le
temps des villes d'origine des joueurs canadiens et invoque explicitement le
froid comme facteur. À citer pour appuyer — ou nuancer — notre effet latitude
significatif et notre dérive de 693 km vers l'ouest.

### A.3 — Effet d'âge relatif (contexte, à mentionner comme facteur non contrôlé)

Notre jeu de données contient les dates de naissance mais nous n'exploitons pas
le mois. Signaler cette littérature montre qu'on connaît le facteur confondant.

⚠️ **Barnsley, R. H., Thompson, A. H., & Barnsley, P. E. (1985).** Hockey success and
birthdate: The relative age effect. *CAHPER Journal*, 51, 23–28.

⚠️ **Barnsley, R. H., & Thompson, A. H. (1988).** Birthdate and success in minor
hockey: The key to the NHL. *Canadian Journal of Behavioural Science*, 20(2),
167–176.

✅ **Nolan, J. E., & Howell, G. (2010).** Hockey success and birth date: The relative
age effect revisited. *International Review for the Sociology of Sport*, 45(4),
507–512. <https://journals.sagepub.com/doi/10.1177/1012690210371560>

---

## B. Littérature méthodologique

### B.0 — Le manuel du cours

✅ **Apparicio, P. et Gelb, J. (2026).** *Méthodes d'analyse spatiale : un grand
bol d'R*. Ressource éducative libre.
→ **La référence de base de tout le volet statistique du projet.** Les méthodes,
les packages et le vocabulaire des modules 06 à 12 en proviennent. Chaque module
renvoie à la section correspondante dans son en-tête :

| Module | Chapitre du manuel |
|---|---|
| 03, 04 — cartographie `tmap` | 1.5 |
| 06 — normalisation, projections | 1.2.1 |
| 07 — matrices de pondération, Moran, LISA, Getis-Ord | 2.2 à 2.4 |
| 08 — semis de points, densité de noyau, fonctions K et L | 3.1, 3.3, 3.4 |
| 09 — centre moyen, distance standard, ellipse | 3.2 |
| 10 — modèles SAR/SEM, GWR | 7.1, 7.3 |
| 11 — STKDE (`sparr`) | 4 |
| 12 — ClustGeo, SKATER | 8.1, 8.2 |

Deux points de méthode viennent directement du manuel et méritent d'être
défendus en rapport : le recours aux **tests par permutations** (`moran.mc`,
`localG_perm`, 999 simulations) plutôt qu'à l'approximation analytique, et la
mise en garde sur la **taille des ellipses de déviation standard**, qui varie
d'un logiciel à l'autre (section 3.2.2).

### B.1 — Module 06 : normalisation, taux et quotient de localisation

⚠️ **Tobler, W. R. (1970).** A computer movie simulating urban growth in the Detroit
region. *Economic Geography*, 46, 234–240.
→ La « première loi de la géographie ». Citation d'ouverture standard pour
justifier qu'on teste l'autocorrélation spatiale.

⚠️ **Isard, W. (1960).** *Methods of Regional Analysis: An Introduction to Regional
Science*. MIT Press.
→ Source classique du quotient de localisation. (Livre, pas article — si le
travail exige des articles, O'Connell 2015 ci-dessus suffit pour justifier la
normalisation par la population.)

### B.2 — Module 07 : autocorrélation spatiale

⚠️ **Moran, P. A. P. (1950).** Notes on continuous stochastic phenomena. *Biometrika*,
37(1/2), 17–23.
→ Source originale du *I* de Moran.

✅ **Anselin, L. (1995).** Local indicators of spatial association — LISA.
*Geographical Analysis*, 27(2), 93–115.
<https://doi.org/10.1111/j.1538-4632.1995.tb00338.x>
→ **À citer obligatoirement** pour notre carte LISA (grappe chaude des Prairies,
grappe froide de la Sun Belt).

✅ **Getis, A., & Ord, J. K. (1992).** The analysis of spatial association by use of
distance statistics. *Geographical Analysis*, 24(3), 189–206.
<https://doi.org/10.1111/j.1538-4632.1992.tb00261.x>
→ Source du Gi\*, notre carte de « points chauds ».

⚠️ **Ord, J. K., & Getis, A. (1995).** Local spatial autocorrelation statistics:
Distributional issues and an application. *Geographical Analysis*, 27(4),
286–306.
→ Version corrigée du Gi\*, celle qu'implémente `spdep::localG`.

✅ **Bivand, R. S., & Wong, D. W. S. (2018).** Comparing implementations of global and
local indicators of spatial association. *TEST*, 27(3), 716–748.
<https://doi.org/10.1007/s11749-018-0599-x>
→ **La citation propre pour le paquet `spdep`.** Discute aussi des choix de
matrice de voisinage — utile pour défendre notre k = 5 plus proches voisins
plutôt que la contiguïté « reine » (qui laissait l'Î.-P.-É. et Hawaï sans
voisin).

✅ **Fotheringham, A. S., & Wong, D. W. S. (1991).** The modifiable areal unit problem
in multivariate statistical analysis. *Environment and Planning A*, 23(7),
1025–1044. <https://doi.org/10.1068/a231025>
→ **La référence pour notre discussion du MAUP** (grille hexagonale de 200 km vs
provinces/États). Montre que l'effet du découpage est imprévisible en analyse
multivariée — exactement la mise en garde à écrire dans le rapport.

⚠️ **Openshaw, S. (1984).** *The Modifiable Areal Unit Problem*. CATMOG 38. Geo Books.
→ Le texte fondateur du MAUP. (Fascicule, pas article.)

### B.3 — Module 08 : semis de points, densité de noyau et risque relatif

✅ **Kelsall, J. E., & Diggle, P. J. (1995).** Kernel estimation of relative risk.
*Bernoulli*, 1(1–2), 3–16.

✅ **Kelsall, J. E., & Diggle, P. J. (1995).** Non-parametric estimation of spatial
variation in relative risk. *Statistics in Medicine*, 14(21–22), 2335–2342.
<https://pubmed.ncbi.nlm.nih.gov/8711273/>
→ **Les deux sources de notre surface de risque relatif.** Ce sont elles qui
justifient l'argument méthodologique le plus fort du projet : comme le risque
relatif est un *rapport* de deux densités, l'effet de la population s'annule et
aucune donnée démographique n'est requise.

⚠️ **Bithell, J. F. (1990).** An application of density estimation to geographical
epidemiology. *Statistics in Medicine*, 9(6), 691–701.
→ L'antécédent direct de Kelsall & Diggle.

✅ **Davies, T. M., Marshall, J. C., & Hazelton, M. L. (2018).** Tutorial on kernel
estimation of continuous spatial and spatiotemporal relative risk. *Statistics
in Medicine*, 37(7), 1191–1221.
→ **Le plus lisible des quatre**, et le plus proche de ce qu'on a codé. Bon
choix si vous ne citez qu'une seule référence sur le risque relatif.

✅ **Davies, T. M., Hazelton, M. L., & Marshall, J. C. (2011).** sparr: Analyzing
spatial relative risk using fixed and adaptive kernel density estimation in R.
*Journal of Statistical Software*, 39(1), 1–14. <https://www.jstatsoft.org/v39/i01/>

⚠️ **Ripley, B. D. (1977).** Modelling spatial patterns. *Journal of the Royal
Statistical Society, Series B*, 39(2), 172–212.
→ Source de la fonction K (et donc de notre fonction L). **À citer en même temps
que notre mise en garde** : l'hypothèse nulle d'homogénéité spatiale complète
n'est pas crédible ici, puisque les localités canadiennes ne peuvent pas se
répartir uniformément sur le territoire.

⚠️ **Baddeley, A., & Turner, R. (2005).** spatstat: An R package for analyzing spatial
point patterns. *Journal of Statistical Software*, 12(6), 1–42.
→ La citation pour `spatstat.geom` / `spatstat.explore`.

⚠️ **Diggle, P. J. (2013).** *Statistical Analysis of Spatial and Spatio-Temporal
Point Patterns* (3ᵉ éd.). CRC Press.
→ Manuel de référence, utile si un correcteur conteste le choix de σ = 100 km ou
la correction de bordure.

### B.4 — Module 09 : centrographie

⚠️ **Lefever, D. W. (1926).** Measuring geographic concentration by means of the
standard deviational ellipse. *American Journal of Sociology*, 32(1), 88–94.
→ Source classique du centre moyen et de la distance-type. Peu de références
plus récentes existent : la méthode est ancienne et stable.

### B.5 — Module 10 : modélisation spatiale

✅ **Anselin, L., Bera, A. K., Florax, R., & Yoon, M. J. (1996).** Simple diagnostic
tests for spatial dependence. *Regional Science and Urban Economics*, 26(1),
77–104. <https://doi.org/10.1016/0166-0462(95)02111-6>
→ **La référence exacte pour l'étape 3 de notre séquence** (tests du score de
Rao / multiplicateurs de Lagrange robustes servant à choisir entre SEM et SAR
lag). Indispensable à citer.

⚠️ **Anselin, L. (1988).** *Spatial Econometrics: Methods and Models*. Kluwer.
→ Le manuel fondateur, source du SEM et du SAR lag.

⚠️ **LeSage, J., & Pace, R. K. (2009).** *Introduction to Spatial Econometrics*. CRC
Press.
→ Traitement moderne, notamment sur l'interprétation des effets directs et
indirects dans un modèle à décalage spatial.

✅ **Brunsdon, C., Fotheringham, A. S., & Charlton, M. E. (1996).** Geographically
weighted regression: A method for exploring spatial nonstationarity.
*Geographical Analysis*, 28(4), 281–298.
<https://doi.org/10.1111/j.1538-4632.1996.tb00936.x>
→ **À citer même si la GWR est rejetée** — et surtout parce qu'elle l'est. Le
critère AICc utilisé pour la rejeter vient de cette lignée méthodologique ; le
rapport peut expliquer que la méthode a été essayée dans les règles puis écartée
sur un critère publié, ce qui est un argument fort.

⚠️ **Fotheringham, A. S., Brunsdon, C., & Charlton, M. (2002).** *Geographically
Weighted Regression: The Analysis of Spatially Varying Relationships*. Wiley.
→ Contient la discussion du surajustement et du choix de fenêtre — exactement
notre motif de rejet (fenêtre optimale ≈ 2 unités sur 64).

### B.6 — Outils logiciels (à citer en note de bas de page ou en annexe)

⚠️ **Pebesma, E. (2018).** Simple features for R: Standardized support for spatial
vector data. *The R Journal*, 10(1), 439–446. → paquet `sf`.

⚠️ **Tennekes, M. (2018).** tmap: Thematic maps in R. *Journal of Statistical
Software*, 84(6), 1–39. → paquet `tmap`.

⚠️ **Wickham, H. (2016).** *ggplot2: Elegant Graphics for Data Analysis* (2ᵉ éd.).
Springer.

En R, `citation("spdep")`, `citation("sf")`, `citation("tmap")`,
`citation("spatstat")` produisent la référence officielle de chaque paquet,
prête à copier.

---

## Pistes de recherche si vous en voulez davantage

Mots-clés qui donnent de bons résultats sur Google Scholar / Érudit / Scopus :

- `birthplace effect sport talent development`
- `community size effect athlete development`
- `sport geography talent production spatial`
- `relative age effect ice hockey`
- `location quotient sport participation`
- `spatial autocorrelation Moran migration Canada`
- `kernel relative risk surface case control geography`

Bases utiles depuis l'Université de Sherbrooke : Sport Discus (la base
spécialisée en sciences du sport, accessible via le portail des bibliothèques),
Scopus et Web of Science pour les articles de méthode spatiale.
