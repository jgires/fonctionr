# Changelog

## fonctionr 0.4.0

Date : 2026-01-27

- Ajout de la possibilité de définir des options globales grâce à la
  fonction
  [`fonctionr_options()`](https://jgires.github.io/fonctionr/reference/fonctionr_options.md).

- Nouveaux arguments pour les palettes et les couleurs.

  - Séparation de la couleur unie et de la palette dans les arguments
    `col` (nouveau) et `pal`. Cette modification engendre une différence
    de comportement par rapport aux versions antérieures de `fonctionr`
    : la couleur unie définie dans `pal` ne fonctionnera plus et la
    couleur par défaut sera appliquée.

  - Dans
    [`distrib_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_continuous.md)
    et
    [`distrib_group_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_group_continuous.md),
    il y a plusieurs changements dans les arguments : `pal` devient
    `col_density` ; `pal_moustache` devient `col_moustache` ; `color`
    devient `col_border`. La raison de ce changement est lié à un
    impératif de cohérence entre les fonctions : l’argument `pal` est
    désormais réservé aux palettes de type qualitatif. Néanmoins pour
    des raisons de compatibilité, les anciens arguments demeurent dans
    la fonction, mais sont inactifs.

- Introduction de l’argument `coef_font` pour multiplier la taille de
  toutes les polices de caractère dans les graphiques de toutes les
  fonctions.

- Un grand nombre de caractères spéciaux sont désormais utilisables dans
  le noms des groupes (du fait que `ggtext` a besoin d’un formatage en
  codes HTML, cela provoquait auparavant des bugs dans les graphiques).

- Les palettes de toutes les fonctions ont désormais la même direction
  (cohérence entre toutes les fonctions).

- Le thème par défaut de
  [`theme_fonctionr()`](https://jgires.github.io/fonctionr/reference/theme_fonctionr.md)
  devient `"fonctionr"`. La valeur `NULL` pour le theme crée désormais
  un design très proche du thème par défaut de ggplot (mais en gérant la
  taille de la police et le formatage permis par ggtext).

- Mise à jour des polices de caractère contenues dans `fonctionr` : la
  police Montserrat inclue dans le package peut désormais être formatée
  en italique et gras ; les polices ‘Amatic’ et ‘Helvetica Neue’ sont
  supprimées (du fait de la limite de 5Mo de CRAN).

## fonctionr 0.3.22

Date : 2026-01-10

- Simplification du code de
  [`esth_graph()`](https://jgires.github.io/fonctionr/reference/esth_graph.md),
  [`many_val_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md),
  [`distrib_group_discrete()`](https://jgires.github.io/fonctionr/reference/distrib_group_discrete.md),
  [`central_group()`](https://jgires.github.io/fonctionr/reference/central_group.md)
  pour l’utilisation de `ggtext`.
- Simplification du code de création des palettes avec la fonction
  interne `create_palette()`.

## fonctionr 0.3.21

Date : 2026-01-05

- Simplification du code de
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  pour formater le label du total en gras avec le package `ggtext`
  (inspiration :
  <https://github.com/wilkelab/ggtext/issues/121#issuecomment-3637732111>).
  Mise en italique du groupe *NA* par la même occasion. La fonction
  interne `relab_ggtext()` a été créée pour opérer cette simplification.
  Les autres fonctions doivent encore être modifiées dans le même sens.

## fonctionr 0.3.20

Date : 2025-12-26

- Correction temporaire d’un bug graphique provoqué par le passage à
  `ggplot 4.0`, qui impliquait que le **total** sur les graphiques (en
  gras) soit indiqué \*\* total \*\* (avec des astérisques). C’est le
  package `ggtext` qui est utilisé pour mettre en gras le total dans les
  graphiques. Or, `ggtext` n’est pas complètement compatible avec le
  nouveau fonctionnement de `ggplot 4.0` : l’héritage des propriétés ne
  se fait pas entre éléments S3 et S7 (voir : [ggtext and parsing html
  with latest version of ggplot · Issue \#6752 ·
  tidyverse/ggplot2](https://github.com/tidyverse/ggplot2/issues/6752)).
  Une solution temporaire a été ajoutée (à corriger lorsque `ggtext`
  sera mis à jour : [Update theme elements to S7 class system · Issue
  \#128 ·
  wilkelab/ggtext](https://github.com/wilkelab/ggtext/issues/128)).

## fonctionr 0.3.19

Date : 2025-11-03

- Ajout de nouvelles palettes dérivées des palettes continues : ce sont
  les mêmes avec plus de contraste, ce qui est plus adapté pour réaliser
  des cartes.

## fonctionr 0.3.18

Date : 2025-10-13

- Correction de l’ordre des couleurs pour la légende avec le
  `group.fill` activé pour
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  et
  [`central_group()`](https://jgires.github.io/fonctionr/reference/central_group.md).

## fonctionr 0.3.17

Date : 2025-06-28

- Ajout de la fonction
  [`relab_cut()`](https://jgires.github.io/fonctionr/reference/relab_cut.md).

## fonctionr 0.3.16

Date : 2025-05-17

- Le signe des décimales change désormais en fonction de la langue. La
  valeur de l’argument `lang` devient `NULL` par défaut pour les
  fonctions qui ont un argument `lang`.

## fonctionr 0.3.15

Date : 2025-04-29

- Ajout de la position `flip` dans
  [`many_val_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md)
  pour intervertir les groupes et les indicateurs choisis sur le
  graphique.

## fonctionr 0.3.14

Date : 2025-03-27

- La fonction
  [`distrib_group_discrete()`](https://jgires.github.io/fonctionr/reference/distrib_group_discrete.md)
  possède désormais un argument `reorder` (`TRUE` pour réordonner les
  groupes selon le premier level de la variable `quali_var`) et un
  argument `show_n` (`TRUE` pour afficher les effectifs de l’échantillon
  en dessous du pourcentage).

## fonctionr 0.3.13

Date : 2025-03-17

- La fonction
  [`make_surface()`](https://jgires.github.io/fonctionr/reference/make_surface.md)
  dispose de l’argument `linewidth_ci` pour régler l’épaisseur des IC.

## fonctionr 0.3.12

Date : 2025-02-16

- La fonction graphique
  [`theme_fonctionr()`](https://jgires.github.io/fonctionr/reference/theme_fonctionr.md)
  dispose maintenant de l’argument `grid.lines`, pour spécifier les
  lignes de la grilles à afficher (`"x"`, `"y"` ou `"both"`).

## fonctionr 0.3.11

Date : 2025-02-11

- Modification des fonctions qui calculent des totaux, pour afficher
  **en gras** le label ainsi que les indicateurs du total. Pour ce
  faire, le package `ggtext` est désormais utilisé ; il y a donc une
  dépendance en plus.
- La fonction
  [`distrib_group_discrete()`](https://jgires.github.io/fonctionr/reference/distrib_group_discrete.md)
  affiche désormais l’unité `%` sur l’axe des x par défaut, c’est-à-dire
  lorsque l’unité est définie comme un espace vide (`""`) et que
  l’argument `scale` vaut `100`.

## fonctionr 0.3.10

Date : 2024-12-17

- Ajout de palettes spécifiques pour l’Observatoire de la Santé et du
  Social.

- Export de la fonction
  [`official_pal()`](https://jgires.github.io/fonctionr/reference/official_pal.md).

- Ajout de la possibilité de désaturer, éclaircir ou foncer les palettes
  de couleur.

## fonctionr 0.3.9

Date : 2024-09-15

- Ajout de l’argument `lang` qui permet l’affichage des indications sur
  les graphiques en trois langues : français, néérlandais, anglais.

- Ajout d’exemples dans la documentation des fonctions.

## fonctionr 0.3.8

Date : 2024-09-04

- Ajout du calcul de totaux pour
  [`central_group()`](https://jgires.github.io/fonctionr/reference/central_group.md)
  et
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  avec les `group.fill` activés.

- Ajout du calcul de totaux pour
  [`distrib_group_d()`](https://jgires.github.io/fonctionr/reference/distrib_group_discrete.md)
  et
  [`many_val_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md).

- Ajout d’un `theme` “IWEPS” pour les graphiques.

- L’argument `show_lab` devient `show_labs` (avec un s) pour toutes les
  fonctions.

- L’argument de couleur `fill` devient `pal` pour toutes les fonctions
  (par cohérence).

## fonctionr 0.3.7

Date : 2024-08-04

- Modifications substancielles pour
  [`central_group()`](https://jgires.github.io/fonctionr/reference/central_group.md)
  : ajout de la possibilité de faire des sous-groupes (argument
  `group.fill`) par groupe.

- Ajout de la possibilité dans
  [`central_group()`](https://jgires.github.io/fonctionr/reference/central_group.md)
  de ne pas calculer le total.

## fonctionr 0.3.6

Date : 2024-07-20

- Ajout de la possibilité dans
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  de ne pas calculer le total.

- Dans
  [`make_surface()`](https://jgires.github.io/fonctionr/reference/make_surface.md),
  l’option `position = "bottom"` change également la position de la
  surface minimale affichée lorsque `compare = T` (suggestion de Robin).

- Optimisation du code (très légère augmentation de la vitesse).

## fonctionr 0.3.5

Date : 2024-07-07

- Modifications substancielles pour
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  : ajout de la possibilité de faire des sous-groupes (argument
  `group.fill`) par groupe.

- La fonction
  [`make_surface()`](https://jgires.github.io/fonctionr/reference/make_surface.md)
  est terminée : documentation et checks.

- Modification du code relatif aux palettes de couleurs pour
  compatibilité avec la nouvelle version de `PrettyCols` (changement de
  nom d’un argument).

- Corrections mineures à divers endroits du code. Le check n’indique
  désormais plus d’erreur, warning ou note.

## fonctionr 0.3.4

Date : 2024-06-29

- Ajout de l’export excel pour
  [`distrib_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_continuous.md)
  et
  [`distrib_group_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_group_continuous.md).
  De légères modifications des outputs de ces deux fonctions ont été
  apportées pour rendre l’export propre.

- Ajout de trois polices d’écriture dans `fonctionr` : `Helvetica Neue`,
  `League Gothic` et `Amatic`.

## fonctionr 0.3.3

Date : 2024-06-06

- Amélioration de l’affichage des aires de densité dans
  [`distrib_group_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_group_continuous.md)
  lorsque la palette est d’une seule couleur (on ne voit plus les
  limites des quantiles).
- Ajout des facets dans
  [`make_surface()`](https://jgires.github.io/fonctionr/reference/make_surface.md).
- Mise à la ligne automatique de caption tous les 100 caractères dans
  toutes les fonctions.

## fonctionr 0.3.2

Date : 2024-05-27

- Correction d’un bug sur le reorder des valeurs dans
  [`distrib_group_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_group_continuous.md).

## fonctionr 0.3.1

Date : 2024-05-25

- Correction d’un bug dans la plupart des fonctions sur le reorder des
  valeurs par la médiane des résultats utilisé pour les facets (la
  fonction est désormais écrite avec des guillemets : `FUN = "median"`.
  Sans cela, il y avait dans certains cas particuliers une erreur).

## fonctionr 0.3.0

Date : 2024-04-26

- La documentation a été complétée pour que le package puisse être
  utilisé par des personnes tierces. Le package commence à être
  diffusable.

- Le bug avec la ligne du geom_line qui est coupée pour
  [`distrib_group_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_group_continuous.md)
  est réglé.

- bug d’ordonnancement des facteurs réglé dans
  [`esth_graph()`](https://jgires.github.io/fonctionr/reference/esth_graph.md).

## fonctionr 0.2.13

Date : 2024-04-20

- Création de
  [`make_surface()`](https://jgires.github.io/fonctionr/reference/make_surface.md)
  comme fonction indépendante utilisable sur n’importe quel tableau de
  résultats.

- Ajout de la possibilité d’afficher le résultat d’un test statistique
  dans
  [`esth_graph()`](https://jgires.github.io/fonctionr/reference/esth_graph.md).

## fonctionr 0.2.12

Date : 2024-04-04

- Création de
  [`distrib_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_continuous.md)
  et
  [`distrib_group_continuous()`](https://jgires.github.io/fonctionr/reference/distrib_group_continuous.md).

- Modification du thème
  [`theme_fonctionr()`](https://jgires.github.io/fonctionr/reference/theme_fonctionr.md)
  pour qu’il soit plus adapté aux nouvelles fonctions.

## fonctionr 0.2.11

Date : 2024-03-17

- Réécriture du code de toutes les fonctions (sauf
  [`esth_graph()`](https://jgires.github.io/fonctionr/reference/esth_graph.md)
  et
  [`pivot_longer_survey()`](https://jgires.github.io/fonctionr/reference/pivot_longer_survey.md))
  : simplification + structuration.
- Nouvelle logique des na.rm finalisée pour toutes les fonctions
  (différenciation de `na.rm.group` et `na.rm.facet`, introduction de
  `na.rm.var` pour
  [`distrib_group_d()`](https://jgires.github.io/fonctionr/reference/distrib_group_discrete.md)
  et
  [`distrib_d()`](https://jgires.github.io/fonctionr/reference/distrib_discrete.md)).
- Ajout de checks pour vérifier si la couleur/palette est valide :
  couleur/palette par défaut si la couleur introduite est invalide.
- Création d’une fonction interne de création de palettes, permettant de
  produire les palettes de différentes institutions (pour l’instant
  Vivalis et Perspective, pour tester).

## fonctionr 0.2.10

Date : 2024-03-15

- Ajout des argument `na.rm.facet` dans
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  et
  [`central_group()`](https://jgires.github.io/fonctionr/reference/central_group.md).
- Possibilité de mettre une couleur unie pour
  [`many_val()`](https://jgires.github.io/fonctionr/reference/many_val.md).
- Création de l’argument `na.vars` pour
  [`many_val()`](https://jgires.github.io/fonctionr/reference/many_val.md)
  et
  [`many_val_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md).

## fonctionr 0.2.9

Date : 2024-03-05

- Ajout des palettes dans
  [`many_val()`](https://jgires.github.io/fonctionr/reference/many_val.md).

## fonctionr 0.2.8

Date : 2024-03-03

- Modification des checks pour voir si les variables indispensables
  existent bien dans `data` : simplification du code et message plus
  clair et utile pour l’utilisateur.

## fonctionr 0.2.7

Date : 2024-03-01

- Ajout de
  [`many_val()`](https://jgires.github.io/fonctionr/reference/many_val.md)
  comme fonction plus générale qui englobe
  [`many_prop()`](https://jgires.github.io/fonctionr/reference/many_val.md).
  Il y a désormais la possibilité de calculer des moyennes et des
  médianes avec les alias
  [`many_mean()`](https://jgires.github.io/fonctionr/reference/many_val.md)
  et
  [`many_median()`](https://jgires.github.io/fonctionr/reference/many_val.md).
- Plus de parcimonie pour l’import des packages extérieurs dans le
  namespace.
- Changement de nom des arguments : `na.var` devient `na.prop` et
  `facet.var` devient `facet`.
- Début de simplification du code dans un but de facilité pour la
  maintenance et les modifs futures.

## fonctionr 0.2.6

Date : 2024-02-22

- Ajout d’un check de l’expression dans
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md),
  évitant de produire des résultats incohérents.

## fonctionr 0.2.5

Date : 2024-02-19

- Implémentation du `trycatch()` sur le test statistique dans
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  : le test statistique est désormais bypassé si les conditions ne sont
  pas remplies et ne stoppe donc plus
  [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  en cas d’erreur.

## fonctionr 0.2.4

Date : 2024-02-17

- Implémentation de checks pour vérifier que les variables de
  [`many_prop_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md)
  soient bien binaires, et pour éviter les variables à une modalité pour
  `quali_var` dans
  [`distrib_group_discrete()`](https://jgires.github.io/fonctionr/reference/distrib_group_discrete.md).
- Correction d’un bug dans
  [`esth_graph()`](https://jgires.github.io/fonctionr/reference/esth_graph.md).

## fonctionr 0.2.3

Date : 2024-02-17

- Implémentation de l’export excel pour
  [`many_prop()`](https://jgires.github.io/fonctionr/reference/many_val.md)
  et
  [`many_val_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md).

## fonctionr 0.2.2

Date : 2024-02-14

- Implémentation du test statistique pour
  [`distrib_discrete()`](https://jgires.github.io/fonctionr/reference/distrib_discrete.md).

- Correction d’un bug découvert à cette occasion, qui empêchait de faire
  fonctionner
  [`distrib_discrete()`](https://jgires.github.io/fonctionr/reference/distrib_discrete.md) avec
  `na.rm.group = FALSE`.

## fonctionr 0.2.1

Date : 2024-02-14

- Amélioration des fonctions internes de vérification de la validité des
  arguments. Celles-ci renvoient désormais un message plus clair pour
  l’utilisateur.

## fonctionr 0.2.0

- [`prop_group()`](https://jgires.github.io/fonctionr/reference/prop_group.md)
  : ajout de la possibilité de calculer des proportions en comptant les
  `NA` au dénominateur avec l’argument `na.var = "include"`. Dans ce
  cas, l’expression `prop_exp` peut contenir la fonction
  [`is.na()`](https://rdrr.io/r/base/NA.html) pour calculer la
  proportion de valeurs manquantes.

- [`many_val_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md)
  : changement de l’ordre des labels pour que l’ordre des couleurs
  corresponde à l’ordre sur le graphique.

- Ajout d’exemples dans la documentation à partir des données SILC
  inclues dans le package laeken.

- Correction de bugs mineurs.

## To do list

### Prioritaire (nécessités ou bugs)

#### En général

- **(Joël) pour les test stat de prop_group et central_group, modifier
  le caption quand il y a des facets pour indiquer que le test se fait
  bien sur le total et ne pas faire le test quand il y a des facets et
  que total = FALSE.**

- **(Joël) Ajouter que pour supprimer xlab ou ylab, on puisse entrer
  `NA` et pas seulement ““.**

- **(François) Ajouter toutes les options dans
  [`fonctionr_options()`](https://jgires.github.io/fonctionr/reference/fonctionr_options.md),
  sauf les arguments avec données ou variables.**

- **(François) Revoir la doc de chaque fonction et le manuel par rapport
  aux changements. Voir ici pour savoir les différentes options
  possibles dans la doc : [https://r-pkgs.org/man.html](#id_0).**

- **(François) Vérifier les messages à l’utilisateur : voir quoi garder,
  corriger, traduire, indiquer les accents (é, è…), ajouter des
  guillemets pour les arguments dans les checks, mieux écrire les
  résultats des tests stat (on ne sait pas quelle est l’hypothèse
  nulle).  
  *=\> Traduire ces messages selon la langue ? Ou tout en anglais ?***

- **(François) Créer un tableau joli en output (avec `flextable`).**

- BUG : si une variable de design == le nom d’un objet externe, ça
  fonctionne =\> APPROFONDIR ET REGLER CA ?

- Rendre l’usage non interactif (= la programmation via d’autres
  fonctions) possible =\> gros travail, usage de `rlang` à la place de
  [`substitute()`](https://rdrr.io/r/base/substitute.html).

- Passer le code de chaque fonction en revue pour cleaner / harmoniser /
  simplifier.  
  *=\> En cours : il faut encore checker le code du graphique ggplot.*

- Mettre des conditions pour réaliser les tests (n min, distribution,
  variances égales…).

- Ajouter des checks pour les inputs :

  - Pas mettre les mêmes colonnes dans les différents arguments ?
    (`group`, `var_distrib`, `facet_var`, etc.).

#### **distrib_c**

- Ajouter le test stat univarié avec comme H0 mu dans la population.
  Apparemment la fonction n’est pas pré-programmée dans `survey`, il
  faut la faire soi-même.
- Ajouter la possibilité de facets.

#### **distrib_group_c**

- **(Joël) Introduire la mise en forme avec ggtext =\> plus difficile
  ici car le `NA` a été tranformé en level. Réfléchir à la meilleure
  solution et revoir éventuellement la fonction.**

- **(François + Joël) Ajouter hauteur des densités proportionnelle aux
  effectifs pondérés.**

- **(Joël) La moustache peut ne prendre qu’une couleur =\> pas
  cohérent  
  =\> Ajouter un check**

- Il y a un warning de la fonction
  [`density()`](https://rdrr.io/r/stats/density.html) qui dit que
  `Selecting bandwidth not using 'weights'`. La doc de density() dit :
  “automatic bandwidth selection will not take the weights into account
  and hence may be suboptimal.”  
  =\> Voir si c’est un problème ?

- Le groupe `NA` est transfomé en level du facteur de groupe. De ce
  fait, il n’apparaît pas toujours en dernier, notamment lorsque
  `reorder = T`. Régler ça (pas cohérent avec les autres fonctions).

- Faire en sorte que le nom du groupe soit toujours le même dans les
  sorties de la liste (pour le moment, parfois “group”, parfois le nom
  de la variable) =\> implique de changer l’argument `by =` des
  `left_join()` dans le script, et donc le nommage des colonnes.

- Ajouter la possibilité de facets.

- La colonne `central` dans l’objet `dens` produit n’a pas de valeur
  `y_ridges`.

- Un peu cleaner le script : il y a des étapes inutiles qui pourraient
  être simplifiées, notamment dans la création des valeurs centrales.

- Ajouter la possibilité d’un total.

#### prop_group

#### central_group

- Bypasser l’erreur du test stat avec
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html).

#### distrib_group_d

- **(François) Ajouter hauteur des barres proportionnelle aux effectifs
  pondérés.**
- Implémenter un test stat lorsqu’il y a des facets =\> via modélisation
  loglinéaire, mais j’ai un peu de mal à comprendre les erreurs de
  `survey` (erreurs fréquentes).
- Redondance du code avec total pour le `geom_text()` ?

#### many_val

- **(Joël) Conditions ajoutées pour définir la priorité de pal sur col
  =\> Voir avec François si c’est OK.  
  =\> C’est idem pour
  [`make_surface()`](https://jgires.github.io/fonctionr/reference/make_surface.md).**
- Les couleurs des palettes sont attachées à l’indicateur, mais pas à la
  position. De ce fait, lorsque `reorder = T`, l’ordre des couleurs de
  la palette n’est pas respecté =\> changer le comportement ?

#### many_val_group

- Le `NA` apparait en premier lorsque `position = "flip"` =\> Corriger,
  il doit apparaître en dernier.
- Améliorer l’alignement des effectifs avec `show_n = T` lorsque
  `position == "stack"`
- Redondance du code avec total pour le `geom_text()` ?
- Les couleurs des palettes sont attachées au groupe, mais pas à la
  position. De ce fait, lorsque `reorder = T`, l’ordre des couleurs de
  la palette n’est pas respecté =\> changer le comportement ?
- Fusionner
  [`many_val_group()`](https://jgires.github.io/fonctionr/reference/many_val_group.md)
  et
  [`many_val()`](https://jgires.github.io/fonctionr/reference/many_val.md)
  !

#### make_surface

- Ajouter la détection automatique des IC (possible si les colonnes sont
  du même nom que la valeur, mais terminant par `_low` et `_upp`).

- Ajouter les checks (penser le cas spécifique de `pvalue`).

- Lorsque facet, mieux penser l’alignement entre mêmes modalités ?

- Ajouter les arguments `dec` et éventuellement `scales`.

#### **esth_graph**

- Changer le nom.

- Ajouter le check pour l’argument `pvalue`.

- Régler le pb si multiples `NA` + voir si un pb se pose avec multiples
  totaux.  
  =\> BUG : la condition stoppe si multiples `NAs` avec facettes même si
  1 seul `NA` par facette (condition trop stricte =\> ça doit passer)

### Améliorations

#### En général

- Réécrire le code du test pour avoir la formule originale =\> possible
  avec [`eval()`](https://rdrr.io/r/base/eval.html), voir code de
  [`central_group()`](https://jgires.github.io/fonctionr/reference/central_group.md).
- Voir si on peut créer une fonction commune à toutes les fonctions du
  package pour créer le ggplot =\> ce serait une large simplification.
  Pour l’instant, il y a déjà un thème commun `theme_fonctionr`.
- Créer une fonction de check des inputs indispensables (car redondance
  entre les 4 fonctions)  
  *=\> Pour l’instant c’est fait “en dur” : difficultés de créer une
  fonction du fait de l’usage du tidyverse : il faut sans doute utiliser
  les fonctions de `rlang`.*
- L’import des packages peut être optimisé : il n’est pas utile
  d’importer des packages extérieurs *entiers* dans le NAMESPACE, cela
  augmente le risque de collisions de noms de fonctions =\> un bon
  résumé ici :
  [https://mdneuzerling.com/post/what-ive-learnt-about-making-an-r-package/](#id_0)
  et pour plus de détails :
  [https://r-pkgs.org/dependencies-in-practice.html](#id_0).  
  *=\> A faire : importer seulement les fonctions utiles (mutate,
  select, etc.).*
- Revoir la solution apportée dans
  [`theme_fonctionr()`](https://jgires.github.io/fonctionr/reference/theme_fonctionr.md)
  au bug de compatibilité entre `ggtext` et `ggplot 4.0` lorsque ggtext
  aura été mis à jour. Voir :
  <https://github.com/jgires/fonctionr/commit/0461f452405628d1aaf692a0266f3a281c2b67d6>.

#### distrib_group_d

- Ajouter les effectifs totaux par groupe ? (dans le nom du groupe ?)

### Notes

#### Filtrage

Il semble qu’il faille filtrer après la déclaration du design. Si on ne
le fait pas, on considère le design sur l’objet filtré (avec moins de
PSU / strates qu’il y en a en réalité), ce qui sous-estime
potentiellement la distribution d’échantillonnage. Voir :

<https://stats.stackexchange.com/questions/411026/why-it-is-important-to-make-survey-design-object-svydesign-function-in-r-with-i>

<https://notstatschat.rbind.io/2021/07/22/subsets-and-subpopulations-in-survey-inference/>

*=\> De ce fait, j’ai inclus une option de filtre (filter_exp) dans les
fonctions (qui filtre après la déclaration du design), qui évite de
filtrer l’objet avant en dégradant le design. =\> Vérifier que c’est
bien OK ! *A FAIRE : expliquer dans la doc !**

### Fonctions à créer

#### Bivarié ou 3 variables+

- Superposition de 2 statistiques (bar + lines) avec échelles
  différentes.

- Tableau croisé avec résidus ou couleur par proportion (proportions par
  c, l, ou total)
