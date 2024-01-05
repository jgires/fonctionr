
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fonctionr <img src="man/figures/fonctionr_logo.png" align="right" width="129"/>

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

`fonctionr` est un package R qui a pour but de produire facilement des
statistiques descriptives complètes à partir de données individuelles
issues de sondages avec un design complexe. Le package inclut des
fonctions permettant de produire les résultats les plus usuels
(comparaison de proportions, de moyennes, de médianes, de
distributions), en tenant compte du plan d’échantillonnage réel grâce au
package `survey`.

`fonctionr` a l’objectif de faciliter l’inférence statistique : outre
des résultats descriptifs, le package produit des intervalles de
confiance et des tests statistiques qui prennent en compte de design
réel de l’enquête. `fonctionr` produit également des graphiques des
résultats à l’aide de `ggplot2`, dans le but de pouvoir intégrer
directement et rapidement les résultats produits dans un rapport/une
publication.

## Installation

Vous pouvez installer le package `fonctionr` depuis
[GitHub](https://github.com/). Pour cela, il vous faut d’abord installer
et charger le package `devtools` :

``` r
# Installer devtools si celui-ci n'est pas installé et charger le package
install.packages("devtools")
library(devtools)

# Installer fonctionr
devtools::install_github("jgires/fonctionr")
```

## Utilisation

Avant toute utilisation, il faut bien entendu charger le package :

``` r
library(fonctionr)
```

L’utilisation de `fonctionr` a pour but d’être simple. Pour nos
exemples, nous utilisons les données de l’enquête SILC pour l’Autriche
contenues dans le package `laeken`. Nous recodons d’abord la variable de
statut économique pour la lisibilité des résultats :

``` r
# Loading of data
data(eusilc, package = "laeken")

# Creation of categories of economic status
eusilc$pl030_rec <- NA
eusilc$pl030_rec[eusilc$pl030 == "1"] <- "Working full time"
eusilc$pl030_rec[eusilc$pl030 == "2"] <- "Working part time"
eusilc$pl030_rec[eusilc$pl030 == "3"] <- "Unemployed"
eusilc$pl030_rec[eusilc$pl030 == "4"] <- "Student"
eusilc$pl030_rec[eusilc$pl030 == "5"] <- "Retired"
eusilc$pl030_rec[eusilc$pl030 == "6"] <- "Permanently disabled"
eusilc$pl030_rec[eusilc$pl030 == "7"] <- "Fulfilling domestic tasks"
```

`fonctionr` permet de calculer facilement des moyennes de revenu par
groupe grâce à la fonction `mean_group()`. Les groupes, ici de statut
économique, sont indiqués dans l’argument `group`. C’est la moyenne du
revenu mensuel qui est calculée, car on peut indiquer soit une variable
quantitative, soit une expression pour calculer celle-ci, directement
dans l’argument `quanti_exp` (dans ce cas la variable `eqIncome` est
divisée par 12 à la volée). Le design de l’enquête SILC est pris en
considération, puisque l’on peut indiquer les clusters (`ids`), les
strates (`strata`) et les poids (`weight`) dans la fonction. Les
intervalles de confiance et un test de différence des moyennes sont
calculés en conséquence.

``` r
eusilc_mean <- mean_group(
  eusilc,
  group = pl030_rec,
  quanti_exp = eqIncome / 12,
  strata = db040,
  ids = db030,
  weight = rb050,
  reorder = T,
  title = "Mean of equivalised income in household by status of individuals",
  subtitle = "Example with austrian SILC data from 'laeken' package",
  )
#> Input: data.frame
#> Sampling design -> ids:  db030, strata:  db040, weights:  rb050
#> Variable(s) détectée(s) dans quanti_exp : eqIncome
#> 0 lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp
```

La fonction produit un tableau de résultats, le résultat d’un test
statistique et un graphique ggplot qui regroupe l’ensemble, prêt à être
inséré dans une publication :

``` r
eusilc_mean$tab
#> # A tibble: 8 × 8
#>   pl030_rec            mean mean_low mean_upp n_sample n_weighted n_weighted_low
#>   <fct>               <dbl>    <dbl>    <dbl>    <int>      <dbl>          <dbl>
#> 1 Fulfilling domesti… 1296.    1253.    1338.     1207    640311.        605978.
#> 2 Permanently disabl… 1330.    1202.    1458.      178    104930.         85796.
#> 3 Retired             1720.    1681.    1758.     3146   1806954.       1746273.
#> 4 Student             1355.    1291.    1419.      736    395829.        365532.
#> 5 Unemployed          1456.    1380.    1532.      518    303252.        276953.
#> 6 Working full time   1895.    1864.    1926.     5162   2869868.       2797833.
#> 7 Working part time   1591.    1542.    1639.     1160    636121.        600709.
#> 8 Total               1703.    1679.    1726.    12107   6757264.       6683738.
#> # ℹ 1 more variable: n_weighted_upp <dbl>
eusilc_mean$test.stat
#> Wald test for pl030_rec
#>  in svyglm(formula = fmla, design = data_W)
#> F =  141.5453  on  6  and  5985  df: p= < 2.22e-16
eusilc_mean$graph
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

`fonctionr` comprend plusieurs fonctions pour réaliser des opérations
similaires, par exemple pour calculer des proportions par groupe avec la
fonction `prop_group()`. On peut à nouveau indiquer directement une
expression à partir de laquelle calculer les proportions dans l’argument
`prop_exp` : dans ce cas la proportion à recevoir des revenus du chômage
(= supérieurs à 0).

``` r
eusilc_prop <- prop_group(
  eusilc,
  group = pl030_rec,
  prop_exp = py090n > 0,
  strata = db040,
  ids = db030,
  weight = rb050,
  reorder = T,
  title = "Proportion of individuals receiving income from unemployment in their household",
  subtitle = "Example with austrian SILC data from 'laeken' package"
)
#> Input: data.frame
#> Sampling design -> ids:  db030, strata:  db040, weights:  rb050
#> Variable(s) détectée(s) dans l'expression : py090n
#> 0 lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) de l'expression
#> Processing time: 0.47 sec
eusilc_prop$tab
#> # A tibble: 8 × 11
#>   pl030_rec                   prop prop_low prop_upp n_sample n_true_weighted
#>   <fct>                      <dbl>    <dbl>    <dbl>    <int>           <dbl>
#> 1 Fulfilling domestic tasks 0.0485   0.0369   0.0624     1207          31048.
#> 2 Permanently disabled      0.209    0.148    0.282       178          21975.
#> 3 Retired                   0.0177   0.0134   0.0229     3146          31988.
#> 4 Student                   0.0194   0.0106   0.0323      736           7666.
#> 5 Unemployed                0.732    0.690    0.770       518         221878.
#> 6 Working full time         0.0818   0.0741   0.0899     5162         234629.
#> 7 Working part time         0.110    0.0921   0.130      1160          69869.
#> 8 Total                     0.0916   0.0863   0.0971    12107         619054.
#> # ℹ 5 more variables: n_true_weighted_low <dbl>, n_true_weighted_upp <dbl>,
#> #   n_tot_weighted <dbl>, n_tot_weighted_low <dbl>, n_tot_weighted_upp <dbl>
eusilc_prop$test.stat
#> 
#>  Pearson's X^2: Rao & Scott adjustment
#> 
#> data:  NextMethod()
#> F = 475.2, ndf = 5.9608, ddf = 35711.0201, p-value < 2.2e-16
eusilc_prop$graph
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## Auteurs

**Joël Girès** est sociologue et travaille à l’Observatoire de la Santé
et du Social de la Région de Bruxelles-Capitale.

**François Ghesquière** est sociologue et travaille à l’Institut wallon
de l’évaluation, de la prospective et de la statistique.

Nous sommes ouverts à toute remarque afin d’améliorer notre package.

[<img src="man/figures/logo_observatoire_sante_social.png" align="center" height="120/"/>](https://www.ccc-ggc.brussels/fr/observatbru/accueil)[<img src="man/figures/logo_iweps.png" align="center" height="120/"/>](https://www.iweps.be)

<div style="border:1px; background-color:#f8f5e4; padding: 5px 10px 10px 10px; margin-bottom: 10px">

## To do list

### Prioritaire (nécessités ou bugs)

#### Filtrage

Il semble qu’il faille filtrer <u>après</u> la déclaration du design. Si
on ne le fait pas, on considère le design sur l’objet filtré (avec moins
de PSU / strates qu’il y en a en réalité), ce qui sous-estime
potentiellement la distribution d’échantillonnage. Voir :

- <https://stats.stackexchange.com/questions/411026/why-it-is-important-to-make-survey-design-object-svydesign-function-in-r-with-i>

- <https://notstatschat.rbind.io/2021/07/22/subsets-and-subpopulations-in-survey-inference/>

  *=\> De ce fait, j’ai inclus une option de filtre (`filter_exp`) dans
  les fonctions (qui filtre après la déclaration du design), qui évite
  de filtrer l’objet avant en dégradant le design. =\> Vérifier que
  c’est bien OK ! **A FAIRE : expliquer dans la doc !***

#### En général

- Joël : j’ai modifié la logique d’affichage des axes dans
  `distrib_group_discrete()` & `distrib_discrete()`. Je voulais que l’on
  puisse supprimer individuellement l’axe X, Y ou la légende. J’ai donc
  ajouté une condition selon laquelle quand `show_labs = TRUE` (défaut
  de la fonction) & que le titre de l’axe est défini comme `""` (=
  charactère vide), alors le titre de l’axe est supprimé (`NULL` dans
  ggplot). J’ai fait ça car on ne peut pas mettre `NULL` directement :
  c’est le défaut de la fonction, et dans ce cas c’est le nom de la
  variable qui est affiché. ***A voir si c’est OK pour généraliser !***

- Documenter tous les arguments des différentes fonctions + vérifier que
  les explications sont bien correctes (quelques erreurs repérées,
  notamment du fait de copier-coller).

- Ordonner les arguments de la même manière pour chaque fonction (voir
  fichier excel) =\> après cela, faire pareil pour l’ordre des arguments
  dans les fonctions de check !

- Ajouter la possibilité de changer le point en virgule pour la décimale
  dans les étiquettes.  
  *=\> Fait dans `prop_group()` =\> **décider : choix avec argument
  (decimal ?) Ensuite, généraliser.***

- Ajouter des exemples pour chaque fonction (pour le site).

- Vérifier que les scripts sont bien commentés pour qu’on se rappelle ce
  qu’on a fait (il manque des notes).

- Vérifier que `reorder = T` réordonne toujours de la même façon pour
  les différentes fonctions (`many_prop_group()` =/= `prop_group()`).

- Pour les fonctions qui calculent des proportions : ajouter la
  possibilité d’un `na.rm.prop = FALSE` pour que la proportion soit
  calculée sur l’ensemble des individus (les `NA` étant comptabilisés
  dans le dénominateur).

- Changer la fonction `scales::pvalue`, qui n’est plus valide
  (superseded) =\> Faire une fonction maison en interne.

- Ajouter des checks pour les inputs :

  1.  Existence des colonnes dans le dataframe *=\> C’est fait sauf pour
      les variables de design + pondération ? Est-ce testable, du fait
      que j’ai mis l’argument `…` ? Investiguer…* ;  
      **=\> Peut-être trouvé la solution ici ? =\>
      <https://stackoverflow.com/questions/70652685/how-to-set-aliases-for-function-arguments-in-an-r-package>**

  2.  Le bon type (logical, factor…) et la bonne taille (pas un vecteur
      \> 1).  
      *=\> C’est fait. Améliorer les messages ? (par ex. : indiquer
      l’argument) Voir :
      <https://stackoverflow.com/questions/77432872/how-paste-be-used-as-a-message-with-r-stopifnot>*

  3.  Suffisamment de modalités (pas de facteur à 1 modalité, par ex.) ;

  4.  Pas mettre les mêmes colonnes dans les différents arguments
      (`group`, `var_distrib`, `facet_var`, etc.) ;

- Mettre des conditions pour réaliser les tests (n min, distribution,
  variances égales…).

- Les cluster / strates / weights n’apparaissent pas dans le message de
  la console avec les replicates =\> pourquoi ?

- Ajouter `n_weight_upp` et `n_weight_low` + harmoniser les noms des
  colonnes entre fonctions (`n` vs `n_tot`, `n_weighted` vs
  `n_tot_weighted`…).  
  *=\> François : je propose l’ordre suivant : les variables de
  ventilation, l’indicateur, l’indicateur_low, l’indicateur_up,
  n_sample, n_true_weighted, n_true_weighted_low, n_true_weighted_upp,
  n_tot_weighted, n_tot_weighted_low,
  n_tot_weighted_upp.*<!--# Joël : OK donc c'est fait ? -->

#### central_group

- Bypasser l’erreur du test stat avec `tryCatch()`.

#### prop_group

- Bypasser l’erreur du test stat avec `tryCatch()`.

#### distrib_group_d

- Implémenter un test stat lorsqu’il y a des facets =\> via modélisation
  loglinéaire, mais j’ai un peu de mal à comprendre les erreurs de
  `survey` (erreurs fréquentes).

#### distrib_d

- Ajouter un test khi2 d’adéquation =\> **Pour rappel, l’import de
  `survey` dans les dépendances ne sert à rien tant que le test n’est
  pas implémenté !**

#### many_prop

- Documenter les arguments !

- Ajouter l’export excel.

#### many_val_group

- Ajouter l’export excel.
- Ajouter un check pour la proportion = seules des valeurs `0` - `1` ou
  `FALSE` - `TRUE`.

**esth_graph**

- Changer le nom.

- Régler le pb si multiples `NA` et voir si un pb se pose avec multiples
  totaux.

### Améliorations

#### En général

- Voir si on peut créer une fonction commune à toutes les fonctions du
  package pour créer le ggplot =\> ce serait une large simplification.
  Pour l’instant, il y a déjà un thème commun `theme_fonctionr`.
- Créer une fonction de check des inputs indispensables (car redondance
  entre les 4 fonctions)  
  *=\> Pour l’instant c’est fait “en dur” : difficultés de créer une
  fonction du fait de l’usage du tidyverse : il faut sans doute utiliser
  les fonctions de `rlang`.*

#### central_group

- Pouvoir réordonner avec les facet =\> solution avec `tidytext`
  <https://juliasilge.com/blog/reorder-within/>

#### prop_group

- Pouvoir réordonner avec les facet =\> solution avec `tidytext`
  <https://juliasilge.com/blog/reorder-within/>

#### distrib_group_d

- Ajouter les n par “cellule” ?

- Ajouter les effectifs totaux par groupe ? (dans le nom du groupe ?)

- Ajouter un total ? Il faudrait une autre couleur, sinon pas clair =\>
  comment faire vu qu’il y a la palette de couleur des modalités ?  
  *=\> Tentative en cours d’utiliser des hachures.*

- Réordonner les levels sur la variable `group` ? Mais selon quelle
  valeur (vu qu’il y en a plusieurs) ? Celle du premier level de la
  variable `var_distrib` ?

- Possibilité d’indiquer un vecteur avec une palette de couleur pour
  coller avec le code couleur de notre institution ?

### Fonctions à créer

#### Univarié

- Densité pour le général (pour variable continue)

#### Bivarié ou 3 variables+

- prop/moyenne/médiane par 2 groupes

- Tableau croisé avec résidus ou couleur par proportion (proportions par
  c, l, ou total)

- Densité par groupe (pour variable continue)

</div>
