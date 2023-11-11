
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

## Auteurs

**Joël Girès** est sociologue et travaille à l’Observatoire de la Santé
et du Social de la Région de Bruxelles-Capitale.

**François Ghesquière** est sociologue et travaille à l’Institut wallon
de l’évaluation, de la prospective et de la statistique.

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

#### Suppression des valeurs manquantes

Dans `prop_group`, garder la possibilité de calculer une proportion sur
tout le groupe (y compris les `NA`) =\> utile lorsque le `NA` a une
signification (exemple : les personnes qui ne peuvent pas avoir
d’arriérés sur leur loyer ou leur emprunt, car ils n’ont pas de loyer ou
d’emprunt).

#### En général

- Ajouter des checks pour les inputs :

  1.  Existence des colonnes dans le dataframe *=\> C’est fait sauf pour
      les variables de design + pondération ? Est-ce testable, du fait
      que j’ai mis l’argument `…` ? Investiguer…* ;

  2.  Pas mettre les mêmes colonnes dans les différents arguments
      (`group`, `var_distrib`, `facet_var`, etc.) ;

  3.  Suffisamment de modalités (pas de facteur à 1 modalité, par ex.) ;

  4.  Le bon type (logical, factor…) et la bonne taille (pas un vecteur
      \> 1).

- Mettre des conditions sur les tests (n min, distribution, variances
  égales…).

- Les cluster / strates / weights n’apparaissent pas dans le message de
  la console avec les replicates =\> pourquoi ?

- Ajouter `n_weight_upp` et `n_weight_low` + harmoniser les noms des
  colonnes entre fonctions (`n` vs `n_tot`, `n_weighted` vs
  `n_tot_weighted`…)

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

### Améliorations

#### En général

- Enlever le paramètre `dodge` ? =\> ne sert à rien.

- Créer une fonction de check des inputs (car redondance entre les 4
  fonctions) *=\> Difficultés du fait de l’usage du tidyverse*

#### central_group

- Pouvoir réordonner avec les facet =\> solution avec `tidytext`
  <https://juliasilge.com/blog/reorder-within/>

#### prop_group

- Pouvoir réordonner avec les facet =\> solution avec `tidytext`
  <https://juliasilge.com/blog/reorder-within/>

#### distrib_group_d

- Ajouter un total ? Il faudrait une autre couleur, sinon pas clair =\>
  comment faire vu qu’il y a la palette de couleur des modalités ?
  Tentative en cours d’utiliser des hachures.

- Réordonner les levels sur la variable `group` ? Mais selon quelle
  valeur (vu qu’il y en a plusieurs) ? Celle du premier level de la
  variable `var_distrib` ?

- Ajouter les effectifs totaux par groupe ? (dans le nom du groupe ?)

- Possibilité d’indiquer un vecteur avec une palette de couleur pour
  coller avec le code couleur de notre institution ?

### Fonctions à créer

#### Univarié

- Densité pour le général (pour variable continue)

- Différents indicateurs + IC par groupe (<u>*PAS OK AVEC
  PIVOT_LONG_SURVEY ?*</u>)

#### Bivarié ou 3 variables+

- prop/moyenne/médiane par 2 groupes

- Tableau croisé avec résidus ou couleur par proportion (proportions par
  c, l, ou total)

- Densité par groupe (pour variable continue)

</div>
