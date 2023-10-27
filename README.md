
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fonctionr

<!-- badges: start -->
<!-- badges: end -->

Description à venir !

# Améliorations

## En général

- Ajouter la possibilité de créer des replicates sur base d’un
  data.frame

- Créer une fonction pour transformer les données en objet srvyr
  (redondance entre les 3 fonctions)

- Ajouter des checks pour les inputs

- Mettre des conditions sur les tests (n min, distribution, variances
  égales…)

## central_group

- Avec la nouvelle manière de faire des tests
  (as.character(substitute(x)) pour chaque variable) =\> plus
  d’opérations possibles sur la variable quanti pour la moyenne/médiane
  (division à la volée par ex.)
- Pouvoir réordonner avec les facet =\> solution avec tidytext
  <https://juliasilge.com/blog/reorder-within/>

## prop_group

- Pouvoir réordonner avec les facet =\> solution avec tidytext
  <https://juliasilge.com/blog/reorder-within/>

## crosstable

- Implémenter un test stat avec des facet =\> via modélisation
  loglinéaire

- Ajouter un total ? Il faudrait une autre couleur, sinon pas clair =\>
  comment faire vu qu’il y a la palette de couleur des modalités ?

- Réordonner les levels sur la variable `group` ? Mais selon quelle
  valeur (vu qu’il y en a plusieurs) ? Celle du premier level de la
  variable `var_distrib` ?

# Fonctions à créer

## Univarié

- Fréquences relative (distribution entre les modalités d’une variable
  catégorielle)

- Densité pour le général (pour variable continue)

## Bivarié ou + de 2 variables

- Ajouter : prop/moyenne/médiane par 2 groupes

- Distribution par groupe (profils ligne ou colonne) (<u>*ME RAPPELLE
  PLUS : C’EST QUOI ???*</u>)

- Tableau croisé avec résidus ou couleur par proportion (proportions par
  c, l, ou total)

- Différents indicateurs + IC par groupe (<u>*PAS OK AVEC
  PIVOT_LONG_SURVEY ?*</u>)

- Densité par groupe (pour variable continue)
