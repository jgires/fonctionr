
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fonctionr

<!-- badges: start -->
<!-- badges: end -->

Description à venir !

# A régler (nécessités ou bugs)

## Question du filtrage

<!--# JO : C'EST FAIT -->

Il semble qu’il faille absolument filtrer APRES la déclaration du
design. Si on le fait pas, on considère le design sur l’objet filtré
(avec moins de PSU / strates qu’il y en a en réalité), ce qui
sous-estime potentiellement la distribution d’échantillonnage.

Voir :

- <https://stats.stackexchange.com/questions/411026/why-it-is-important-to-make-survey-design-object-svydesign-function-in-r-with-i>

- <https://notstatschat.rbind.io/2021/07/22/subsets-and-subpopulations-in-survey-inference/>

  - *=\> De ce fait, j’ai inclus une option de filtre (`filter_exp`)
    dans les fonctions (qui filtre après la déclaration du design), qui
    évite de filtrer l’objet avant en dégradant le design. =\> Vérifier
    que c’est bien OK !*

## Question de la suppression des valeurs manquantes

Dans `prop_group`, garder la possibilité de calculer une proportion sur
tout le groupe (y compris les `NA`) =\> utile lorsque le `NA` a une
signification (exemple : les personnes qui ne peuvent pas avoir
d’arriérés sur leur loyer ou leur emprunt, car ils n’ont pas de loyer ou
d’emprunt).

## En général

- Ajouter des checks pour les inputs :

  1.  Existence des colonnes dans le dataframe
      ;<!--# JO : C'est fait sauf pour les variables de design + pondération ? Est-ce testable, du fait que j'ai mis l'argument "..." ? Je pense, regarder -->

  2.  Pas mettre les mêmes colonnes dans les différents arguments
      (`group`, `var_distrib`, `facet_var`, etc.) ;

  3.  Suffisamment de modalités (pas de facteur à 1 modalité, par ex.) ;

  4.  Autres ? =\> tester

- Mettre des conditions sur les tests (n min, distribution, variances
  égales…).

- Les cluster \| strates \| weights n’apparaissent pas dans le message
  de la console avec les replicates =\> trouver la cause ? Ou changer de
  méthode =\> faire un print de l’objet survey affiche les
  caractéristiques du design =\> bcp mieux, mais je ne comprends pas
  comment Thomas Lumley arrive à ce résultat.

- Ajouter `n_weight_upp` et `n_weight_low`.

- Harmoniser les thèmes (axes, etc.).

- Harmoniser les noms des colonnes entre fonctions (`n` vs `n_tot`,
  `n_weighted` vs `n_tot_weighted`)

- Ajouter des polices (dont
  Montserrat)<!--# JO : C'EST FAIT : 3 polices sont désormais intégrées dans le package. A tester sur d'autres PC, sur Linux, etc. -->

- Ajouter l’argument `show_labs` + les argument de
  labels.<!--# JO : C'EST FAIT ; j'ai du coup aussi ajoute l'argument caption à quali_distrib(). Par contre les arguments xlab et ylab ne sont pas adaptés => ylab pour l'axe des x, du fait du coord_flip(), c'est étrange -->

- Où afficher la valeur dans les différentes fonctions ? (dans la barre
  mais avec un if pour quand la barre est trop petite
  ?)<!--# JO : Ne pose pas de pb si c'est à l'extérieur de la barre => option à privilégier pour moi (implémenté temporairement) -->

- ~~Bien mettre le n = (en dessous de la
  barre).~~<!--# JO : En fait les n en dessous c'est moche. J'ai remis au milieu. Ce n'est pas possible que ça marche à tous les coups, et n c'est pour la verif, pas grave si parfois ça dépasse de la barre -->

- Ajouter les trucs que François avait modifié dans les fonctions de
  Joël. <!--# JO : C'EST FAIT -->

## central_group

- Bypasser l’erreur du test stat avec `tryCatch()`.
- Implémenter l’export excel comme pour `prop_group` =\> pas oublier
  d’ajouter `broom` dans les dépendances.<!--# JO : C'EST FAIT -->

## prop_group

- Bypasser l’erreur du test stat avec `tryCatch()`.
- Une expression qui contient `is.na(x)` ne fonctionne pas, car je
  supprime les `NA` sur les variables de l’expression ! =\> Exclure la
  possibilité d’entrer la fonction `is.na()` dans
  l’expression<!--# JO : C'EST FAIT -->
  - *=\> Tentative pour que ça marche : est-ce que le code est safe ?*

## quali_distrib_group

- Implémenter un test stat lorsqu’il y a des facet =\> via modélisation
  loglinéaire, mais j’ai un peu de mal à comprendre les erreurs de
  `survey` (erreurs fréquentes).

- Implémenter l’export excel comme pour `prop_group` =\> pas oublier
  d’ajouter `broom` dans les dépendances.<!--# JO : C'EST FAIT -->

## quali_distrib

- Ajouter un test khi2 d’adéquation.

- Ajouter un filtre.

  - *=\> C’est fait, mais il faut ajouter des conditions d’arrêt si
    certain caractères ne fonctionnent pas*
    <!--# JO : J'ai ajouté un stop si les variables de filtre n'existent pas dans data, c'est ce que tu voulais dire ? -->

- Quid des `NA` ? couleur distincte? Si oui à faire
  <!--# JO : Je l'ai fait, comme pour les autres fonctions : NA en gris clair -->

- Quid des `NA` dans les facets? Les supprimer? Faire une catégorie à
  part? Actuellement, il en fait une catégorie à part.
  <!--# JO : J'ai modifié pour faire comme pour les autres fonctions : si on met na.rm = T alors ça supprime aussi sur les facet, car laisser le NA sur facet et pas sur quali_var n'est pas cohérent. Si na.rm = F, alors le NA des facets est laissé. Mettre des na.rm pour chacune des variables n'est pas du tout ergonomique : si la personne veut faire un truc plus fin, elle peut le faire directement avec survey, ce n'est pas le but du package -->

- Concernant le ré-ordonnancement des modalités, le choix a été fait de
  ne pas laisser la possibilité de réordonner.
  <!--# J'ai finalement ajouté l'option. Il fallait bouger la cat NA en bas, ce qui demandait la même chose que pour réordonner les catégories => autant le faire. Par ailleurs en réalité ce peut être utile, en fonction de ce que tu cherches -->

- Exporter en excel.<!--# JO : C'est fait -->

- Mettre les n et positionner à gauche.

  - *=\> C’est fait, mais comment représenter quand la part est trop
    faible ? Et est-ce pertinent? Faut-il ne pas plutôt mettre la
    totalité du n ?*

# Améliorations

## En général

- Créer une fonction pour transformer les données en objet srvyr
  (redondance entre les 4 fonctions)

## central_group

- Pouvoir réordonner avec les facet =\> solution avec `tidytext`
  <https://juliasilge.com/blog/reorder-within/>

## prop_group

- Pouvoir réordonner avec les facet =\> solution avec `tidytext`
  <https://juliasilge.com/blog/reorder-within/>

## quali_distrib_group

- Ajouter un total ? Il faudrait une autre couleur, sinon pas clair =\>
  comment faire vu qu’il y a la palette de couleur des modalités
  ?<!--# FR : idée : utiliser des textures? hachures?  -->

- Réordonner les levels sur la variable `group` ? Mais selon quelle
  valeur (vu qu’il y en a plusieurs) ? Celle du premier level de la
  variable `var_distrib`
  ?<!--# FR : cela peut me sembler une bonne idée. -->

- Ajouter les effectifs totaux par groupe ? (dans le nom du groupe
  ?)<!--# FR : Cela me semble une bonne idée -->

- Possibilité d’indiquer un vecteur avec une palette de couleur pour
  coller avec le code couleur de notre
  institution.<!--# JO : Bonne idée -->

- Ajuster la légende (nombre de lignes?) pour éviter de masquer une
  partie de celle-ci. J’ai souvent eu le cas. Même si c’est un détail,
  cela rend impossible l’utilisation du graphique propre dans un
  rapport<!--# JO : j'ai simplifié le code -->.

# Fonctions à créer

## Univarié

- Densité pour le général (pour variable continue)

## Bivarié ou 3 variables+

- prop/moyenne/médiane par 2 groupes

- Distribution par groupe (profils ligne ou colonne) (<u>*ME RAPPELLE
  PLUS : C’EST QUOI ???*</u>)

- Tableau croisé avec résidus ou couleur par proportion (proportions par
  c, l, ou total)

- Différents indicateurs + IC par groupe (<u>*PAS OK AVEC
  PIVOT_LONG_SURVEY ?*</u>)

- Densité par groupe (pour variable continue)
