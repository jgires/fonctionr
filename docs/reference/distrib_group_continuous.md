# distrib_group_continuous

Function to compare means or medians in different groups from complex
survey data. It produces a table, a graphic and a statistical test.

## Usage

``` r
distrib_group_continuous(
  data,
  group,
  quanti_exp,
  type = "median",
  facet = NULL,
  filter_exp = NULL,
  ...,
  na.rm.group = TRUE,
  na.rm.facet = TRUE,
  quantiles = seq(0.1, 0.9, 0.1),
  moustache_probs = c(0.95, 0.8, 0.5),
  bw = 1,
  resolution = 512,
  height = 0.8,
  limits = NULL,
  reorder = FALSE,
  show_mid_point = TRUE,
  show_mid_line = FALSE,
  show_ci_errorbar = TRUE,
  show_ci_lines = FALSE,
  show_ci_area = FALSE,
  show_quant_lines = FALSE,
  show_moustache = TRUE,
  show_value = TRUE,
  show_labs = TRUE,
  digits = 0,
  unit = "",
  dec = NULL,
  pal = "#e0dfe0",
  pal_moustache = c("#EB9BA0", "#FAD7B1"),
  color = NA,
  alpha = 1,
  font = "Roboto",
  wrap_width_y = 25,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  lang = "fr",
  theme = NULL,
  export_path = NULL
)

distrib_group_c(...)
```

## Arguments

- data:

  A dataframe or an object from the survey package or an object from the
  srvyr package.

- group:

  A variable defining groups to be compared.

- quanti_exp:

  An expression that define the variable the variable to be described.

- type:

  Type of central value : "mean" to compute mean as the central value by
  group ; "median" to compute median as the central value by group..

- facet:

  A supprimer?

- filter_exp:

  An expression that filters the data, preserving the design.

- ...:

  All options possible in as_survey_design in srvyr package.

- na.rm.group:

  TRUE if you want to remove observations with NA on the group variable.
  FALSE if you want to create a group with the NA value for the group
  variable. NA in the variables included in quanti_exp are not affected
  in this argument. All the observation with a NA in the variables
  included in quanti_exp are excluded.

- na.rm.facet:

  Argument a supprimer?

- quantiles:

  Quantiles to be computed in the distributions. Default are deciles.

- moustache_probs:

  A vector defining the proportions of the population used to draw the
  moustache. Default is 0.95, 0.8, 0.5 to draw a moustache with three
  groups containing respectively 50 percent, 80 percent and 95 percent
  of the population closest to the median.

- bw:

  The smoothing bandwidth to be used. The kernels are scaled such that
  this is the standard deviation of the smoothing kernel. Default is 1.

- resolution:

  Resolution of the density curve. Default is 512

- height:

  Height of the curves. Default is 0.8

- limits:

  Limits of the x axe of the graphic. Does not apply to the computation.
  Default is NULL to show the entire distribution on the graphic.

- reorder:

  TRUE if you want to reorder the groups according to the mean/median
  (depending on type). NA value, in case if na.rm.group = FALSE, is not
  included in the reorder (A VERIFIER).

- show_mid_point:

  TRUE if you want to show the mean or median (depending on type) as a
  point on the graphic. FALSE if you do not want to show it. Default is
  TRUE.

- show_mid_line:

  TRUE if you want to show the mean or median (depending on type) as a
  line on the graphic. FALSE if you do not want to show it. Default is
  FALSE

- show_ci_errorbar:

  TRUE if you want to show confidence interval of the mean or median
  (depending on type) as an error bar on the graphic. FALSE if you do
  not want to show it as lines. Default is TRUE.

- show_ci_lines:

  TRUE if you want to show confidence interval of the mean or median
  (depending on type) as lines on the graphic. FALSE if you do not want
  to show it as lines. Default is FALSE

- show_ci_area:

  TRUE if you want to show confidence interval of the mean or median
  (depending on type) as a coloured area on the graphic. FALSE if you do
  not want to show it as an area. Default is FALSE.

- show_quant_lines:

  TRUE if you want to show quantiles as lines on the graphic. FALSE if
  you do not want to show them as lines. Default is FALSE.

- show_moustache:

  TRUE if you want to show the moustache on the graphic. FALSE if you do
  not want to show it. Default is TRUE.

- show_value:

  TRUE if you want to show the mean/median of each group on the graphic.
  FALSE if you do not want to show the mean/median. Default is TRUE.

- show_labs:

  TRUE if you want to show axes, titles and caption labels. FALSE if you
  do not want to show any label on axes and titles. Default is TRUE.

- digits:

  Numbers of digits showed on the value labels on the graphic. Default
  is 0.

- unit:

  Unit showed on the graphic. Default is no unit.

- dec:

  Decimal mark shown on the graphic. Depends on lang: "," for fr and nl
  ; "." for en.

- pal:

  Color of the density areas. Can be one or sereval colors to create a
  palette.

- pal_moustache:

  Color of the moustache. Can be one or sereval colors to create a
  palette.

- color:

  Color of the density curve. Has to be one color.

- alpha:

  Transparence of the density curve. Default is 1.

- font:

  Font used in the graphic. See load_and_active_fonts() for available
  fonts.

- wrap_width_y:

  Number of characters before going to the line in the labels of the
  groups. Default is 25

- title:

  Title of the graphic.

- subtitle:

  Subtitle of the graphic.

- xlab:

  X label on the graphic. As coord_flip() is used in the graphic, xlab
  refers to the X label on the graphic, after the coord_flip(), and not
  to the x variable in the data. If xlab = NULL, X label on the graphic
  will be "Moyenne : " + quanti_exp or "Medianne : " + quanti_exp. To
  show no X label, use xlab = "".

- ylab:

  Y label on the graphic. As coord_flip() is used in the graphic, ylab
  refers to the Y label on the graphic, after the coord_flip(), and not
  to the y variable in the data. If ylab = NULL, Y label on the graphic
  will be group. To show no Y label, use ylab = "".

- caption:

  Caption of the graphic.

- lang:

  The language of the indications on the chart. Possibilities: "fr",
  "nl", "en". Default is "fr".

- theme:

  Theme of the graphic. IWEPS adds y axis lines and ticks.

- export_path:

  Path to export the results in an xlsx file. The file includes three
  sheets : the table, the graphic and the statistical test.

## Value

A list that contains a table, a graphic and a statistical test

## Examples

``` r
# Loading of data
data(eusilc, package = "laeken")

# Recoding eusilc$pl030 into eusilc$pl030_rec
eusilc$pl030_rec <- NA
eusilc$pl030_rec[eusilc$pl030 == "1"] <- "Working full time"
eusilc$pl030_rec[eusilc$pl030 == "2"] <- "Working part time"
eusilc$pl030_rec[eusilc$pl030 == "3"] <- "Unemployed"
eusilc$pl030_rec[eusilc$pl030 == "4"] <- "Student"
eusilc$pl030_rec[eusilc$pl030 == "5"] <- "Retired"
eusilc$pl030_rec[eusilc$pl030 == "6"] <- "Permanently disabled"
eusilc$pl030_rec[eusilc$pl030 == "7"] <- "Fulfilling domestic tasks"

# Computation, taking sample design into account
eusilc_dist_g_c <- distrib_group_c(
  eusilc,
  group = pl030_rec,
  quanti_exp = eqIncome,
  strata = db040,
  ids = db030,
  weight = rb050,
  limits = c(0, 50000),
  title = "Distribution of eq. income",
  subtitle = "Example with austrian SILC data from 'laeken' package"
)
#> Input: data.frame
#> Sampling design -> ids:  db030, strata:  db040, weights:  rb050
#> Variable(s) detectee(s) dans quanti_exp : eqIncome
#> 0 lignes supprimees avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp

# Results in graph form
eusilc_dist_g_c$graph
#> `height` was translated to `width`.
#> Warning: Removed 1978 rows containing missing values or values outside the scale range
#> (`geom_ribbon()`).
#> Warning: Removed 3710 rows containing missing values or values outside the scale range
#> (`geom_line()`).


# Results in table format
eusilc_dist_g_c$tab
#> # A tibble: 7 × 8
#>   pl030_rec      median median_low median_upp n_sample n_weighted n_weighted_low
#>   <chr>           <dbl>      <dbl>      <dbl>    <int>      <dbl>          <dbl>
#> 1 Fulfilling do… 14367.     13918.     14774.     1207    640311.        605978.
#> 2 Permanently d… 15967.     13753.     16797.      178    104930.         85796.
#> 3 Retired        18401.     17956.     18887.     3146   1806954.       1746273.
#> 4 Student        14435.     13780.     15133.      736    395829.        365532.
#> 5 Unemployed     15872.     14725.     16900.      518    303252.        276953.
#> 6 Working full … 21032.     20644.     21406.     5162   2869868.       2797833.
#> 7 Working part … 17580.     17043.     18270.     1160    636121.        600709.
#> # ℹ 1 more variable: n_weighted_upp <dbl>
```
