# central_group

Function to compare means or medians in different groups from complex
survey data. It produces a table, a graphic and a statistical test.

## Usage

``` r
central_group(
  data,
  group,
  quanti_exp,
  type,
  group.fill = NULL,
  facet = NULL,
  filter_exp = NULL,
  ...,
  na.rm.group = T,
  na.rm.facet = T,
  total = TRUE,
  reorder = F,
  show_ci = T,
  show_n = FALSE,
  show_value = TRUE,
  show_labs = TRUE,
  total_name = NULL,
  digits = 0,
  unit = "",
  dec = NULL,
  pal = NULL,
  direction = 1,
  desaturate = 0,
  lighten = 0,
  darken = 0,
  dodge = 0.9,
  font = "Roboto",
  wrap_width_y = 25,
  wrap_width_leg = 25,
  legend_ncol = 4,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  legend_lab = NULL,
  caption = NULL,
  lang = "fr",
  theme = NULL,
  export_path = NULL
)

median_group(..., type = "median")

mean_group(..., type = "mean")
```

## Arguments

- data:

  A dataframe or an object from the survey package or an object from the
  srvyr package.

- group:

  A variable defining groups to be compared.

- quanti_exp:

  An expression that define the variable from which the mean/median is
  computed.

- type:

  "mean" to compute mean by group ; "median" to compute median by group.

- group.fill:

  A variable defining a second variable of groups to be compared.

- facet:

  A variable defining the faceting group.

- filter_exp:

  An expression that filters the data, preserving the design.

- ...:

  All options possible in as_survey_design in srvyr package.

- na.rm.group:

  TRUE if you want to remove observations with NA on the group variable
  or NA on the facet variable, if applicable. FALSE if you want to
  create a group with the NA value for the group variable and a facet
  with the NA value for the facet variable. NA in the variables included
  in quanti_exp are not affected in this argument. All the observation
  with a NA in the variables included in quanti_exp are always excluded.
  Default is TRUE.

- na.rm.facet:

  TRUE if you want to remove observations with NA on the group variable
  or NA on the facet variable. FALSE if you want to create a group with
  the NA value for the group variable and a facet with the NA value for
  the facet variable. NA in the variables included in prop_exp are not
  affected in this argument. All the observation with a NA in the
  variables included in prop_exp are excluded.

- total:

  TRUE if you want to calculate a total, FALSE if you don't. The default
  is TRUE

- reorder:

  TRUE if you want to reorder the groups according to the mean/median.
  NA value, in case if na.rm.group = FALSE, is not included in the
  reorder.

- show_ci:

  TRUE if you want to show the error bars on the graphic. FALSE if you
  do not want to show the error bars. Default is TRUE.

- show_n:

  TRUE if you want to show on the graphic the number of individuals in
  the sample in each group. FALSE if you do not want to show this
  number. Default is FALSE.

- show_value:

  TRUE if you want to show the mean/median of each group on the graphic.
  FALSE if you do not want to show the mean/median. Default is TRUE.

- show_labs:

  TRUE if you want to show axes, titles and caption labels. FALSE if you
  do not want to show any label on axes and titles. Default is TRUE.

- total_name:

  Name of the total bar on the graphic. Default is Total.

- digits:

  Numbers of digits showed on the value labels on the graphic. Default
  is 0.

- unit:

  Unit showed on the graphic. Default is no unit.

- dec:

  Decimal mark shown on the graphic. Depends on lang: "," for fr and nl
  ; "." for en.

- pal:

  If group.fill is empty, pal must be a vector containing a single color
  to define the color of the bars. If a variable is specified in
  group.fill, pal is the color palette used on the graph to
  differentiate its different modalities. Palettes from fonctionr and
  the MetBrewer and PrettyCols packages are available. The NA bar, if
  na.rm.group = FALSE, and the total bar are always in gray.

- direction:

  Direction of the palette color. Default is 1. The opposite direction
  is -1.

- desaturate:

  Numeric specifying the amount of desaturation where 1 corresponds to
  complete desaturation, 0 to no desaturation, and values in between to
  partial desaturation.

- lighten:

  Numeric specifying the amount of lightening. Negative numbers cause
  darkening.

- darken:

  Numeric specifying the amount of lightening. Negative numbers cause
  lightening.

- dodge:

  Width of the bar, between 0 and 1.Default is 0.9.

- font:

  Font used in the graphic. See load_and_active_fonts() for available
  fonts.

- wrap_width_y:

  Number of characters before going to the line in the labels of the
  groups. Default is 25.

- wrap_width_leg:

  Number of characters before going to the line for the labels of the
  categories of group.fill. Default is 25.

- legend_ncol:

  Number of colomns in the legend. Default is 4.

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

- legend_lab:

  Legend (fill) label on the graphic. If legend_lab = NULL, legend label
  on the graphic will be group.fill. To show no legend label, use
  legend_lab = "".

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

# Creation of age categories
eusilc$age_cat <- cut(eusilc$age,
breaks = 6,
include.lowest = TRUE)

# Calculation of income means by age category with fonctionr, taking sample design into account
eusilc_mean <- mean_group(
  eusilc,
  group = age_cat,
  quanti_exp = eqIncome / 12,
  strata = db040,
  ids = db030,
  weight = rb050,
  title = "Mean of equivalised income in household by age of individuals",
  subtitle = "Example with austrian SILC data from 'laeken' package"
  )
#> Input: data.frame
#> Sampling design -> ids:  db030, strata:  db040, weights:  rb050
#> Variable(s) detectee(s) dans quanti_exp : eqIncome
#> 0 lignes supprimees avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp

# Results in graph form
eusilc_mean$graph
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_text()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text()`).


# Results in table format
eusilc_mean$tab
#> # A tibble: 7 × 8
#>   age_cat      mean mean_low mean_upp n_sample n_weighted n_weighted_low
#>   <fct>       <dbl>    <dbl>    <dbl>    <int>      <dbl>          <dbl>
#> 1 [-1.1,15.3] 1444.    1409.    1479.     2720   1424958.       1358818.
#> 2 (15.3,31.7] 1607.    1571.    1643.     2944   1612502.       1549489.
#> 3 (31.7,48]   1703.    1669.    1737.     4025   2230581.       2163723.
#> 4 (48,64.3]   1889.    1843.    1935.     2817   1578046.       1517273.
#> 5 (64.3,80.7] 1611.    1561.    1662.     1847   1053098.       1001605.
#> 6 (80.7,97.1] 1545.    1467.    1622.      474    283037.        256754.
#> 7 Total       1658.    1635.    1681.    14827   8182222        8079226.
#> # ℹ 1 more variable: n_weighted_upp <dbl>
```
