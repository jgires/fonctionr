# make_surface

make_surface

## Usage

``` r
make_surface(
  tab,
  var,
  value,
  error_low = NULL,
  error_upp = NULL,
  facet = NULL,
  pvalue = NULL,
  reorder = F,
  compare = F,
  space = NULL,
  position = "mid",
  show_ci = TRUE,
  name_total = "Total",
  digits = 0,
  unit = NULL,
  pal = "Kandinsky",
  direction = 1,
  desaturate = 0,
  lighten = 0,
  darken = 0,
  size_text = 3.88,
  bg = "#f8f5f5",
  linewidth_ci = 0.5,
  ratio = 3/2,
  font = "Roboto",
  wrap_width_lab = 20,
  title = NULL,
  hjust.title = 0,
  subtitle = NULL,
  caption = NULL
)
```

## Arguments

- tab:

  dataframe with the variables to be ploted.

- var:

  The variable in tab with the labels of the indicators to be ploted.

- value:

  The variable in tab with the values of the indicator to be ploted.

- error_low:

  The variable in tab that is the lower bound of the confidence
  interval. If either error_low or error_upp is NULL error bars are not
  shown on the graphic.

- error_upp:

  The variable in tab that is the upper bound of the confidence
  interval. If either error_low or error_upp is NULL error bars are not
  shown on the graphic.

- facet:

  A variable in tab defining the faceting group, if applicable. Default
  is NULL.

- pvalue:

  The p-value to show in the caption. It can a numeric value or the
  pvalue object from a statsistical test.

- reorder:

  TRUE if you want to reorder the values. NA value is not included in
  the reorder.

- compare:

  TRUE to display a square representing the smallest value. When facets
  are enabled, this is the smallest value per facet category.

- space:

  The space between the squares. The unit is that of the indicator.

- position:

  The position of the squares: "mid" for center alignment, "bottom" for
  bottom alignment.

- show_ci:

  TRUE if you want to show the CI on the graphic. The bounds of the
  confidence intervals are displayed as dotted squares around the
  result. FALSE if you do not want to show them. Default is TRUE.

- name_total:

  Name of the var label that may contain the total. When indicated, it
  is not displayed on the graph.

- digits:

  Numbers of digits showed on the values labels on the graphic. Default
  is 0.

- unit:

  The unit showd on the plot. Default is percent.

- pal:

  Color palette used on the graphic. The palettes from the packages
  MetBrewer, MoMAColors and PrettyCols are available.

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

- size_text:

  Text size displayed in surfaces. Default is 3.88 (as in ggplot2).

- bg:

  Color of the background.

- linewidth_ci:

  linewidth of ci borders.

- ratio:

  Aspect ratio of the surfaces.

- font:

  Font used in the graphic. See load_and_active_fonts() for available
  fonts.

- wrap_width_lab:

  Number of characters before going to the line for the labels of the
  categories of var. Default is 20.

- title:

  Title of the graphic.

- hjust.title:

  Horizontal alignment of title & subtitle.

- subtitle:

  Subtitle of the graphic.

- caption:

  Caption of the graphic.

## Value

A ggplot graphic.

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

# Calculation of income means by age category with fonctionr, taking sample design into account
eusilc_mean <- mean_group(
  eusilc,
  group = pl030_rec,
  quanti_exp = py010n + py050n + py090n + py100n + py110n + py120n + py130n + py140n,
  filter_exp = !pl030_rec %in% c("Student", "Fulfilling domestic tasks") & db040 == "Tyrol",
  weights = rb050
)
#> Input: data.frame
#> Sampling design -> ids:  `1`, weights:  rb050
#> Variable(s) detectee(s) dans quanti_exp : py010n, py050n, py090n, py100n, py110n, py120n, py130n, py140n
#> 0 lignes supprimees avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp

# Displaying results with make_surface()
eusilc_mean$tab |>
  make_surface(
    var = pl030_rec,
    value = mean,
    error_low = mean_low,
    error_upp = mean_upp,
    reorder = TRUE,
    wrap_width_lab = 15,
    unit = "€",
    title = "Equivalised income in household by socio-economic status",
    subtitle = "Example with austrian SILC data from 'laeken' package"
)

```
