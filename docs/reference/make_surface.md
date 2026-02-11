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
  col = NULL,
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
  subtitle = NULL,
  hjust.title = 0,
  caption = NULL,
  coef_font = 1
)
```

## Arguments

- tab:

  dataframe with the variables to be ploted.

- var:

  The variable in tab with the labels of the indicator to be ploted.

- value:

  The variable in tab with the values of the indicator to be ploted.

- error_low:

  The variable in tab that is the lower bound of the confidence
  interval. If either error_low or error_upp is NULL error rectangles
  are not shown on the graphic.

- error_upp:

  The variable in tab that is the upper bound of the confidence
  interval. If either error_low or error_upp is NULL error rectangles
  are not shown on the graphic.

- facet:

  A variable in tab defining the faceting group, if applicable. Default
  is NULL.

- pvalue:

  The p-value to show in the caption. It can be a numeric value or the
  pvalue object from a statsistical test.

- reorder:

  TRUE if you want to reorder the values. NA label in var is not
  included in the reorder.

- compare:

  TRUE to display a rectangle representing the smallest value. When
  facets are enabled, this is the smallest value per facet category.

- space:

  The space between the rectangles. The unit is that of the indicator.

- position:

  The position of the rectangles: "mid" for center alignment, "bottom"
  for bottom alignment.

- show_ci:

  TRUE if you want to show the CI on the graphic. The bounds of the
  confidence intervals are displayed as dotted rectangles around the
  result. FALSE if you do not want to show them. Default is TRUE.

- name_total:

  Name of the var label that may contain the total. When indicated, it
  is not displayed on the graph.

- digits:

  Number of decimal places displayed on the values labels on the
  graphic. Default is 0.

- unit:

  The unit showd on the plot. Default is none.

- col:

  Color of the rectangles if the user wants a monocolor graph. col must
  be a R color or an hexadecimal color code. As pal has a priority over
  col, if the user wants to use col, he must not use simultaneously the
  pal argument (even pal = NULL).

- pal:

  Colors of the rectangles if the user wants the rectangles to have
  different colors. pal must be vector of R colors or hexadecimal colors
  or a palette from packages MetBrewer or PrettyCols or a palette from
  fonctionr. Default is "Kandinsky" from MetBrewer. pal has a priority
  over col.

- direction:

  Direction of the palette color. Default is 1. The opposite direction
  is -1.

- desaturate:

  Numeric specifying the amount of desaturation where 1 corresponds to
  complete desaturation (no colors, grey layers only), 0 to no
  desaturation, and values in between to partial desaturation. Default
  is 0. It affects only the palette (pal) and not the monocolor (col).
  See colorspace::desaturate for details. If desaturate and
  lighten/darken arguments are used, lighten/darken is applied in a
  second time (i.e. on the color transformed by desaturate).

- lighten:

  Numeric specifying the amount of lightening. Negative numbers cause
  darkening. Value shoud be ranged between -1 (black) and 1 (white).
  Default is 0. It affects only the palette (pal) and not the monocolor
  (col). See colorspace::lighten for details. If both argument ligthen
  and darken are used (not advised), darken is applied in a second time
  (i.e. on the color transformed by lighten).

- darken:

  Numeric specifying the amount of lightening. Negative numbers cause
  lightening. Value shoud be ranged between -1 (white) and 1 (black).
  Default is 0. It affects only the palette (pal) and not the monocolor
  (col). See colorspace::darken for details. If both argument ligthen
  and darken are used (not advised), darken is applied in a second time
  (i.e. on the color transformed by lighten).

- size_text:

  Text size displayed in rectangles . Default is 3.88 (as in ggplot2).

- bg:

  Color of the background. bg must be a R color or an hexadecimal color
  code.

- linewidth_ci:

  Line width of the dotted confidence intervals lines. It affects also
  the lenghts of the dots and spaces bteween dots. Default is 0.5 to
  have confidence lines two times thiner than the lines of the
  indicators.

- ratio:

  Ratio between the length and the width of the rectangles. 1 produces
  squares ; greater than 1 produces vertical rectangles and smaller than
  1 produces horizontal rectangles. Default is 3/2.

- font:

  Font used in the graphic. See load_and_active_fonts() for available
  fonts. Default is "Roboto".

- wrap_width_lab:

  Number of characters before going to the line for the labels of the
  categories of var. Default is 20.

- title:

  Title of the graphic.

- subtitle:

  Subtitle of the graphic.

- hjust.title:

  Horizontal alignment of title & subtitle. It should take a numeric
  value. Default (0) leads to left alignment, 1 leads to right alignment
  and 0.5 leads to centered alignement.

- caption:

  Caption of the graphic.

- coef_font:

  A multiplier factor for font size of all fonts on the graphic. Default
  is 1. Usefull when exporting the graphic for a publication (e.g. in a
  Quarto document).

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
#> Warning: NAs introduced by coercion
#> Warning: Active parameters in function r_options(): font, coef_font
#> Input: data.frame
#> Sampling design -> ids:  `1`, weights:  rb050
#> Numbers of observation(s) removed by each filter (one after the other): 
#> 13680 observation(s) removed by filter_exp
#> 296 observation(s) removed due to missing group
#> Variable(s) detected in quanti_exp: py010n, py050n, py090n, py100n, py110n, py120n, py130n, py140n
#> 0 observation(s) removed due to missing value(s) for the variable(s) in quanti_exp

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
#> Warning: NAs introduced by coercion
#> Warning: Parametres actifs dans fonctionr_options(): font, coef_font

```
