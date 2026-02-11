# fonctionr_options

Function to set global options for fonctionr

## Usage

``` r
fonctionr_options(
  na.rm.group = NULL,
  na.rm.facet = NULL,
  na.prop = NULL,
  na.vars = NULL,
  na.rm.var = NULL,
  probs = NULL,
  total = NULL,
  prop_method = NULL,
  quantiles = NULL,
  moustache_probs = NULL,
  bw = NULL,
  resolution = NULL,
  height = NULL,
  limits = NULL,
  reorder = NULL,
  position = NULL,
  show_ci = NULL,
  show_mid_point = NULL,
  show_mid_line = NULL,
  show_ci_errorbar = NULL,
  show_ci_lines = NULL,
  show_ci_area = NULL,
  show_quant_lines = NULL,
  show_moustache = NULL,
  show_n = NULL,
  show_value = NULL,
  show_labs = NULL,
  total_name = NULL,
  scale = NULL,
  digits = NULL,
  unit = NULL,
  dec = NULL,
  col = NULL,
  pal = NULL,
  direction = NULL,
  desaturate = NULL,
  lighten = NULL,
  darken = NULL,
  col_density = NULL,
  col_moustache = NULL,
  col_border = NULL,
  alpha = NULL,
  dodge = NULL,
  font = NULL,
  wrap_width_y = NULL,
  wrap_width_leg = NULL,
  legend_ncol = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  legend_lab = NULL,
  caption = NULL,
  lang = NULL,
  theme = NULL,
  coef_font = NULL,
  erase_all = FALSE
)
```

## Arguments

- na.rm.group:

  na.rm.group

- na.rm.facet:

  na.rm.facet

- na.prop:

  na.prop

- na.vars:

  na.vars

- na.rm.var:

  na.rm.var

- probs:

  probs

- total:

  total

- prop_method:

  prop_method

- quantiles:

  quantiles

- moustache_probs:

  moustache_probs

- bw:

  bw

- resolution:

  resolution

- height:

  height

- limits:

  limits

- reorder:

  reorder

- position:

  position

- show_ci:

  show_ci

- show_mid_point:

  show_mid_point

- show_mid_line:

  show_mid_line

- show_ci_errorbar:

  show_ci_errorbar

- show_ci_lines:

  show_ci_lines

- show_ci_area:

  show_ci_area

- show_quant_lines:

  show_quant_lines

- show_moustache:

  show_moustache

- show_n:

  show_n

- show_value:

  show_value

- show_labs:

  show_labs

- total_name:

  total_name

- scale:

  scale

- digits:

  digits

- unit:

  unit

- dec:

  dec

- col:

  col

- pal:

  pal

- direction:

  direction

- desaturate:

  desaturate

- lighten:

  lighten

- darken:

  darken

- col_density:

  col_density

- col_moustache:

  col_moustache

- col_border:

  col_border

- alpha:

  alpha

- dodge:

  dodge

- font:

  font

- wrap_width_y:

  wrap_width_y

- wrap_width_leg:

  wrap_width_leg

- legend_ncol:

  legend_ncol

- title:

  title

- subtitle:

  subtitle

- xlab:

  xlab

- ylab:

  ylab

- legend_lab:

  legend_lab

- caption:

  caption

- lang:

  lang

- theme:

  theme

- coef_font:

  coef_font

- erase_all:

  TRUE erases all the options. Default is FALSE.

## Examples

``` r
# We set settings for font type and font size
fonctionr_options(font = "Montserrat", coef_font = 1.5)
#> $fonctionr.options
#> $fonctionr.options$font
#> [1] "Montserrat"
#> 
#> $fonctionr.options$coef_font
#> [1] 1.5
#> 
#> 

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
eusilc_prop <- prop_group(
eusilc,
group = pl030_rec,
prop_exp = py090n > 0,
weight = rb050,
title = "% of ind. receiving unemployment benefits in their hh",
subtitle = "Example with austrian SILC data from 'laeken' package"
)
#> Warning: NAs introduced by coercion
#> Warning: Active parameters in function r_options(): font, coef_font
#> Input: data.frame
#> Sampling design -> ids:  `1`, weights:  rb050
#> Numbers of observation(s) removed by each filter (one after the other): 
#> 2720 observation(s) removed due to missing group
#> Variable(s) detected in prop_exp: py090n
#> 0 observation(s) removed due to missing value(s) for the variable(s) in prop_exp

# Results in graph form
eusilc_prop$graph
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_text()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text()`).

```
