# fonctionr_options

Function to set global options for fonctionr. The arguments defined in
the options are only active if the user has not manually specified a
value for those arguments within the various functions. Arguments may be
shared by multiple functions (if they have the same name) or specific to
certain functions.

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
  parallel = NULL,
  erase_all = FALSE
)
```

## Arguments

- na.rm.group:

  `na.rm.group` argument.

- na.rm.facet:

  `na.rm.facet` argument.

- na.prop:

  `na.prop` argument.

- na.vars:

  `na.vars` argument.

- na.rm.var:

  `na.rm.var` argument.

- probs:

  `probs` argument.

- total:

  `total` argument.

- prop_method:

  `prop_method` argument.

- quantiles:

  `quantiles` argument.

- moustache_probs:

  `moustache_probs` argument.

- bw:

  `bw` argument.

- resolution:

  `resolution` argument.

- height:

  `height` argument.

- limits:

  `limits` argument.

- reorder:

  `reorder` argument.

- position:

  `position` argument.

- show_ci:

  `show_ci` argument.

- show_mid_point:

  `show_mid_point` argument.

- show_mid_line:

  `show_mid_line` argument.

- show_ci_errorbar:

  `show_ci_errorbar` argument.

- show_ci_lines:

  `show_ci_lines` argument.

- show_ci_area:

  `show_ci_area` argument.

- show_quant_lines:

  `show_quant_lines` argument.

- show_moustache:

  `show_moustache` argument.

- show_n:

  `show_n` argument.

- show_value:

  `show_value` argument.

- show_labs:

  `show_labs` argument.

- total_name:

  `total_name` argument.

- scale:

  `scale` argument.

- digits:

  `digits` argument.

- unit:

  `unit` argument.

- dec:

  `dec` argument.

- col:

  `col` argument.

- pal:

  `pal` argument.

- direction:

  `direction` argument.

- desaturate:

  `desaturate` argument.

- lighten:

  `lighten` argument.

- darken:

  `darken` argument.

- col_density:

  `col_density` argument.

- col_moustache:

  `col_moustache` argument.

- col_border:

  `col_border` argument.

- alpha:

  `alpha` argument.

- dodge:

  `dodge` argument.

- font:

  `font` argument.

- wrap_width_y:

  `wrap_width_y` argument.

- wrap_width_leg:

  `wrap_width_leg` argument.

- legend_ncol:

  `legend_ncol` argument.

- title:

  `title` argument.

- subtitle:

  `subtitle` argument.

- xlab:

  `xlab` argument.

- ylab:

  `ylab` argument.

- legend_lab:

  `legend_lab` argument.

- caption:

  `caption` argument.

- lang:

  `lang` argument.

- theme:

  `theme` argument.

- coef_font:

  `coef_font` argument.

- parallel:

  `parallel` argument.

- erase_all:

  `TRUE` erases all the options. Default is `FALSE`.

## Value

No return value, called for side effects.

## Examples

``` r
# We set global settings
fonctionr_options(coef_font = 1.5, col = "magenta", caption = "Beautiful caption")
#> $fonctionr.options
#> $fonctionr.options$col
#> [1] "magenta"
#> 
#> $fonctionr.options$caption
#> [1] "Beautiful caption"
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
#> Warning: NAs introduced by coercion
#> Warning: Active parameters in function r_options(): col, caption, coef_font
#> Input: data.frame
#> Sampling design -> ids:  `1`, weights:  rb050
#> Variable(s) detected in prop_exp: py090n
#> Numbers of observation(s) removed by each filter (one after the other): 
#> 2720 observation(s) removed due to missing group
#> 0 observation(s) removed due to missing value(s) for the variable(s) in prop_exp

# Results in graph form
eusilc_prop$graph
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_text()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text()`).


# We set back settings to default
fonctionr_options(erase_all = TRUE)
#> No fonctionr option active
```
