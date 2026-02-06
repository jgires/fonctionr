# esth_graph

Function to construct a graphic following the aestetics of the other
functions of functionr from a table

## Usage

``` r
esth_graph(
  tab,
  var,
  value,
  error_low = NULL,
  error_upp = NULL,
  facet = NULL,
  n_var = NULL,
  pvalue = NULL,
  reorder = F,
  show_value = TRUE,
  name_total = NULL,
  scale = 1,
  digits = 2,
  unit = "",
  dec = ",",
  pal = NULL,
  col = "indianred4",
  dodge = 0.9,
  font = "Roboto",
  wrap_width_y = 25,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  theme = "fonctionr",
  coef_font = 1
)
```

## Arguments

- tab:

  dataframe with the indicators to be ploted.

- var:

  The variable in tab with the labels of the indicator to be ploted.

- value:

  The variable in tab with the values of the indicator to be ploted.

- error_low:

  The variable in tab with the lower bound of the confidence interval.
  If either error_low or error_upp is NULL error bars are not shown on
  the graphic.

- error_upp:

  The variable in tab with the upper bound of the confidence interval.
  If either error_low or error_upp is NULL error bars are not shown on
  the graphic.

- facet:

  A variable in tab defining the faceting group, if applicable. Default
  is NULL.

- n_var:

  The variable in tab containing the number of observations for each
  indicator ploted. Default (NULL) does not show the numbers of
  observations on the plot.

- pvalue:

  The p-value to show in the caption. It can be a numeric value or the
  pvalue object from a statsistical test.

- reorder:

  TRUE if you want to reorder var according to value. FALSE if you do
  not want to reorder. NA and total labels in var are not included in
  the reorder. Default is FALSE.

- show_value:

  TRUE if you want to show the values on the graphic. FALSE if you do
  not want to show them. Default is TRUE.

- name_total:

  Name of the var label that may contain the total. When indicated, it
  is displayed separately (bold name and value color is 'grey40') on the
  graph.

- scale:

  Denominator of the indicator. Default is 1 to not modify indicators.

- digits:

  Number of decimal places displayed on the values labels on the
  graphic. Default is 0.

- unit:

  The unit displayed on the grphaic. Default is no unit.

- dec:

  Decimal mark shown on the graphic. Default is ","

- pal:

  For compatibility with old versions.

- col:

  Color of the bars. col must be a R color or an hexadecimal color code.
  Default is "indianred4". The color of NA and total are always "grey"
  and "grey40".

- dodge:

  Width of the bars. Default is 0.9 to let a small space between bars. A
  value of 1 leads to no space betweens bars. Values higher than 1 are
  not advised because they cause an overlaping of the bars.

- font:

  Font used in the graphic. See load_and_active_fonts() for available
  fonts. Default is "Roboto".

- wrap_width_y:

  Number of characters before going to the line for the labels of var
  Default is 25.

- title:

  Title of the graphic.

- subtitle:

  Subtitle of the graphic.

- xlab:

  X label on the graphic. As coord_flip() is used in the graphic, xlab
  refers to the x label on the graphic, after the coord_flip(), and not
  to var in tab.

- ylab:

  Y label on the graphic. As coord_flip() is used in the graphic, ylab
  refers to the y label on the graphic, after the coord_flip(), and not
  to value in tab.

- caption:

  Caption of the graphic.

- theme:

  Theme of the graphic. Default is "fonctionr". "IWEPS" adds y axis
  lines and ticks. NULL uses the default grey ggplot2 theme.

- coef_font:

  A multiplier factor for font size of all fonts on the graphic. Default
  is 1. Usefull when exporting the graphic for a publication (e.g. in a
  Quarto document).

## Value

A ggplot graphic.

## Examples

``` r
# Making fictional dataframe

data_test<-data.frame(Indicators = c("Variable 1",
                                     "Variable 2",
                                     "Variable 3",
                                     "Variable 4",
                                     "Variable 5",
                                     "Tot"),
                      Estimates = c(1.52,1.63,2.34,4.15,1.32,2.13),
                      IC_low = c(1.32,1.4,1.98,4,14.2,26),
                      IC_upp = c(1.73,1.81,22.4,47.44,1.45,2.34),
                      sample_size = c(215,300,129,212,189,1045))

# Using dataframe to make a plot
plot_test<-esth_graph(data_test,
           var = Indicators,
           value = Estimates,
           error_low = IC_low,
           error_upp = IC_upp,
           n_var = sample_size,
           pvalue = .001,
           reorder = TRUE,
           show_value = TRUE,
           name_total = "Tot",
           scale = 1,
           digits = 1,
           unit = "%",
           dec = ".",
           col = "green4",
           dodge = 0.8,
           font = "Montserrat",
           wrap_width_y = 25,
           title = "Plot",
           subtitle = "Using fake data",
           xlab = "Proportion (in %)",
           ylab = "Indicators",
           caption = "Source: fictional own calculation",
           theme = "IWEPS")

# Result is a ggplot
plot_test
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text()`).
#> Warning: Removed 5 rows containing missing values or values outside the scale range
#> (`geom_text()`).

```
