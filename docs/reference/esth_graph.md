# esth_graph

Function to construct a graphic following the aestetics of the other
function function of this package from a table

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
  pal = "indianred4",
  dodge = 0.9,
  font = "Roboto",
  wrap_width_y = 25,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  theme = NULL
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

- n_var:

  The variable in tab containing the number of observation per for each
  indicator. Default is NULL, not showing the number of observation on
  the plot.

- pvalue:

  The p-value to show in the caption. It can a numeric value or the
  pvalue object from a statsistical test.

- reorder:

  TRUE if you want to reorder var according to value. FALSE if you do
  not want to reorder. Default is FALSE.

- show_value:

  TRUE if you want to show the values of value on the graphic. FALSE if
  you do not want to show the proportion. Default is TRUE.

- name_total:

  Name of the var label that may contain the total. When indicated, it
  is displayed separately on the graph.

- scale:

  Denominator of the proportion. Default is 100 to interprets numbers as
  percentages.

- digits:

  Numbers of digits showed on the values labels on the graphic. Default
  is 0.

- unit:

  The unit showd on the plot. Default is percent.

- dec:

  Decimal mark shown on the graphic. Default is ","

- pal:

  Colour of the bars.

- dodge:

  Width of the bar, between 0 and 1.

- font:

  Font used in the graphic. See load_and_active_fonts() for available
  fonts.

- wrap_width_y:

  Number of characters before going to the line. Applies to the labels
  var. Default is 25.

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

  Theme of the graphic. IWEPS adds y axis lines and ticks.

## Value

A ggplot graphic.

## Examples

``` r
# Making fictional dataframe

data_test<-data.frame(Indicators = c("Variable 1","Variable 2","Variable 3","Variable 4","Variable 5","Tot"),
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
           pal = "green4",
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
