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
