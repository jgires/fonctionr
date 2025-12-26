# official_pal

Function to produce color palettes from different institutions

## Usage

``` r
official_pal(
  inst,
  n,
  direction = 1,
  desaturate = 0,
  lighten = 0,
  darken = 0,
  show_pal = F,
  font = "Gotham Narrow",
  list_pal_names = F
)
```

## Arguments

- inst:

  Name of the palette.

- n:

  Number of colors.

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

- show_pal:

  TRUE to display a graph representing the specified color palette.

- font:

  Font used in the graphic. See load_and_active_fonts() for available
  fonts.

- list_pal_names:

  TRUE to generate a vector with palette names.

## Examples

``` r
official_pal("OBSS", 8, show_pal = TRUE)

official_pal("OBSS_Greens", 8, show_pal = TRUE)

official_pal("OBSS_div_mid4", 7, show_pal = TRUE)

official_pal("OBSS_div_bi3", 8, show_pal = TRUE)

official_pal("IBSA", 4, show_pal = TRUE)

official_pal("ULB", 6, show_pal = TRUE)

```
