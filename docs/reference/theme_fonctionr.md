# theme_fonctionr

A ggplot theme that is ready to use. It is used by most other functions,
but can also be applied to an external ggplot object.

## Usage

``` r
theme_fonctionr(
  font = "Roboto",
  theme = "fonctionr",
  display = "ggplot",
  grid.lines = "x",
  coef_font = 1
)
```

## Arguments

- font:

  Font used in the graphic. See
  [`load_and_active_fonts()`](https://jgires.github.io/fonctionr/reference/load_and_active_fonts.md)
  for available fonts.

- theme:

  The optionnal theme you want for the graphic. Available themes:
  `"fonctionr"` and `“IWEPS”`. Default is `NULL`.

- display:

  The way `theme_fonctionr()` works on the axis texts: like ggplot2 or
  ggtext.

- grid.lines:

  Specify major grid lines : `"x"`, `"y"` or `"both"`. Default is `"x"`.

- coef_font:

  A multiplier factor for font size.

## Value

No return value, called for side effects.
