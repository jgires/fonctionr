# relab_cut

Function to recode the default labels of a factor created by cut() from
base R into more intuitive labels

## Usage

``` r
relab_cut(vec, suffix = NULL, right = TRUE, lang = "fr")
```

## Arguments

- vec:

  The vector to be recoded. It should be produced by cut(). Notice that
  the labels may not include scientific notation. Thus, in cut(),
  dig.lab argument should be high enough in order to produce labels
  without scientific notation.

- suffix:

  The suffix to be indicated after the values. Usualy, the unit of the
  variable will be used (e.g. euros, percents). Default is NULL, for no
  suffix.

- right:

  TRUE if categories have been created with parameter right = TRUE in
  cut(). FALSE if categories have been created with parameter right =
  FALSE in cut(). Default is TRUE.

- lang:

  Language of new labels. Possibilities are "fr" (french), "nl" (dutch)
  and "en" (english). Default is "fr".

## Value

A vector with new labels

## Examples

``` r
cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE) |>
table()
#> 
#> [0.001,201)   [201,401)   [401,600)   [600,800) [800,1e+03] 
#>         200         200         200         200         200 

cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE, dig.lab = 4) |>
relab_cut(suffix = "€", right = FALSE) |>
table()
#> 
#> Moins de 200.8€  200.8 - 400.5€  400.6 - 600.3€  600.4 - 800.1€  Plus de 800.1€ 
#>             200             200             200             200             200 
```
