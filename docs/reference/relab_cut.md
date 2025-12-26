# relab_cut

Function to recode the default labels of a factor created by cut() from
base R

## Usage

``` r
relab_cut(vec, suffix = NULL, right = TRUE, lang = "fr")
```

## Arguments

- vec:

  The vector to be recoded

- suffix:

  The suffix to be indicated after the values

- right:

  TRUE if categories have been created with parameter right = TRUE in
  cut().

- lang:

  The language of new labels

## Value

A vector

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
