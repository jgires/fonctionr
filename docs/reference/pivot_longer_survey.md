# pivot_longer_survey

Function to pivot from wide to long agregated data by group produced
with srvyr::summarise

## Usage

``` r
pivot_longer_survey(data, n_groups)
```

## Arguments

- data:

  A dataframe with Agregated data to pivot

- n_groups:

  Number of groups by which data have been agregated

## Value

A dataframe

## Examples

``` r
# Loading data
data(eusilc, package = "laeken")

# Loading srvyr
library(srvyr)
#> 
#> Attaching package: 'srvyr'
#> The following object is masked from 'package:stats':
#> 
#>     filter

# Making srvyr object
eusilc_srvyr <- as_survey_design(eusilc, ids = db030, strata = db040, weights = rb050)

# computing srvyr result using summarise()
result_srvyr<-eusilc_srvyr %>%
 group_by(rb090,pb220a) %>% # by sex and nationality
   summarise(mean_eqIncome = survey_mean(eqIncome),mean_age =survey_mean(age))

# Showing the srvyr summirise output
result_srvyr
#> # A tibble: 8 × 6
#> # Groups:   rb090 [2]
#>   rb090  pb220a mean_eqIncome mean_eqIncome_se mean_age mean_age_se
#>   <fct>  <fct>          <dbl>            <dbl>    <dbl>       <dbl>
#> 1 male   AT            21721.             172.    46.0        0.247
#> 2 male   EU            22045.            1137.    42.4        1.37 
#> 3 male   Other         16962.             465.    38.2        0.781
#> 4 male   NA            17614.             265.     7.81       0.141
#> 5 female AT            19702.             155.    48.4        0.263
#> 6 female EU            19783.            1252.    44.2        1.41 
#> 7 female Other         16840.             447.    39.5        0.709
#> 8 female NA            17017.             281.     7.85       0.140

# Pivoting the out with pivot_longer_survey()
   pivoted_result <- pivot_longer_survey(result_srvyr, n_groups = 2)
#> Joining with `by = join_by(rb090, pb220a)`

# Output is pivoted
pivoted_result
#> # A tibble: 16 × 5
#>    rb090  pb220a type             value       se
#>    <fct>  <fct>  <chr>            <dbl>    <dbl>
#>  1 male   AT     mean_eqIncome 21721.    172.   
#>  2 male   AT     mean_age         46.0     0.247
#>  3 male   EU     mean_eqIncome 22045.   1137.   
#>  4 male   EU     mean_age         42.4     1.37 
#>  5 male   Other  mean_eqIncome 16962.    465.   
#>  6 male   Other  mean_age         38.2     0.781
#>  7 male   NA     mean_eqIncome 17614.    265.   
#>  8 male   NA     mean_age          7.81    0.141
#>  9 female AT     mean_eqIncome 19702.    155.   
#> 10 female AT     mean_age         48.4     0.263
#> 11 female EU     mean_eqIncome 19783.   1252.   
#> 12 female EU     mean_age         44.2     1.41 
#> 13 female Other  mean_eqIncome 16840.    447.   
#> 14 female Other  mean_age         39.5     0.709
#> 15 female NA     mean_eqIncome 17017.    281.   
#> 16 female NA     mean_age          7.85    0.140
```
