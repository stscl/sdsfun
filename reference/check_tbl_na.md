# check for NA values in a tibble

check for NA values in a tibble

## Usage

``` r
check_tbl_na(tbl)
```

## Arguments

- tbl:

  A `tibble`

## Value

A logical value.

## Examples

``` r
demotbl = tibble::tibble(x = c(1,2,3,NA,1),
                         y = c(NA,NA,1:3),
                         z = 1:5)
demotbl
#> # A tibble: 5 × 3
#>       x     y     z
#>   <dbl> <int> <int>
#> 1     1    NA     1
#> 2     2    NA     2
#> 3     3     1     3
#> 4    NA     2     4
#> 5     1     3     5
check_tbl_na(demotbl)
#> [1] TRUE
```
