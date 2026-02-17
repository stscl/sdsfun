# transforming a category tibble into the corresponding dummy variable tibble

transforming a category tibble into the corresponding dummy variable
tibble

## Usage

``` r
dummy_tbl(tbl)
```

## Arguments

- tbl:

  A `tibble` or `data.frame`.

## Value

A `tibble`

## Examples

``` r
a = tibble::tibble(x = 1:3,y = 4:6)
dummy_tbl(a)
#> # A tibble: 3 × 4
#>     x_1   x_2   y_1   y_2
#>   <int> <int> <int> <int>
#> 1     1     0     1     0
#> 2     0     1     0     1
#> 3     0     0     0     0
```
