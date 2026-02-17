# convert discrete variables in a tibble to integers

convert discrete variables in a tibble to integers

## Usage

``` r
tbl_all2int(tbl)
```

## Arguments

- tbl:

  A `tibble`,`data.frame` or `sf` object.

## Value

A converted `tibble`,`data.frame` or `sf` object.

## Examples

``` r
demotbl = tibble::tibble(x = c(1,2,3,3,1),
                         y = letters[1:5],
                         z = c(1L,1L,2L,2L,3L),
                         m = factor(letters[1:5],levels = letters[5:1]))
tbl_all2int(demotbl)
#> # A tibble: 5 × 4
#>       x     y     z     m
#>   <int> <int> <int> <int>
#> 1     1     1     1     5
#> 2     2     2     1     4
#> 3     3     3     2     3
#> 4     3     4     2     2
#> 5     1     5     3     1
```
