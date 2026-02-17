# transforming a categorical variable into dummy variables

transforming a categorical variable into dummy variables

## Usage

``` r
dummy_vec(x)
```

## Arguments

- x:

  An integer vector or can be converted into an integer vector.

## Value

A matrix.

## Examples

``` r
dummy_vec(c(1,1,3,2,4,6))
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    1    0    0    0
#> [3,]    0    1    0    0
#> [4,]    0    0    1    0
#> [5,]    0    0    0    1
#> [6,]    0    0    0    0
```
