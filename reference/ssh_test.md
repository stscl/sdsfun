# spatial stratified heterogeneity test

spatial stratified heterogeneity test

## Usage

``` r
ssh_test(y, hs)
```

## Arguments

- y:

  Variable Y, continuous numeric vector.

- hs:

  Spatial stratification or classification of each explanatory variable.
  `factor`, `character`, `integer` or `data.frame`, `tibble` and `sf`
  object.

## Value

A `tibble`

## Examples

``` r
ssh_test(y = 1:7, hs = c('x',rep('y',3),rep('z',3)))
#> # A tibble: 1 × 2
#>   Qvalue Pvalue
#>    <dbl>  <dbl>
#> 1  0.857 0.0528
```
