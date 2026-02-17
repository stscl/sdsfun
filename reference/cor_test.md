# (partial) correlation test

(partial) correlation test

## Usage

``` r
cor_test(x, y, z = NULL, level = 0.05)
```

## Arguments

- x:

  A numeric vector representing the first variable.

- y:

  A numeric vector representing the second variable.

- z:

  An optional numeric vector or matrix of control variables. If
  provided, partial correlation is computed.

- level:

  (optional) Significance level. Default is 0.05.

## Value

A numeric vector

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
cor_test(gzma$PS_Score,gzma$EL_Score)
#>               correlation              significance upper confidence interval 
#>              3.644956e-01              4.953858e-05              5.115509e-01 
#> lower confidence interval 
#>              1.966957e-01 
cor_test(gzma$PS_Score,gzma$EL_Score,gzma$OH_Score)
#>               correlation              significance upper confidence interval 
#>               -0.05926374                1.00000000                0.12359895 
#> lower confidence interval 
#>               -0.23823372 
```
