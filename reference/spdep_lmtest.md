# spatial linear models selection

spatial linear models selection

## Usage

``` r
spdep_lmtest(formula, data, listw = NULL)
```

## Arguments

- formula:

  A formula for linear regression model.

- data:

  An `sf` object of observation data.

- listw:

  (optional) A listw. See
  [`spdep::mat2listw()`](https://r-spatial.github.io/spdep/reference/mat2listw.html)
  and
  [`spdep::nb2listw()`](https://r-spatial.github.io/spdep/reference/nb2listw.html)
  for details.

## Value

A list

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
spdep_lmtest(PS_Score ~ ., gzma)
#>  Rao's score (a.k.a Lagrange multiplier) diagnostics for spatial
#>  dependence
#> data:  
#> model: stats::lm(formula = formula, data = data)
#> test weights: listw
#>  
#>          statistic parameter   p.value    
#> RSerr      66.8965         1 3.331e-16 ***
#> RSlag      59.1366         1 1.465e-14 ***
#> adjRSerr    8.2199         1  0.004143 ** 
#> adjRSlag    0.4600         1  0.497622    
#> SARMA      67.3565         2 2.331e-15 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
