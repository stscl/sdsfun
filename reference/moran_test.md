# global spatial autocorrelation test

global spatial autocorrelation test

## Usage

``` r
moran_test(sfj, wt = NULL, alternative = "greater", symmetrize = FALSE)
```

## Arguments

- sfj:

  An `sf` object or can be converted to `sf` by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

- wt:

  (optional) Spatial weight matrix. Must be a `matrix` class. If `wt` is
  not provided, `sdsfun` will use a first-order queen adjacency binary
  matrix.

- alternative:

  (optional) Specification of alternative hypothesis as `greater`
  (default), `lower`, or `two.sided`.

- symmetrize:

  (optional) Whether or not to symmetrize the asymmetrical spatial
  weight matrix ***wt*** by: 1/2 \* (***wt*** + ***wt***'). Default is
  `FALSE`.

## Value

A list utilizing a `result` tibble to store the following information
for each variable:

- `MoranI`:

  observed value of the Moran coefficient

- `EI`:

  expected value of Moran's I

- `VarI`:

  variance of Moran's I (under normality)

- `ZI`:

  standardized Moran coefficient

- `PI`:

  *p*-value of the test statistic

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
moran_test(gzma)
#> ***                 global moran test                 
#> -------------------------------------------------------------------
#>  Variable     MoranI         EI         VarI      zI        pI     
#> ---------- ------------- ----------- ---------- ------- -----------
#>  PS_Score   0.504972***   -0.008547   0.003119   9.195   1.877e-20 
#> 
#>  EL_Score   0.351095***   -0.008547   0.003119   6.44    5.988e-11 
#> 
#>  OH_Score   0.593021***   -0.008547   0.003119   10.77   2.347e-27 
#> 
#>  IL_Score   0.483476***   -0.008547   0.003119   8.81    6.257e-19 
#> -------------------------------------------------------------------
#> 
```
