# construct inverse distance weight

Function for constructing inverse distance weight.

## Usage

``` r
inverse_distance_swm(sfj, power = 1, bandwidth = NULL)
```

## Arguments

- sfj:

  Vector object that can be converted to `sf` by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

- power:

  (optional) Default is 1. Set to 2 for gravity weights.

- bandwidth:

  (optional) When the distance is bigger than bandwidth, the
  corresponding part of the weight matrix is set to 0. Default is
  `NULL`, which means not use the bandwidth.

## Value

A inverse distance weight matrices with class of `matrix`.

## Details

The inverse distance weight formula is \\w\_{ij} = 1 / d\_{ij}^\alpha\\

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
pts = read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
wt = inverse_distance_swm(pts)
wt[1:5,1:5]
#>              1            2            3            4            5
#> 1 0.000000e+00 5.813118e-05 1.654238e-04 3.818149e-05 7.868869e-05
#> 2 5.813118e-05 0.000000e+00 7.576789e-05 2.988167e-05 3.913838e-05
#> 3 1.654238e-04 7.576789e-05 0.000000e+00 4.239359e-05 7.877952e-05
#> 4 3.818149e-05 2.988167e-05 4.239359e-05 0.000000e+00 6.845458e-05
#> 5 7.868869e-05 3.913838e-05 7.877952e-05 6.845458e-05 0.000000e+00
```
