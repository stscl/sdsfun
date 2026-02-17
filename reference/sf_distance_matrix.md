# generates distance matrix

Generates distance matrix for sf object

## Usage

``` r
sf_distance_matrix(sfj)
```

## Arguments

- sfj:

  An `sf` object or can be converted to `sf` by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

## Value

A matrix.

## Examples

``` r
pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
pts_distm = sf_distance_matrix(pts)
pts_distm[1:5,1:5]
#>           1        2         3        4        5
#> 1     0.000 17202.47  6045.079 26190.70 12708.31
#> 2 17202.473     0.00 13198.202 33465.33 25550.36
#> 3  6045.079 13198.20     0.000 23588.47 12693.66
#> 4 26190.701 33465.33 23588.471     0.00 14608.23
#> 5 12708.306 25550.36 12693.655 14608.23     0.00
```
