# only spade power of spatial determinant

only spade power of spatial determinant

## Usage

``` r
spade_psd(y, hs, wt)
```

## Arguments

- y:

  Dependent variable

- hs:

  Independent variable

- wt:

  Spatial weight matrix

## Value

A numeric value

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
wt1 = inverse_distance_swm(gzma)
spade_psd(y = gzma$PS_Score,
          hs = discretize_vector(gzma$PS_Score,5),
          wt = wt1)
#> [1] 0.9480239
```
