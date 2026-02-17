# generates wgs84 utm projection epsg coding character

Generates a utm projection epsg coding character corresponding to an
`sfj` object under the WGS84 spatial reference.

## Usage

``` r
sf_utm_proj_wgs84(sfj)
```

## Arguments

- sfj:

  An `sf` object or can be converted to `sf` by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

## Value

A character.

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
sf_utm_proj_wgs84(gzma)
#> [1] "EPSG:32649"
```
