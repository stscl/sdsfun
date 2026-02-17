# sf object geometry column name

Get the geometry column name of an sf object

## Usage

``` r
sf_geometry_name(sfj)
```

## Arguments

- sfj:

  An `sf` object.

## Value

A character.

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
sf_geometry_name(gzma)
#> [1] "geom"
```
