# sf object geometry type

Get the geometry type of an sf object

## Usage

``` r
sf_geometry_type(sfj)
```

## Arguments

- sfj:

  An `sf` object.

## Value

A lowercase character vector

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
sf_geometry_type(gzma)
#> [1] "polygon"
```
