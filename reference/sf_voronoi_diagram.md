# generates voronoi diagram

Generates Voronoi diagram (Thiessen polygons) for sf object

## Usage

``` r
sf_voronoi_diagram(sfj)
```

## Arguments

- sfj:

  An `sf` object.

## Value

An `sf` object of polygon geometry type or can be converted to this by
[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

## Note

Only sf objects of (multi-)point type are supported to generate voronoi
diagram and the returned result includes only the geometry column.

## Examples

``` r
pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
pts_v = sf_voronoi_diagram(pts)

library(ggplot2)
ggplot() +
  geom_sf(data = pts_v, color = 'red',
          fill = 'transparent') +
  geom_sf(data = pts, color = 'blue', size = 1.25) +
  theme_void()

```
