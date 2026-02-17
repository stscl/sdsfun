# generates cgcs2000 Gauss-Kruger projection epsg coding character

Generates a Gauss-Kruger projection epsg coding character corresponding
to an `sfj` object under the CGCS2000 spatial reference.

## Usage

``` r
sf_gk_proj_cgcs2000(sfj, degree = 6L)
```

## Arguments

- sfj:

  An `sf` object or can be converted to `sf` by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

- degree:

  (optional) `3`-degree or `6`-degree zonal projection, default is `6L`.

## Value

A character.

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun')) |>
  sf::st_transform(4490)
sf_gk_proj_cgcs2000(gzma,3)
#> [1] "EPSG:4547"
sf_gk_proj_cgcs2000(gzma,6)
#> [1] "EPSG:4508"
```
