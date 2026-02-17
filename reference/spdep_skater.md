# spatial c(k)luster analysis by tree edge removal

SKATER forms clusters by spatially partitioning data that has similar
values for features of interest.

## Usage

``` r
spdep_skater(sfj, k = 6, nb = NULL, ini = 5, ...)
```

## Arguments

- sfj:

  An `sf` object of observation data. Please ensure that the attribute
  columns are included in the SKATER analysis.

- k:

  (optional) The number of clusters. Default is `6`.

- nb:

  (optional) A neighbours list with class nb. If the input `nb` is NULL,
  it will be constructed automatically using
  [`spdep_nb()`](https://stscl.github.io/sdsfun/reference/spdep_nb.md).

- ini:

  (optional) The initial node in the minimal spanning tree. Defaul is
  `5`.

- ...:

  (optional) Other parameters passed to spdep::skater().

## Value

A numeric vector of clusters.

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
gzma_c = spdep_skater(gzma,8)
gzma$group = gzma_c
plot(gzma["group"])

```
