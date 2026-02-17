# construct neighbours list

construct neighbours list

## Usage

``` r
spdep_nb(sfj, queen = TRUE, k = NULL, order = 1L, cumulate = TRUE)
```

## Arguments

- sfj:

  An `sf` object or can be converted to `sf` by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

- queen:

  (optional) if `TRUE`, using queen contiguity, otherwise rook
  contiguity. Default is `TRUE`.

- k:

  (optional) The number of nearest neighbours. Ignore this parameter
  when not using distance based neighbours.

- order:

  (optional) The order of the adjacency object. Default is `1`.

- cumulate:

  (optional) Whether to accumulate adjacency objects. Default is `TRUE`.

## Value

A neighbours list with class `nb`

## Note

When `k` is set to a positive value, using K-Nearest Neighbor

## Examples

``` r
pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
nb1 = spdep_nb(pts, k = 6)
nb2 = spdep_nb(pts, queen = TRUE)
nb3 = spdep_nb(pts, queen = FALSE, order = 2)
```
