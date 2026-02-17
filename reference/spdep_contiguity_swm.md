# constructs spatial weight matrices based on contiguity

Constructs spatial weight matrices based on contiguity via `spdep`
package.

## Usage

``` r
spdep_contiguity_swm(
  sfj,
  queen = TRUE,
  k = NULL,
  order = 1L,
  cumulate = TRUE,
  style = "W",
  zero.policy = TRUE
)
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
  when not using distance based neighbours to construct spatial weight
  matrices.

- order:

  (optional) The order of the adjacency object. Default is `1`.

- cumulate:

  (optional) Whether to accumulate adjacency objects. Default is `TRUE`.

- style:

  (optional) `style` can take values `W`, `B`, `C`, and `S`. More to see
  [`spdep::nb2mat()`](https://r-spatial.github.io/spdep/reference/nb2mat.html).
  Default is `W`.

- zero.policy:

  (optional) if `FALSE` stop with error for any empty neighbour sets, if
  `TRUE` permit the weights list to be formed with zero-length weights
  vectors. Default is `TRUE`.

## Value

A matrix

## Note

When `k` is set to a positive value, using K-Nearest Neighbor Weights.

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
wt1 = spdep_contiguity_swm(gzma, k = 6, style = 'B')
wt2 = spdep_contiguity_swm(gzma, queen = TRUE, style = 'B')
wt3 = spdep_contiguity_swm(gzma, queen = FALSE, order = 2, style = 'B')
```
