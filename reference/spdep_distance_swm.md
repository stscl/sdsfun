# constructs spatial weight matrices based on distance

Constructs spatial weight matrices based on distance via `spdep`
package.

## Usage

``` r
spdep_distance_swm(
  sfj,
  kernel = NULL,
  k = NULL,
  bandwidth = NULL,
  power = 1,
  style = "W",
  zero.policy = TRUE
)
```

## Arguments

- sfj:

  An `sf` object or can be converted to `sf` by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

- kernel:

  (optional) The kernel function, can be one of `uniform`,
  `triangular`,`quadratic`(`epanechnikov`),`quartic` and `gaussian`.
  Default is `NULL`.

- k:

  (optional) The number of nearest neighbours. Default is `NULL`. Only
  useful when `kernel` is provided.

- bandwidth:

  (optional) The bandwidth, default is `NULL`. When the spatial
  reference of sf object is the geographical coordinate system, the unit
  of `bandwidth` is `km`. The unit used in the projection coordinate
  system are consistent with those used in the sf object coordinate
  system.

- power:

  (optional) Default is `1`. Useful when `kernel` is not provided.

- style:

  (optional) `style` can take values `W`, `B`, `C`, and `S`. More to see
  [`spdep::nb2mat()`](https://r-spatial.github.io/spdep/reference/nb2mat.html).
  Default is `W`. For spatial weights based on distance functions, a
  style of `B` means using the original value of the calculated distance
  function.

- zero.policy:

  (optional) if `FALSE` stop with error for any empty neighbour sets, if
  `TRUE` permit the weights list to be formed with zero-length weights
  vectors. Default is `TRUE`.

## Value

A matrix

## Details

five different kernel weight functions:

- uniform: \\K\_{(z)} = 1/2\\,for \\\lvert z \rvert \< 1\\

- triangular \\K\_{(z)} = 1 - \lvert z \rvert\\,for \\\lvert z \rvert \<
  1\\

- quadratic (epanechnikov) \\K\_{(z)} = \frac{3}{4} \left( 1 - z^2
  \right)\\,for \\\lvert z \rvert \< 1\\

- quartic \\K\_{(z)} = \frac{15}{16} {\left( 1 - z^2 \right)}^2\\,for
  \\\lvert z \rvert \< 1\\

- gaussian \\K\_{(z)} = \frac{1}{\sqrt{2 \pi}} e^{- \frac{z^2}{2}}\\

For the equation above, \\z = d\_{ij} / h_i\\ where \\h_i\\ is the
bandwidth

## Note

When `kernel` is setting, using distance weight based on kernel
function, Otherwise the inverse distance weight will be used.

## Examples

``` r
pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
wt1 = spdep_distance_swm(pts, style = 'B')
wt2 = spdep_distance_swm(pts, kernel = 'gaussian')
wt3 = spdep_distance_swm(pts, k = 3, kernel = 'gaussian')
wt4 = spdep_distance_swm(pts, k = 3, kernel = 'gaussian', bandwidth = 10000)
```
