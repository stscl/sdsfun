# spatial variance

spatial variance

## Usage

``` r
spvar(x, wt, method = c("cpp", "r"))
```

## Arguments

- x:

  A numerical vector .

- wt:

  The spatial weight matrix.

- method:

  (optional) The method for calculating spatial variance, which can be
  chosen as either `cpp` or `r`. Default is `cpp`.

## Value

A numerical value.

## Details

The spatial variance formula is \\\Gamma = \frac{\sum_i \sum\_{j \neq i}
\omega\_{ij}\frac{(y_i-y_j)^2}{2}}{\sum_i \sum\_{j \neq i}
\omega\_{ij}}\\

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
wt1 = inverse_distance_swm(gzma)
spvar(gzma$PS_Score,wt1)
#> [1] 2.406486
```
