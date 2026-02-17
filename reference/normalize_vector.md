# normalization

normalization

## Usage

``` r
normalize_vector(x, to_left = 0, to_right = 1)
```

## Arguments

- x:

  A continuous numeric vector.

- to_left:

  (optional) Specified minimum. Default is `0`.

- to_right:

  (optional) Specified maximum. Default is `1`.

## Value

A continuous vector which has normalized.

## Examples

``` r
normalize_vector(c(-5,1,5,0.01,0.99))
#> [1] 0.000 0.600 1.000 0.501 0.599
```
