# discretization

discretization

## Usage

``` r
discretize_vector(
  x,
  n,
  method = "natural",
  breakpoint = NULL,
  sampleprob = 0.15,
  thr = 0.4,
  seed = 123456789
)
```

## Arguments

- x:

  A continuous numeric vector.

- n:

  (optional) The number of discretized classes.

- method:

  (optional) The method of discretization, default is `natural`.

- breakpoint:

  (optional) Break points for manually splitting data. When `method` is
  `manual`, `breakpoint` is required.

- sampleprob:

  (optional) When the data size exceeds `3000`, perform sampling for
  discretization, applicable only to natural breaks. Default is `0.15`.

- thr:

  (optional) Threshold for controlling iteration, applicable only to
  headtails breaks. Default is `0.4`.

- seed:

  (optional) Random seed number, default is `123456789`.

## Value

A discretized integer vector

## Examples

``` r
xvar = c(22361, 9573, 4836, 5309, 10384, 4359, 11016, 4414, 3327, 3408,
         17816, 6909, 6936, 7990, 3758, 3569, 21965, 3605, 2181, 1892,
         2459, 2934, 6399, 8578, 8537, 4840, 12132, 3734, 4372, 9073,
         7508, 5203)
discretize_vector(xvar, n = 5, method = 'natural')
#>  [1] 5 4 2 3 4 2 4 2 1 1 5 3 3 3 2 1 5 1 1 1 1 1 3 3 3 2 5 1 2 4 3 2
```
