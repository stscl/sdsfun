# spatial fuzzy overlay

spatial fuzzy overlay

## Usage

``` r
fuzzyoverlay(formula, data, method = "and")
```

## Arguments

- formula:

  A formula of spatial fuzzy overlay.

- data:

  A data.frame or tibble of discretized data.

- method:

  (optional) Overlay methods. When `method` is `and`, use `min` to do
  fuzzy overlay; and when `method` is `or`,use `max` to do fuzzy
  overlay. Default is `and`.

## Value

A numeric vector.

## Note

Independent variables in the `data` provided to `fuzzyoverlay()` must be
discretized variables, and dependent variable are continuous variable.

## Examples

``` r
set.seed(42)
sim = tibble::tibble(y = stats::runif(7,0,10),
                     x1 = c(1,rep(2,3),rep(3,3)),
                     x2 = c(rep(1,2),rep(2,2),rep(3,3)))
fo1 = fuzzyoverlay(y~x1+x2,data = sim, method = 'and')
fo1
#>      1      2      3      4      5      6      7 
#> "x1_1" "x1_2" "x2_2" "x2_2" "x1_3" "x1_3" "x1_3" 
fo2 = fuzzyoverlay(y~x1+x2,data = sim, method = 'or')
fo2
#>      1      2      3      4      5      6      7 
#> "x2_1" "x2_1" "x1_2" "x1_2" "x1_3" "x1_3" "x1_3" 
```
