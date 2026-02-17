# standardization

To calculate the Z-score using variance normalization, the formula is as
follows:

\\Z = \frac{(x - mean(x))}{sd(x)}\\

## Usage

``` r
standardize_vector(x)
```

## Arguments

- x:

  A numeric vector

## Value

A standardized numeric vector

## Examples

``` r
standardize_vector(1:10)
#>  [1] -1.4863011 -1.1560120 -0.8257228 -0.4954337 -0.1651446  0.1651446
#>  [7]  0.4954337  0.8257228  1.1560120  1.4863011
```
