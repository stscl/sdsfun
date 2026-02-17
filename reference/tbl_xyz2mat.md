# convert xyz tbl to matrix

convert xyz tbl to matrix

## Usage

``` r
tbl_xyz2mat(tbl, x = 1, y = 2, z = 3)
```

## Arguments

- tbl:

  A `tibble`,`data.frame` or `sf` object.

- x:

  (optional) The x-axis coordinates column number, default is `1`.

- y:

  (optional) The y-axis coordinates column number, default is `2`.

- z:

  (optional) The z (attribute) coordinates column number, default is
  `3`.

## Value

A `list`.

- z_attrs_matrix:

  A matrix with attribute information.

- x_coords_matrix:

  A matrix with the x-axis coordinates.

- y_coords_matrix:

  A matrix with the y-axis coordinates.

## Examples

``` r
set.seed(42)
lon = rep(1:3,each = 3)
lat = rep(1:3,times = 3)
zattr = rnorm(9, mean = 10, sd = 1)
demodf = data.frame(x = lon, y = lat, z = zattr)
demodf
#>   x y         z
#> 1 1 1 11.370958
#> 2 1 2  9.435302
#> 3 1 3 10.363128
#> 4 2 1 10.632863
#> 5 2 2 10.404268
#> 6 2 3  9.893875
#> 7 3 1 11.511522
#> 8 3 2  9.905341
#> 9 3 3 12.018424
tbl_xyz2mat(demodf)
#> $z_attrs_matrix
#>           [,1]      [,2]      [,3]
#> [1,] 10.363128  9.893875 12.018424
#> [2,]  9.435302 10.404268  9.905341
#> [3,] 11.370958 10.632863 11.511522
#> 
#> $x_coords_matrix
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    1    2    3
#> [3,]    1    2    3
#> 
#> $y_coords_matrix
#>      [,1] [,2] [,3]
#> [1,]    3    3    3
#> [2,]    2    2    2
#> [3,]    1    1    1
#> 
```
