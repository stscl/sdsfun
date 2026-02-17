# get variable names in a formula and data

get variable names in a formula and data

## Usage

``` r
formula_varname(formula, data)
```

## Arguments

- formula:

  A formula.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

## Value

A list.

- `yname`:

  Independent variable name

- `xname`:

  Dependent variable names

## Examples

``` r
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
formula_varname(PS_Score ~ EL_Score + OH_Score, gzma)
#> $yname
#> [1] "PS_Score"
#> 
#> $xname
#> [1] "EL_Score" "OH_Score"
#> 
formula_varname(PS_Score ~ ., gzma)
#> $yname
#> [1] "PS_Score"
#> 
#> $xname
#> [1] "EL_Score" "OH_Score" "IL_Score"
#> 
```
