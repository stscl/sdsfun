# generate subsets of a set

generate subsets of a set

## Usage

``` r
generate_subsets(set, empty = TRUE, self = TRUE)
```

## Arguments

- set:

  A vector.

- empty:

  (optional) When `empty` is `TRUE`, the generated subset includes the
  empty set, otherwise the empty set is removed. Default is `TRUE`.

- self:

  (optional) When `self` is `TRUE`, the resulting subset includes the
  set itself, otherwise the set itself is removed. Default is `TRUE`.

## Value

A list.

## Examples

``` r
generate_subsets(letters[1:3])
#> [[1]]
#> NULL
#> 
#> [[2]]
#> [1] "a"
#> 
#> [[3]]
#> [1] "b"
#> 
#> [[4]]
#> [1] "c"
#> 
#> [[5]]
#> [1] "a" "b"
#> 
#> [[6]]
#> [1] "a" "c"
#> 
#> [[7]]
#> [1] "b" "c"
#> 
#> [[8]]
#> [1] "a" "b" "c"
#> 
generate_subsets(letters[1:3],empty = FALSE)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
#> 
#> [[4]]
#> [1] "a" "b"
#> 
#> [[5]]
#> [1] "a" "c"
#> 
#> [[6]]
#> [1] "b" "c"
#> 
#> [[7]]
#> [1] "a" "b" "c"
#> 
generate_subsets(letters[1:3],self = FALSE)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> [1] "a"
#> 
#> [[3]]
#> [1] "b"
#> 
#> [[4]]
#> [1] "c"
#> 
#> [[5]]
#> [1] "a" "b"
#> 
#> [[6]]
#> [1] "a" "c"
#> 
#> [[7]]
#> [1] "b" "c"
#> 
generate_subsets(letters[1:3],empty = FALSE,self = FALSE)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
#> 
#> [[4]]
#> [1] "a" "b"
#> 
#> [[5]]
#> [1] "a" "c"
#> 
#> [[6]]
#> [1] "b" "c"
#> 
```
