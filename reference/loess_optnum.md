# determine optimal spatial data discretization for individual variables

Function for determining optimal spatial data discretization for
individual variables based on locally estimated scatterplot smoothing
(LOESS) model.

## Usage

``` r
loess_optnum(qvec, discnumvec, increase_rate = 0.05)
```

## Arguments

- qvec:

  A numeric vector of q statistics.

- discnumvec:

  A numeric vector of break numbers corresponding to `qvec`.

- increase_rate:

  (optional) The critical increase rate of the number of discretization.
  Default is `0.05`.

## Value

A two element numeric vector.

- `discnum`:

  optimal number of spatial data discretization

- `increase_rate`:

  the critical increase rate of the number of discretization

## Note

When `increase_rate` is not satisfied by the calculation, the discrete
number corresponding to the highest `q statistic` is selected as a
return.

Note that `sdsfun` sorts `discnumvec` from smallest to largest and keeps
`qvec` in one-to-one correspondence with `discnumvec`.

## Examples

``` r
qv = c(0.26045642,0.64120405,0.43938704,0.95165535,0.46347836,
       0.25385338,0.78778726,0.95938330,0.83247885,0.09285196)
loess_optnum(qv,3:12)
#>       discnum increase_rate 
#>          6.00          0.05 
```
