# Changelog

## sdsfun 0.9.0

## sdsfun 0.8.1

CRAN release: 2025-09-22

- Loading `sdsfun` no longer initializes the RNG state
  ([\#59](https://github.com/stscl/sdsfun/issues/59)).

- Fix misclassification of matrix inputs as vectors in
  [`cor_test()`](https://stscl.github.io/sdsfun/reference/cor_test.md)
  ([\#58](https://github.com/stscl/sdsfun/issues/58)).

- Fix a deprecation warning emitted by Armadillo
  ([\#55](https://github.com/stscl/sdsfun/issues/55)).

## sdsfun 0.8.0

CRAN release: 2025-05-12

- Add `cor_test` for unified (partial) correlation testing
  ([\#49](https://github.com/stscl/sdsfun/issues/49)).

- Implement head/tails breaking method in `discretize_vector` function
  ([\#42](https://github.com/stscl/sdsfun/issues/42)).

## sdsfun 0.7.0

CRAN release: 2025-01-13

- Handling NA Values Specifically for
  [`rm_lineartrend()`](https://stscl.github.io/sdsfun/reference/rm_lineartrend.md)
  ([\#29](https://github.com/stscl/sdsfun/issues/29)).

## sdsfun 0.6.0

CRAN release: 2024-12-11

- Add support for removing variable linear trend based on covariate
  ([\#23](https://github.com/stscl/sdsfun/issues/23)).

- Adds support for converting long-form tables into matrices
  corresponding to two-dimensional spatial positions
  ([\#20](https://github.com/stscl/sdsfun/issues/20)).

- The aggregation method for hierarchical clustering in
  [`hclustgeo_disc()`](https://stscl.github.io/sdsfun/reference/hclustgeo_disc.md)
  is set to `ward.D2` by default
  ([\#18](https://github.com/stscl/sdsfun/issues/18)).

## sdsfun 0.5.0

CRAN release: 2024-11-25

- Add `hclustgeo_disc` for hierarchical clustering with spatial soft
  constraints ([\#13](https://github.com/stscl/sdsfun/issues/13)).

- Add `spade_psd` for the fast estimation of the SPADE model psd-value.

- Add
  [`sf_gk_proj_cgcs2000()`](https://stscl.github.io/sdsfun/reference/sf_gk_proj_cgcs2000.md)
  function.

## sdsfun 0.4.3

CRAN release: 2024-11-15

- Add `geodetector_q` for the fast estimation of the geodetector
  q-value.

## sdsfun 0.4.2

CRAN release: 2024-11-11

- Add `check_tbl_na` and `tbl_all2int` functions to better support the
  `gdverse` and `sesp` packages.

## sdsfun 0.4.1

CRAN release: 2024-10-16

- Fix the bug that causes R to crash when the input `x` is a `factor` or
  `character` in the `discretize_vector` function
  ([\#12](https://github.com/stscl/sdsfun/issues/12)).

- Implement the `formula_varname` function to return the corresponding
  variable names from both `formula` and `data`.

## sdsfun 0.4.0

CRAN release: 2024-10-08

- Improve the handling of `n`-level categorical variables by generating
  `n-1` dummy variables
  ([\#2](https://github.com/stscl/sdsfun/issues/2)).

- Add the `spdep_lmtest` function for spatial linear model selection
  ([\#3](https://github.com/stscl/sdsfun/issues/3)).

- Migrate the `moran_test` function from the `geocomplexity` package to
  `sdsfun` ([\#4](https://github.com/stscl/sdsfun/issues/4)).

- Implement the geographical detector’s factor detector in `ssh_test`
  using `Rcpp` to enhance performance
  ([\#5](https://github.com/stscl/sdsfun/issues/5)).

- Introduce the `discretize_vector` function to support variable
  discretization ([\#6](https://github.com/stscl/sdsfun/issues/6)).

- Apply the `loess_optnum` function to select the optimal number of
  discretization intervals
  ([\#10](https://github.com/stscl/sdsfun/issues/10)).

- Implement spatial variance calculation in the `spvar` function, with
  support for both `R` and `C++` implementations
  ([\#11](https://github.com/stscl/sdsfun/issues/11)).

- Rename `dummy_vector` to `dummy_vec` for consistency in naming
  conventions.

- Add the `sf_coordinates` function to extract coordinates from `sf`
  objects.

## sdsfun 0.3.0

CRAN release: 2024-09-30

- Begin to use `Rcpp` to complete some tasks with high computational
  complexity.

- Add `dummy_vector` and `dummy_tbl` functions for dummy variables
  generation.

- Add `spdep_nb` function for neighbours list generation.

- Add `spdep_skater` function for spatial constrained clustering.

- Add `fuzzyoverlay` function for spatial fuzzy overlay.

- Update package extdata; now `sdsfun` has two extdata: `gzma.gpkg` and
  `pts.gpkg`.

## sdsfun 0.2.1

CRAN release: 2024-09-23

- Enhance support for other types of spatial vector data.

- Inhibit the warning information produced by `spdep`.

- Add
  [`sf_utm_proj_wgs84()`](https://stscl.github.io/sdsfun/reference/sf_utm_proj_wgs84.md)
  function.

## sdsfun 0.2.0

CRAN release: 2024-09-15

- Add
  [`sf_distance_matrix()`](https://stscl.github.io/sdsfun/reference/sf_distance_matrix.md)
  function.

## sdsfun 0.1.1

CRAN release: 2024-09-08

- Update the function documentation for
  [`spdep_contiguity_swm()`](https://stscl.github.io/sdsfun/reference/spdep_contiguity_swm.md)
  and
  [`spdep_distance_swm()`](https://stscl.github.io/sdsfun/reference/spdep_distance_swm.md).

- Move the source code from `SpatLyu/sdsfun` on GitHub to
  `stscl/sdsfun`.

## sdsfun 0.1.0

CRAN release: 2024-08-30

- Initial CRAN submission.
