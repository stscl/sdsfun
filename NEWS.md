# sdsfun 0.9.0

# sdsfun 0.8.0

* Add `cor_test` for unified (partial) correlation testing (#49).

* Implement head/tails breaking method in `discretize_vector` function (#42).

# sdsfun 0.7.0

* Handling NA Values Specifically for `rm_lineartrend()` (#29).

# sdsfun 0.6.0

* Add support for removing variable linear trend based on covariate (#23).

* Adds support for converting long-form tables into matrices corresponding to two-dimensional 
spatial positions (#20).

* The aggregation method for hierarchical clustering in `hclustgeo_disc()` is set 
to `ward.D2` by default (#18).

# sdsfun 0.5.0

* Add `hclustgeo_disc` for hierarchical clustering with spatial soft constraints (#13).

* Add `spade_psd` for the fast estimation of the SPADE model psd-value.

* Add `sf_gk_proj_cgcs2000()` function.

# sdsfun 0.4.3

* Add `geodetector_q` for the fast estimation of the geodetector q-value.

# sdsfun 0.4.2

* Add `check_tbl_na` and `tbl_all2int` functions to better support the `gdverse` and `sesp` packages.

# sdsfun 0.4.1

* Fix the bug that causes R to crash when the input `x` is a `factor` or `character` in the `discretize_vector` function (#12).

* Implement the `formula_varname` function to return the corresponding variable names from both `formula` and `data`.

# sdsfun 0.4.0

* Improve the handling of `n`-level categorical variables by generating `n-1` dummy variables (#2).

* Add the `spdep_lmtest` function for spatial linear model selection (#3).

* Migrate the `moran_test` function from the `geocomplexity` package to `sdsfun` (#4).

* Implement the geographical detector's factor detector in `ssh_test` using `Rcpp` to enhance performance (#5).

* Introduce the `discretize_vector` function to support variable discretization (#6).

* Apply the `loess_optnum` function to select the optimal number of discretization intervals (#10).

* Implement spatial variance calculation in the `spvar` function, with support for both `R` and `C++` implementations (#11).

* Rename `dummy_vector` to `dummy_vec` for consistency in naming conventions.

* Add the `sf_coordinates` function to extract coordinates from `sf` objects.

# sdsfun 0.3.0

* Begin to use `Rcpp` to complete some tasks with high computational complexity.

* Add `dummy_vector` and `dummy_tbl` functions for dummy variables generation.

* Add `spdep_nb` function for neighbours list generation.

* Add `spdep_skater` function for spatial constrained clustering.

* Add `fuzzyoverlay` function for spatial fuzzy overlay.

* Update package extdata; now `sdsfun` has two extdata: `gzma.gpkg` and `pts.gpkg`.

# sdsfun 0.2.1

* Enhance support for other types of spatial vector data.

* Inhibit the warning information produced by `spdep`.

* Add `sf_utm_proj_wgs84()` function.

# sdsfun 0.2.0

* Add `sf_distance_matrix()` function.

# sdsfun 0.1.1

* Update the function documentation for `spdep_contiguity_swm()` and `spdep_distance_swm()`.

* Move the source code from `SpatLyu/sdsfun` on GitHub to `stscl/sdsfun`.

# sdsfun 0.1.0

* Initial CRAN submission.
