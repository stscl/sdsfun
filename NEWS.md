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
