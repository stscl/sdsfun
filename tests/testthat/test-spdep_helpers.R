test_that("check first order queen contiguity for polygon data in spdep_contiguity_swm",
          {
            gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
            wt = spdep_contiguity_swm(gzma, queen = TRUE, style = 'B')
            expect_equal(unname(wt[1,2]), 0)
          }
)

test_that("check first order queen contiguity for point data in spdep_contiguity_swm",
          {
            pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
            wt = spdep_contiguity_swm(pts, queen = TRUE, style = 'B')
            expect_equal(unname(wt[1,2]), 1)
          }
)

test_that("check high order queen contiguity for polygon data in spdep_contiguity_swm",
          {
            gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
            wt = spdep_contiguity_swm(gzma, queen = TRUE, order = 2, style = 'B')
            expect_equal(unname(wt[1,4]), 1)
          }
)

test_that("check knn neighbours contiguity for polygon data in spdep_contiguity_swm",
          {
            gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
            wt = spdep_contiguity_swm(gzma, k = 6, style = 'B')
            expect_equal(unname(wt[2,1]), 0)
          }
)

test_that("check knn neighbours contiguity for point data in spdep_contiguity_swm",
          {
            pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
            wt = spdep_contiguity_swm(pts, k = 6, style = 'B')
            expect_equal(unname(wt[2,7]), 1)
          }
)

test_that("check first order inverse distance weights in spdep_distance_swm",
          {
            pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
            wt1 = spdep_distance_swm(pts, style = 'B')
            wt2 = inverse_distance_swm(pts)
            expect_equal(unname(wt1[1,2]), wt2[1,2])
          }
)

test_that("check kernel functional distance weights in spdep_distance_swm",
          {
            pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
            wt = spdep_distance_swm(pts, kernel = 'gaussian', k = 6, style = 'B')
            expect_equal(unname(wt[1,2]), 0)
          }
)

test_that("check whether the bandwidth setting is valid in spdep_distance_swm",
          {
            pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
            wt1 = spdep_distance_swm(pts, bandwidth = 10000, style = 'B')
            wt2 = spdep_distance_swm(pts, bandwidth = 10000,
                                     kernel = 'gaussian', style = 'B')
            expect_equal(unname(wt1[1,2]),0)
            expect_equal(unname(wt2[1,2]),0)
          }
)
