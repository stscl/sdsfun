test_that("check first order queen contiguity for polygon data in spdep_contiguity_swm",
          {
            henan = sf::read_sf(system.file('extdata/henan.geojson',package = 'sdsfun'))
            wt = spdep_contiguity_swm(henan, queen = TRUE, style = 'B')
            expect_equal(unname(wt[1,2]), 1)
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
            henan = sf::read_sf(system.file('extdata/henan.geojson',package = 'sdsfun'))
            wt = spdep_contiguity_swm(henan, queen = TRUE, order = 2, style = 'B')
            expect_equal(unname(wt[1,4]), 1)
          }
)

test_that("check knn neighbours contiguity for polygon data in spdep_contiguity_swm",
          {
            henan = sf::read_sf(system.file('extdata/henan.geojson',package = 'sdsfun'))
            wt = spdep_contiguity_swm(henan, k = 6, style = 'B')
            expect_equal(unname(wt[2,1]), 1)
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
            wt = spdep_distance_swm(pts, style = 'B')
            wt2 = inverse_distance_swm(pts)
            expect_equal(unname(wt[1,2]), wt2[1,2])
          }
)
