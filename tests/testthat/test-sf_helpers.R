test_that("sf_geometry_name works", {
  gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
  expect_equal(sf_geometry_name(gzma), "geom")
})

test_that("sf_geometry_type works", {
  gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
  expect_equal(sf_geometry_type(gzma), "polygon")
})
