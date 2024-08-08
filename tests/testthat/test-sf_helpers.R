test_that("sf_geometry_name works", {
  snnu = sf::read_sf(system.file('extdata/snnu.geojson',package = 'sdsfun'))
  expect_equal(sf_geometry_name(snnu), "geometry")
})

test_that("sf_geometry_type works", {
  snnu = sf::read_sf(system.file('extdata/snnu.geojson',package = 'sdsfun'))
  expect_equal(sf_geometry_type(snnu), "polygon")
})
