test_that("test whether spvar function works well", {
  gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
  wt1 = inverse_distance_swm(gzma)
  expect_equal(spvar(gzma$PS_Score,wt1, method = 'cpp'),
               spvar(gzma$PS_Score,wt1, method = 'r'))
})
