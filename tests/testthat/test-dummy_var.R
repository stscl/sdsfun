test_that("test dummy_vector works", {
  expect_equal(dummy_vector(c(1,2,4,6)),
               diag(1,4,4))
})

test_that("test dummy_tbl works", {
  a = tibble::tibble(x = 1:2,y = 3:4)
  b = diag(1,2,2)
  b = cbind(b,b)
  colnames(b) = c(paste0("x_",1:2),paste0("y_",3:4))
  expect_equal(dummy_tbl(a),
               tibble::as_tibble(b))
})
