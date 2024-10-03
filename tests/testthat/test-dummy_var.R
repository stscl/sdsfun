test_that("test dummy_vec works", {
  expect_equal(dummy_vec(c(1,2,4,6)),
               rbind(diag(1,3,3),rep(0,3)))
})

test_that("test dummy_tbl works", {
  a = tibble::tibble(x = 1:2,y = 3:4)
  b = matrix(c(1,0,1,0),ncol = 2)
  colnames(b) = paste0(c("x","y"),"_1")
  expect_equal(dummy_tbl(a),
               tibble::as_tibble(b))
})
