#' @title transforming a categorical variable into dummy variables
#'
#' @param x An integer vector or can be converted into an integer vector.
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' dummy_vector(c(1,1,3,2,4,6))
#'
dummy_vector = \(x){
  x = as.integer(x)
  return(DummyVar(x))
}

# dummy_tbl = \(tbl){
#   new_tblname = purrr::map2_chr(tbl,
#                                 colnames(tbl),
#                                 \(.tbl,.tblname) paste0(.tblname,
#                                                         "_",
#                                                         Runique(.tbl)))
# }
