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

#' @title transforming a category tibble into the corresponding dummy variable tibble
#'
#' @param tbl A `tibble` or `data.frame`.
#'
#' @return A `tibble`
#' @export
#'
#' @examples
#' a = tibble::tibble(x = 1:3,y = 4:6)
#' dummy_tbl(a)
#'
dummy_tbl = \(tbl){
  new_tblname = purrr::map2(tbl,
                            colnames(tbl),
                            \(.tbl,.tblname) paste0(.tblname, "_",
                                                    seq_along(Runique(.tbl)[-1])))
  dummytbl = DummyMat(as.matrix(tbl))
  colnames(dummytbl) = unlist(new_tblname)
  return(tibble::as_tibble(dummytbl))
}
