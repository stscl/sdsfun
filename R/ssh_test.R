#' @title spatial stratified heterogeneity test
#'
#' @param y Variable Y, continuous numeric vector.
#' @param hs Spatial stratification or classification of each explanatory variable.
#' `factor`, `character`, `integer` or `data.frame`, `tibble` and `sf` object.
#'
#' @return A `tibble`
#' @export
#'
#' @examples
#' ssh_test(y = 1:7, hs = c('x',rep('y',3),rep('z',3)))
#'
ssh_test = \(y,hs){
  if (inherits(hs,"data.frame")) {
    if (inherits(hs,"sf")) {
      hs = sf::st_drop_geometry(hs)
    }
    hs = sdsfun::tbl_all2int(hs)
    qvs = purrr::map(hs,\(.h) GDFactorQ(y,.h))
    qv = purrr::map_dbl(qvs,\(.qv) .qv[[1]])
    pv = purrr::map_dbl(qvs,\(.qv) .qv[[2]])
    return(tibble::tibble(Variable = names(hs),
                          Qvalue = qv,
                          Pvalue = pv))
  } else {
    hs = as.integer(as.factor(hs))
    return(tibble::as_tibble(GDFactorQ(y,hs)))
  }
}


#' only geodetector q-value
#'
#' @param y Dependent variable
#' @param hs Independent variable
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' geodetector_q(y = 1:7, hs = c('x',rep('y',3),rep('z',3)))
#'
geodetector_q = \(y,hs){
  hs = as.integer(as.factor(hs))
  return(CalcFactorQ(y,hs))
}
