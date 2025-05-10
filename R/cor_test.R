#' (partial) correlation test
#'
#' @param x A numeric vector representing the first variable.
#' @param y A numeric vector representing the second variable.
#' @param z An optional numeric vector or matrix of control variables. If provided, partial correlation is computed.
#' @param level (optional) Significance level. Default is 0.05.
#'
#' @returns A numeric vector
#' @export
#'
#' @examples
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' cor_test(gzma$PS_Score,gzma$EL_Score)
#'
cor_test = \(x,y,z = NULL,level = 0.05){
  if (is.null(z)) {
    res = RcppPearsonCor(x,y,level)
  } else {
    if (is.atomic(z)) z = matrix(z,ncol = 1)
    if (!inherits(z,"matrix")) z = as.matrix(z)
    res = RcppPartialCor(x,y,z,level)
  }
  return(res)
}
