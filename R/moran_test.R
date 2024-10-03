#' @title test global spatial autocorrelation
#' @description
#' Spatial autocorrelation test based on global moran index.
#' @note
#' This is a `C++` implementation of the `MI.vec` function in `spfilteR` package,
#' and embellishes the console output.
#'
#' The return result of this function is actually a `list`, please access the result
#' tibble using `$result`.
#'
#' The non-numeric columns of the attribute columns in `sfj` are ignored.
#'
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
#' @param wt (optional) Spatial weight matrix. Must be a `matrix` class. If `wt` is not
#' provided, `sdsfun` will use a first-order queen adjacency binary matrix.
#' @param alternative (optional) Specification of alternative hypothesis as `greater` (default),
#' `lower`, or `two.sided`.
#' @param symmetrize (optional) Whether or not to symmetrize the asymmetrical spatial weight matrix
#' \emph{\strong{wt}} by: 1/2 * (\emph{\strong{wt}} + \emph{\strong{wt}}'). Default is `FALSE`.
#'
#' @return A list with `moran_test` class and result stored on the `result` tibble.
#' Which contains the following information for each variable:
#' \describe{
#' \item{\code{MoranI}}{observed value of the Moran coefficient}
#' \item{\code{EI}}{expected value of Moran's I}
#' \item{\code{VarI}}{variance of Moran's I (under normality)}
#' \item{\code{ZI}}{standardized Moran coefficient}
#' \item{\code{PI}}{\emph{p}-value of the test statistic}
#' }
#'
#' @export
#'
#' @examples
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' moran_test(gzma)
#'
moran_test = \(sfj,
               wt = NULL,
               alternative = "greater",
               symmetrize = FALSE){
  .check_spwt(sfj)

  if (is.null(wt)){
    wt = sdsfun::spdep_contiguity_swm(sfj,
                                      queen = TRUE,
                                      style = 'W',
                                      zero.policy = TRUE)
  }

  if (!(alternative %in% c("greater", "less", "two.sided"))) {
    stop("Invalid input: `alternative` must be either `greater`, `less`, or `two.sided`")
  }

  dmat = sfj %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    as.matrix()
  mitres = MI_vec(dmat, wt, alternative, symmetrize)
  mitres = tibble::as_tibble(mitres) %>%
    dplyr::mutate(Variable = colnames(dmat)) %>%
    dplyr::select(dplyr::all_of("Variable"),
                  dplyr::everything())
  res = list(result = mitres)
  class(res) = 'moran_test'
  return(res)
}

#' print global moran test result
#' @export
#' @noRd
print.moran_test = \(x,...){
  cat("***                 global moran test                 ")
  x = PrintGlobalMoranI(as.data.frame(x$result))
  pander::pander(x,...)
}
