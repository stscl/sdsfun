#' @title constructs spatial weight matrices based on contiguity
#' @description
#' Constructs spatial weight matrices based on contiguity via `spdep` package.
#' @note
#' When `k` is set to a positive value, using K-Nearest Neighbor Weights.
#'
#' @param sfj An sf object.
#' @param queen (optional) if `TRUE`, using queen contiguity, otherwise rook contiguity.
#' Default is `TRUE`.
#' @param k (optional) The number of nearest neighbours. Ignore this parameter when not
#' using distance based neighbours to construct spatial weight matrices.
#' @param order (optional) The order of the adjacency object. Default is `1`.
#' @param cumulate (optional) Whether to accumulate adjacency objects. Default is `TRUE`.
#' @param style (optional) `style` can take values `W`, `B`, `C`, and `S`. More to see
#' `spdep::nb2mat()`. Default is `W`.
#' @param zero.policy (optional)  if `FALSE` stop with error for any empty neighbour sets,
#' if `TRUE` permit the weights list to be formed with zero-length weights vectors. Default
#' is `TRUE`.
#'
#' @return A matrix
#' @export
#'
#' @examples
#' library(sf)
#' pts = read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#'
#' wt1 = spdep_contiguity_swm(pts, k = 6, style = 'B')
#' wt2 = spdep_contiguity_swm(pts, queen = TRUE, style = 'B')
#' wt2 = spdep_contiguity_swm(pts, queen = FALSE, order = 2, style = 'B')
#'
spdep_contiguity_swm = \(sfj,
                         queen = TRUE,
                         k = NULL,
                         order = 1L,
                         cumulate = TRUE,
                         style = 'W',
                         zero.policy = TRUE){
  if (is.null(k)){
    spdep_wt = .spwt_polygon_contiguity(sfj,queen,order,cumulate,
                                        style,zero.policy)
  } else {
    spdep_wt = .spwt_distance_contiguity(sfj,k,style,zero.policy)
  }
  return(spdep_wt)
}

#' @title constructs spatial weight matrices based on distance
#' @description
#' Constructs spatial weight matrices based on distance via `spdep` package.
#' @note
#' When `kernel` is setting, using kernel function based distance weight.
#'
#' @param sfj An sf object.
#' @param bandwidth (optional) The bandwidth, default is `NULL`.
#' @param k (optional) The number of nearest neighbours. Default is `NULL`.
#' @param kernel (optional) The kernel function, can be one of `uniform`,
#' `triangular`,`quadratic`(`epanechnikov`),`quartic` and `gaussian`. Default is `NULL`.
#' @param power (optional) Default is `1`. Useful when `kernel` is not provided.
#' @param style (optional) `style` can take values `W`, `B`, `C`, and `S`. More to see
#' `spdep::nb2mat()`. Default is `W`.
#' @param zero.policy (optional)  if `FALSE` stop with error for any empty neighbour sets,
#' if `TRUE` permit the weights list to be formed with zero-length weights vectors. Default
#' is `TRUE`.
#'
#' @return A matrix
#' @export
#'
#' @examples
#' library(sf)
#' pts = read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#'
#' wt1 = spdep_distance_swm(pts, style = 'B')
#' wt2 = spdep_distance_swm(pts, kernel = 'gaussian')
#' wt2 = spdep_distance_swm(pts, k = 3, kernel = 'gaussian')
#'
spdep_distance_swm = \(sfj,
                       bandwidth = NULL,
                       k = NULL,
                       kernel = NULL,
                       power = 1,
                       style = 'W',
                       zero.policy = TRUE){
  if (is.null(kernel)){
    spdep_wt = .spwt_idw(sfj,bandwidth,power,style,zero.policy)
  } else {
    spdep_wt = .spwt_kernel_weight(sfj,bandwidth,k,kernel,style,zero.policy)
  }
  return(spdep_wt)
}
