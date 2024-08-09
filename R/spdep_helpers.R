#' @title constructs spatial weight matrices based on contiguity
#' @description
#' Constructs spatial weight matrices based on contiguity via `spdep` package.
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

spdep_distance_swm = \(){}
