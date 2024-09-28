#' construct neighbours list
#' @note
#' When `k` is set to a positive value, using K-Nearest Neighbor
#'
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
#' @param queen (optional) if `TRUE`, using queen contiguity, otherwise rook contiguity.
#' Default is `TRUE`.
#' @param k (optional) The number of nearest neighbours. Ignore this parameter when not
#' using distance based neighbours.
#' @param order (optional) The order of the adjacency object. Default is `1`.
#' @param cumulate (optional) Whether to accumulate adjacency objects. Default is `TRUE`.
#'
#' @return A neighbours list with class `nb`
#' @export
#'
#' @examples
#' library(sf)
#' pts = read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#'
#' nb1 = spdep_nb(pts, k = 6)
#' nb2 = spdep_nb(pts, queen = TRUE)
#' nb3 = spdep_nb(pts, queen = FALSE, order = 2)
#'
spdep_nb = \(sfj,
             queen = TRUE,
             k = NULL,
             order = 1L,
             cumulate = TRUE){
  if (is.null(k)){
    spnb = .spdep_polynb(sfj,queen,order,cumulate)
  } else {
    spnb = .spdep_knnnb(sfj,k)
  }
  return(spnb)
}

#' @title constructs spatial weight matrices based on contiguity
#' @description
#' Constructs spatial weight matrices based on contiguity via `spdep` package.
#' @note
#' When `k` is set to a positive value, using K-Nearest Neighbor Weights.
#'
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
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
#' gzma = read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#'
#' wt1 = spdep_contiguity_swm(gzma, k = 6, style = 'B')
#' wt2 = spdep_contiguity_swm(gzma, queen = TRUE, style = 'B')
#' wt3 = spdep_contiguity_swm(gzma, queen = FALSE, order = 2, style = 'B')
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
#' @details
#'
#' five different kernel weight functions:
#' - uniform:
#' \eqn{K_{(z)} = 1/2},for \eqn{\lvert z \rvert < 1}
#'
#' - triangular
#' \eqn{K_{(z)} = 1 - \lvert z \rvert},for \eqn{\lvert z \rvert < 1}
#'
#' - quadratic (epanechnikov)
#' \eqn{K_{(z)} = \frac{3}{4} \left( 1 - z^2 \right)},for \eqn{\lvert z \rvert < 1}
#'
#' - quartic
#' \eqn{K_{(z)} = \frac{15}{16} {\left( 1 - z^2 \right)}^2},for \eqn{\lvert z \rvert < 1}
#'
#' - gaussian
#' \eqn{K_{(z)} = \frac{1}{\sqrt{2 \pi}} e^{- \frac{z^2}{2}}}
#'
#' For the equation above, \eqn{z = d_{ij} / h_i}
#' where \eqn{h_i} is the bandwidth
#'
#' @note
#' When `kernel` is setting, using distance weight based on kernel function, Otherwise
#' the inverse distance weight will be used.
#'
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
#' @param kernel (optional) The kernel function, can be one of `uniform`,
#' `triangular`,`quadratic`(`epanechnikov`),`quartic` and `gaussian`. Default is `NULL`.
#' @param k (optional) The number of nearest neighbours. Default is `NULL`. Only useful
#' when `kernel` is provided.
#' @param bandwidth (optional) The bandwidth, default is `NULL`. When the spatial reference
#' of sf object is the geographical coordinate system, the unit of `bandwidth` is `km`. The
#' unit used in the projection coordinate system are consistent with those used in the sf
#' object coordinate system.
#' @param power (optional) Default is `1`. Useful when `kernel` is not provided.
#' @param style (optional) `style` can take values `W`, `B`, `C`, and `S`. More to see
#' `spdep::nb2mat()`. Default is `W`. For spatial weights based on distance functions,
#' a style of `B` means using the original value of the calculated distance function.
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
#' wt3 = spdep_distance_swm(pts, k = 3, kernel = 'gaussian')
#' wt4 = spdep_distance_swm(pts, k = 3, kernel = 'gaussian', bandwidth = 10000)
#'
spdep_distance_swm = \(sfj,
                       kernel = NULL,
                       k = NULL,
                       bandwidth = NULL,
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

#' @title spatial c(k)luster analysis by tree edge removal
#' @description
#' SKATER forms clusters by spatially partitioning data that has similar values for features of interest.
#'
#' @param sfj An `sf` object of observation data. Please ensure that the attribute columns are included
#' in the SKATER analysis.
#' @param k (optional) The number of clusters. Default is `6`.
#' @param nb (optional) A neighbours list with class nb. If the input `nb` is NULL, it will be constructed
#' automatically using `spdep_nb()`.
#' @param ini (optional) The initial node in the minimal spanning tree. Defaul is `5`.
#' @param ... (optional) Other parameters passed to spdep::skater().
#'
#' @return A numeric vector of clusters.
#' @export
#'
#' @examples
#' library(sf)
#' gzma = read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' gzma_c = spdep_skater(gzma,8)
#' gzma$group = gzma_c
#' plot(gzma["group"])
#'
spdep_skater = \(sfj,
                 k = 6,
                 nb = NULL,
                 ini = 5,
                 ...){
  .check_spwt(sfj)

  if (is.null(nb)) {
    nb = spdep_nb(sfj,queen = TRUE)
  }

  dpad = as.matrix(sf::st_drop_geometry(sfj))
  dpad = apply(dpad,2,normalize_vector)
  lcosts = spdep::nbcosts(nb, dpad)
  nbw = spdep::nb2listw(nb, glist = lcosts, style = "B")
  mst = spdep::mstree(nbw,ini)
  res = spdep::skater(mst[,1:2], dpad, k-1, ...)

  return(res$groups)
}
