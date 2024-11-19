#' @title discretization
#'
#' @param x A continuous numeric vector.
#' @param n (optional) The number of discretized classes.
#' @param method (optional) The method of discretization, default is `natural`.
#' @param breakpoint (optional) Break points for manually splitting data. When
#' `method` is `manual`, `breakpoint` is required.
#' @param sampleprob (optional) When the data size exceeds `3000`, perform sampling
#' for discretization, applicable only to natural breaks. Default is `0.15`.
#' @param seed (optional) Random seed number, default is `123456789`.
#'
#' @return A discretized integer vector
#' @export
#'
#' @examples
#' xvar = c(22361, 9573, 4836, 5309, 10384, 4359, 11016, 4414, 3327, 3408,
#'          17816, 6909, 6936, 7990, 3758, 3569, 21965, 3605, 2181, 1892,
#'          2459, 2934, 6399, 8578, 8537, 4840, 12132, 3734, 4372, 9073,
#'          7508, 5203)
#' discretize_vector(xvar, n = 5, method = 'natural')
#'
discretize_vector = \(x, n, method = 'natural',
                      breakpoint = NULL,
                      sampleprob = 0.15,
                      seed = 123456789){
  if (any(inherits(x,'factor'),inherits(x,'character'))){
    return(as.integer(as.factor(x)))
  }

  if (method %in% c("sd","equal","geometric","quantile")){
    res = eval(parse(text = paste0(method,"Disc(x,n)")))
  } else if (method == "manual") {
    if (is.null(breakpoint)) {
      stop("When method is manual, breakpoint is required.")
    } else {
      res = manualDisc(x,breakpoint)
    }
  } else if (method == "natural") {
    base::set.seed(seed)
    res = naturalDisc(x,n,sampleprob)
  } else {
    stop("Only support those methods: sd,equal,quantile,geometric,natural and manual.")
  }
  return(res)
}

#' hierarchical clustering with spatial soft constraints
#'
#' @param sfj An `sf` object.
#' @param n The number of hierarchical clustering classes, which can be a numeric value or vector.
#' @param alpha (optional) A real value between 0 and 1. This mixing parameter gives the relative
#' importance of "feature"  space and constraint" space.  Default is `0.5`.
#' @param scale (optional) Whether to scaled the dissimilarities matrix, default is `TRUE`.
#' @param wt (optional) Vector with the weights of the observations. By default, `wt` is `NULL`.
#' @param ... (optional) Other arguments passed to `stats::dist()`.
#'
#' @return A vector with group memberships if n are scalar, otherwise a matrix with group memberships
#' is returned where each column corresponds to the elements of n, respectively.
#' @export
#'
#' @examples
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' gzma$group = hclustgeo_disc(gzma,5)
#' plot(gzma["group"])
#'
hclustgeo_disc = \(sfj, n, alpha = 0.5, scale = TRUE, wt = NULL, ...){
  if (alpha == 0) {
    D1 = matrix(0,nrow = nrow(sfj),ncol = nrow(sfj))
  } else if (inherits(sfj,"sf")){
    D1 = sdsfun::sf_distance_matrix(sfj)
    sfj = sf::st_drop_geometry(sfj)
  } else {
    stop("sfj should be an sf object if alpha is not 0 !")
  }
  D0 = as.matrix(stats::dist(as.matrix(sfj),...))
  deltadist = stats::as.dist(RcppHClustGeoMat(D0,D1,alpha,scale,wt))
  resh = stats::hclust(deltadist,method="ward.D",members=wt)
  return(stats::cutree(resh,k = n))
}
