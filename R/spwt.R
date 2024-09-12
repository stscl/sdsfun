#' check sf object geometry type that spwt supported
#' @noRd
.check_spwt = \(sfj){
  if (!inherits(sfj,'sf')){
    stop('sfj must be an sf object')
  }

  if (!(sf_geometry_type(sfj) %in% c('point','multipoint',
                                     'polygon','multipolygon'))){
    stop("Only (multi-)point and (multi-)polygon vector objects are supported")
  }
}

#' polygon contiguity based spatial weights
#' @noRd
.spwt_polygon_contiguity = \(sfj,
                             queen = TRUE,
                             order = 1L,
                             cumulate = TRUE,
                             style = 'W',
                             zero.policy = TRUE){
  .check_spwt(sfj)

  if (sf_geometry_type(sfj) %in% c('point','multipoint')){
    sfj = sf_voronoi_diagram(sfj)
  }

  sfj_nb = spdep::poly2nb(sfj,queen = queen)
  if (order >= 2) {
    sfj_nb_highorder = spdep::nblag(sfj_nb, order)
    if (cumulate) {
      sfj_nb = spdep::nblag_cumul(sfj_nb_highorder)
    } else {
      sfj_nb = sfj_nb_highorder[[order]]
    }
  }

  sfj_wt = spdep::nb2mat(sfj_nb, style = style,
                         zero.policy = zero.policy)

  return(sfj_wt)
}

#' distance based contiguity to construct spatial weights
#' @noRd
.spwt_distance_contiguity = \(sfj,
                              k = 6,
                              style = 'W',
                              zero.policy = TRUE){
  .check_spwt(sfj)

  if (sf_geometry_type(sfj) %in% c('multipoint','multipolygon')){
    suppressWarnings({sfj = sf::st_point_on_surface(sfj)})
  } else if (sf_geometry_type(sfj) == 'polygon') {
    suppressWarnings({sfj = sf::st_centroid(sfj)})
  }

  coords = sfj %>%
    sf::st_coordinates() %>%
    {.[,c('X','Y')]}

  longlat = dplyr::if_else(sf::st_is_longlat(sfj),TRUE,FALSE,FALSE)

  nb_knn = spdep::knearneigh(coords,k = k,longlat = longlat)
  sfj_nb = spdep::knn2nb(nb_knn)

  sfj_wt = spdep::nb2mat(sfj_nb, style = style,
                         zero.policy = zero.policy)

  return(sfj_wt)
}

#' spatial weights with distance kernel function
#' @noRd
.spwt_kernel_weight = \(sfj,
                        bandwidth = NULL,
                        k = NULL,
                        kernel = "gaussian",
                        style = 'W',
                        zero.policy = TRUE){
  .check_spwt(sfj)

  if (sf_geometry_type(sfj) %in% c('multipoint','multipolygon')){
    suppressWarnings({sfj = sf::st_point_on_surface(sfj)})
  } else if (sf_geometry_type(sfj) == 'polygon') {
    suppressWarnings({sfj = sf::st_centroid(sfj)})
  }

  coords = sfj %>%
    sf::st_coordinates() %>%
    {.[,c('X','Y')]}

  longlat = dplyr::if_else(sf::st_is_longlat(sfj),TRUE,FALSE,FALSE)

  if (is.null(k)) {k = 1}

  if (is.null(bandwidth)){
    k1 = spdep::knn2nb(spdep::knearneigh(coords,k = k,longlat = longlat))
    bandwidth = max(unlist(spdep::nbdists(k1,coords,longlat = longlat)))
  }

  kernelnb = spdep::dnearneigh(coords, 0, bandwidth, longlat = longlat)
  kernelnb = spdep::include.self(kernelnb)
  kerneldist = spdep::nbdists(kernelnb,coords,longlat = longlat)

  switch (kernel,
          "uniform" = {
            kw_fun = function(x) x * 0 + 0.5
          },
          "triangular" = {
            kw_fun = function(x) 1 - abs((x/bandwidth))
          },
          "epanechnikov" = {
            kw_fun = function(x) .75*(1-(x/bandwidth)^2)
          },
          "quadratic" = {
            kw_fun = function(x) .75*(1-(x/bandwidth)^2)
          },
          "quartic" = {
            kw_fun = function(x) (15/16)*(1-(x/bandwidth)^2)^2
          },
          "gaussian" = {
            kw_fun = function(x) (1/sqrt(2*pi))*exp((-(x/bandwidth)^2)/2)
          }
  )

  kernelwt = lapply(kerneldist, kw_fun)
  sfj_wt = spdep::nb2mat(kernelnb, glist = kernelwt,
                         style = style, zero.policy = zero.policy)

  return(sfj_wt)
}

#' inverse distance weight via spdep package
#' @noRd
.spwt_idw = \(sfj,
              bandwidth = NULL,
              power = 1,
              style = 'W',
              zero.policy = TRUE){
  .check_spwt(sfj)

  if (sf_geometry_type(sfj) %in% c('multipoint','multipolygon')){
    suppressWarnings({sfj = sf::st_point_on_surface(sfj)})
  } else if (sf_geometry_type(sfj) == 'polygon') {
    suppressWarnings({sfj = sf::st_centroid(sfj)})
  }

  coords = sfj %>%
    sf::st_coordinates() %>%
    {.[,c('X','Y')]}

  longlat = dplyr::if_else(sf::st_is_longlat(sfj),TRUE,FALSE,FALSE)

  k = nrow(coords) - 1
  if (is.null(bandwidth)){
    suppressWarnings({
      k1 = spdep::knn2nb(spdep::knearneigh(coords,k = k,longlat = longlat))
    })
    bandwidth = max(unlist(spdep::nbdists(k1,coords,longlat = longlat)))
  }

  kernelnb = spdep::dnearneigh(coords, 0, bandwidth, longlat = longlat)
  kerneldist = spdep::nbdists(kernelnb,coords,longlat = longlat)
  kernelwt = lapply(kerneldist, \(x) 1 / x ^ power)
  sfj_wt = spdep::nb2mat(kernelnb, glist = kernelwt,
                         style = style, zero.policy = zero.policy)

  return(sfj_wt)
}

#' @title construct inverse distance weight
#' @description
#' Function for constructing inverse distance weight.
#' @details
#' The inverse distance weight formula is
#' \eqn{w_{ij} = 1 / d_{ij}^\alpha}
#'
#' @param sfj Vector object that can be converted to `sf` by `sf::st_as_sf()`.
#' @param power (optional) Default is 1. Set to 2 for gravity weights.
#' @param bandwidth (optional) When the distance is bigger than bandwidth, the
#' corresponding part of the weight matrix is set to 0. Default is `NULL`, which
#' means not use the bandwidth.
#'
#' @return A inverse distance weight matrices with class of `matrix`.
#' @export
#'
#' @examples
#' library(sf)
#' pts = read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#' wt = inverse_distance_swm(pts)
#' wt[1:5,1:5]
#'
inverse_distance_swm = \(sfj,
                         power = 1,
                         bandwidth = NULL){
  .check_spwt(sfj)

  if (sf_geometry_type(sfj) %in% c('multipoint','multipolygon')){
    suppressWarnings({sfj = sf::st_point_on_surface(sfj)})
  } else if (sf_geometry_type(sfj) == 'polygon') {
    suppressWarnings({sfj = sf::st_centroid(sfj)})
  }

  coords = sfj %>%
    sf::st_coordinates() %>%
    {.[,c('X','Y')]}

  longlat = dplyr::if_else(sf::st_is_longlat(sfj),TRUE,FALSE,FALSE)
  if (longlat) {
    distij = stats::as.dist(geosphere::distm(coords))
  } else {
    distij = stats::dist(as.data.frame(coords))
  }
  wij = 1 / distij ^ power

  if (!is.null(bandwidth)){
    wij = apply(wij,1,\(x) ifelse(x >= bandwidth,0,x))
  }

  return(as.matrix(wij))
}
