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
    if (cumulate){
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
    sfj = sf::st_point_on_surface(sfj)
  } else if (sf_geometry_type(sfj) == 'polygon') {
    sfj = sf::st_centroid(sfj)
  }

  nb_knn = spdep::knearneigh(sf::st_coordinates(sfj), k = k)
  sfj_nb = spdep::knn2nb(nb_knn)

  sfj_wt = spdep::nb2mat(sfj_nb, style = style,
                         zero.policy = zero.policy)

  return(sfj_wt)
}

