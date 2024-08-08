#' contiguity spatial weights
#' @noRd
.spwt_contiguity = \(sfj,
                     order = 1L,
                     style = 'W',
                     queen = TRUE,
                     cumulate = TRUE,
                     zero.policy = TRUE){
  if (!inherits(sfj,'sf')){
    stop('sfj must be an sf object')
  }

  if (!(sf_geometry_type(sfj) %in% c('point','multipoint',
                                     'polygon','multipolygon'))){
    stop("Only (multi-)point and (multi-)polygon vector objects are supported")
  }

  if (sf_geometry_type(sfj) %in% c('point','multipoint')){
    sfj = sf_voronoi_diagram(sfj)
  }

  sfj_nb = spdep::poly2nb(sfj,queen = queen)
  if (order >= 2) {
    sfj_nb_highorder = spdep::nblag(sfj_nb, order)
    if (cumulate){
      sfj_nb = spdep::nblag_cumul(sfj_nb_highorder)
    } else {
      sfj_nb = sfj_nb_highorder
    }
  }

  sfj_wt = spdep::nb2mat(sfj_nb, style = style,
                         zero.policy = zero.policy)

  return(sfj_wt)
}
