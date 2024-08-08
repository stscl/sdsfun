#' @title sf object geometry column name
#' @description
#' Get the geometry column name of an sf object
#'
#' @param sfj An `sf` object.
#'
#' @return A character.
#' @export
#'
#' @examples
#' library(sf)
#' snnu = read_sf(system.file('extdata/snnu.geojson',package = 'sdsfun'))
#' sf_geometry_name(snnu)
#'
sf_geometry_name = \(sfj){
  gname = attr(sfj, "sf_column")
  return(gname)
}

#' @title sf object geometry type
#' @description
#' Get the geometry type of an sf object
#'
#' @param sfj An `sf` object.
#'
#' @return A lowercase character vector
#' @export
#'
#' @examples
#' library(sf)
#' snnu = read_sf(system.file('extdata/snnu.geojson',package = 'sdsfun'))
#' sf_geometry_type(snnu)
#'
sf_geometry_type = \(sfj){
  sfj_type = sfj %>%
    sf::st_geometry_type() %>%
    as.character() %>%
    unique()
  return(tolower(sfj_type))
}
