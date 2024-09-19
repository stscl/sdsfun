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

#' @title generates voronoi diagram
#' @description
#' Generates Voronoi diagram (Thiessen polygons) for sf object
#' @note
#' Only sf objects of (multi-)point type are supported to generate voronoi diagram
#' and the returned result includes only the geometry column.
#'
#' @param sfj An `sf` object.
#'
#' @return An `sf` object of polygon geometry type.
#' @export
#'
#' @examples
#' library(sf)
#' pts = read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#' pts_v = sf_voronoi_diagram(pts)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = pts_v, color = 'red',
#'           fill = 'transparent') +
#'   geom_sf(data = pts, color = 'blue', size = 1.25) +
#'   theme_void()
#'
sf_voronoi_diagram = \(sfj){
  if (!(sf_geometry_type(sfj) %in% c('point','multipoint'))){
    stop("Only (multi-)point vector objects are supported to generate voronoi diagram")
  }

  suppressWarnings({
   sfj_voronoi = sfj %>%
     sf::st_geometry() %>%
     sf::st_union() %>%
     sf::st_voronoi() %>%
     sf::st_collection_extract() %>%
     sf::st_sf(geometry = .) %>%
     tibble::as_tibble() %>%
     sf::st_as_sf()
   })

  return(sfj_voronoi)
}

#' @title generates distance matrix
#' @description
#' Generates distance matrix for sf object
#'
#' @param sfj An `sf` object.
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' library(sf)
#' pts = read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#' pts_distm = sf_distance_matrix(pts)
#' pts_distm[1:5,1:5]
#'
sf_distance_matrix = \(sfj){
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

  return(as.matrix(distij))
}

#' @title generates wgs84 utm projection epsg coding character
#' @description
#' Generates a utm projection epsg coding character corresponding to an `sfj` object
#' under the WGS84 spatial reference.
#'
#' @param sfj An `sf` object.
#'
#' @return A character.
#' @export
#'
#' @examples
#' library(sf)
#' snnu = read_sf(system.file('extdata/snnu.geojson',package = 'sdsfun'))
#' sf_utm_proj_wgs84(snnu)
#'
sf_utm_proj_wgs84 = \(sfj){
  longlat = dplyr::if_else(sf::st_is_longlat(sfj),TRUE,FALSE,FALSE)
  if (!longlat){
    stop("The spatial reference of the input `sfj` object needs to be in the WGS84 geographic coordinate system.")
  } else {
    sf_ext = as.double(sf::st_bbox(sfj))
    center_lon = mean(sf_ext[c(1,3)])
    utm_zone = floor((center_lon + 180) / 6) + 1
    return(paste0("EPSG:326",utm_zone))
  }
}
