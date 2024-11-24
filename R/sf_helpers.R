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
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' sf_geometry_name(gzma)
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
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' sf_geometry_type(gzma)
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
#' @return An `sf` object of polygon geometry type or can be converted to this by `sf::st_as_sf()`.
#' @export
#'
#' @examples
#' pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
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
  if (!inherits(sfj,'sf')){
    sfj = sf::st_as_sf(sfj)
  }

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

#' @title extract locations
#' @description
#' Extract locations of sf objects.
#'
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#' sf_coordinates(pts)
#'
sf_coordinates = \(sfj){
  .check_spwt(sfj)

  if (sf_geometry_type(sfj) %in% c('multipoint','multipolygon')){
    suppressWarnings({sfj = sf::st_point_on_surface(sfj)})
  } else if (sf_geometry_type(sfj) == 'polygon') {
    suppressWarnings({sfj = sf::st_centroid(sfj)})
  }

  coords = sfj %>%
    sf::st_coordinates() %>%
    {.[,c('X','Y')]}

  return(as.matrix(coords))
}

#' @title generates distance matrix
#' @description
#' Generates distance matrix for sf object
#'
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' pts = sf::read_sf(system.file('extdata/pts.gpkg',package = 'sdsfun'))
#' pts_distm = sf_distance_matrix(pts)
#' pts_distm[1:5,1:5]
#'
sf_distance_matrix = \(sfj){
  coords = sf_coordinates(sfj)
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
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
#'
#' @return A character.
#' @export
#'
#' @examples
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' sf_utm_proj_wgs84(gzma)
#'
sf_utm_proj_wgs84 = \(sfj){
  if (!inherits(sfj,'sf')){
    sfj = sf::st_as_sf(sfj)
  }
  crs_info = sf::st_crs(sfj)
  iswgs84 = dplyr::if_else(crs_info$epsg == 4326,TRUE,FALSE,FALSE)
  if (!iswgs84){
    stop("The spatial reference of the input `sfj` object needs to be the WGS84 geographic coordinate system.")
  } else {
    sf_ext = as.double(sf::st_bbox(sfj))
    center_lon = mean(sf_ext[c(1,3)])
    utm_zone = floor((center_lon + 180) / 6) + 1
    return(paste0("EPSG:326",utm_zone))
  }
}

#' @title generates cgcs2000 Gauss-Kruger projection epsg coding character
#' @description
#' Generates a Gauss-Kruger projection epsg coding character corresponding to an `sfj` object
#' under the CGCS2000 spatial reference.
#'
#' @param sfj An `sf` object or can be converted to `sf` by `sf::st_as_sf()`.
#' @param degree (optional) `3`-degree or `6`-degree zonal projection, default is `6L`.
#'
#' @return A character.
#' @export
#'
#' @examples
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun')) |>
#'   sf::st_transform(4490)
#' sf_gk_proj_cgcs2000(gzma,3)
#' sf_gk_proj_cgcs2000(gzma,6)
#'
sf_gk_proj_cgcs2000 = \(sfj,degree = 6L){
  if (!inherits(sfj,'sf')){
    sfj = sf::st_as_sf(sfj)
  }
  crs_info = sf::st_crs(sfj)
  iscgcs2000 = dplyr::if_else(crs_info$epsg == 4490,TRUE,FALSE,FALSE)
  if (!iscgcs2000){
    stop("The spatial reference of the input `sfj` object needs to be the CGCS2000 geographic coordinate system.")
  }

  sf_ext = as.double(sf::st_bbox(sfj))
  center_lon = mean(sf_ext[c(1,3)])
  if (degree == 6L) {
    gk_zone = floor((center_lon + 6) / 6)
    GKCenterLong = gk_zone * 6 - 3
    epsgcode = seq(4502,4512,by = 1)[which(seq(75,135,by = 6) == GKCenterLong)]
  } else if (degree == 3L) {
    gk_zone = floor((center_lon + 1.5) / 3)
    GKCenterLong = gk_zone * 3
    epsgcode = seq(4534,4554,by = 1)[which(seq(75,135,by = 3) == GKCenterLong)]
  } else {
    stop("The degree only be `3` or `6`!")
  }
  return(paste0("EPSG:",epsgcode))
}
