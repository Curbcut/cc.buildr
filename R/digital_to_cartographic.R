#' Convert digital geographic data to cartographic representation
#'
#' This function transforms digital geographic data into a cartographic
#' format suitable for mapping and spatial analysis, ensuring that both
#' input datasets are in the same coordinate reference system before
#' performing spatial operations.
#'
#' @param scale <`sf object`> An sf object representing the digital geographic
#' data to be converted.
#' @param DA_carto <`sf object`> An sf object representing the cartographic
#' boundaries to intersect with the scale parameter.
#' @param crs <`character`> A character string specifying the coordinate
#' reference system to which the input data should be transformed.
#'
#' @return An sf object with the original digital data's geometry and the
#' converted cartographic data.
#' @export
digital_to_cartographic <- function(scale, DA_carto, crs) {

  # Transform to the specified CRS
  scale <- sf::st_transform(scale, crs = crs)
  DA_carto <- sf::st_transform(DA_carto, crs = crs)

  # Find the intersection of 'scale' and 'DA_carto'
  cartographic <- sf::st_intersection(scale, DA_carto)
  cartographic <- sf::st_cast(cartographic, "MULTIPOLYGON")

  # New geometry using cartographic, keep digital
  scale_no_geo <- sf::st_drop_geometry(scale)
  scale_no_geo$geometry <- cartographic$geometry
  scale_no_geo <- sf::st_as_sf(scale_no_geo, crs = crs)
  scale_no_geo$geometry_digital <- scale$geometry

  return(scale_no_geo)
}
