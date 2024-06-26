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

  id_present <- "ID" %in% names(scale)
  if (!id_present) scale$ID <- seq_along(scale$geometry)

  # Transform to the specified CRS
  scale <- sf::st_transform(scale, crs = crs)
  DA_carto <- sf::st_transform(DA_carto, crs = crs)

  # Find the intersection of 'scale' and 'DA_carto'
  cartographic <- sf::st_intersection(scale, DA_carto)
  cartographic <- sf::st_cast(cartographic, "MULTIPOLYGON")

  # Missing IDs in carto ?
  if (sum(!scale$ID %in% cartographic$ID) > 0) {
    missing <- scale[!scale$ID %in% cartographic$ID, ]
    sf::st_geometry(missing) <- rep(sf::st_sfc(sf::st_multipolygon(list()), crs = crs), nrow(missing))
    cartographic <- rbind(missing, cartographic)
  }

  # Order exactly the same
  cartographic <- cartographic[order(cartographic$ID), ]
  scale <- scale[order(scale$ID), ]

  if (!identical(cartographic$ID, scale$ID)) {
    stop(paste0("The string of IDs in the intersected scale (with DA_carto) and the ",
                "original scale is not identical."))
  }

  # New geometry using cartographic, keep digital
  scale <- sf::st_transform(scale, crs = 4326)
  scale_no_geo <- sf::st_drop_geometry(scale)
  scale_no_geo$geometry <- cartographic$geometry
  scale_no_geo <- sf::st_as_sf(scale_no_geo, crs = crs)
  scale_no_geo$geometry_digital <- scale$geometry

  if (!id_present) scale_no_geo$ID <- NULL

  return(scale_no_geo)
}
