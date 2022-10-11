#' Create master polygon
#'
#' @param all_regions <`named list`> A name list of all the geos wanted. The value
#' of all level must either be a valid `regions` call argument to the
#' \code{\link[cancensus]{get_census}} function, e.g. \code{list(CMA = 24462)}
#' for Montreal, or either a link to a shapefile, e.g. \code{list("geometry/island.shp"}.
#' @param crs <`numeric`> Optional. EPSG coordinate reference system to be
#' assigned, e.g. \code{32618} for Montreal. Optional, defaults to the UTM zone
#' retrieved by the centroid of the master_polygon created. If not supplied, it
#' is derived from the centroid of all the geos through a simple mathematical
#' approach.
#'
#' @return The output is a named list of length 4: The master polygon,
#' all the individual geos present in \code{all_regions}, the crs, and the
#' `regions` argument to \code{\link[cancensus]{get_census}} of the province
#' in which the centroid of the master polygon falls.
#' @export
create_master_polygon <- function(all_regions, crs = NULL) {


  # Download or retrieve the geo sf -----------------------------------------

  geos <-
    sapply(all_regions, \(x) {
      z <- if (!is.list(x)) sf::read_sf(x) else {
        cancensus::get_census(susbuildr::current_census,
                              regions = x,
                              geo_format = "sf",
                              quiet = TRUE)
      }
      z <- sf::st_transform(z, 4326)
      z <- sf::st_make_valid(z)$geometry
      z <- sf::st_cast(z, "MULTIPOLYGON")
      z
    }, simplify = FALSE, USE.NAMES = TRUE)


  # Make valid master_polygon -----------------------------------------------

  master_polygon <- Reduce(sf::st_union, geos)
  master_polygon <- master_polygon[
    sf::st_geometry_type(master_polygon) == "MULTIPOLYGON"]
  master_polygon <- sf::st_make_valid(master_polygon)


  # Get crs -----------------------------------------------------------------

  crs <- if (!is.null(crs)) crs else {
    z <- sf::st_centroid(master_polygon)
    z <- sf::st_coordinates(z)[1]
    utm_zone <- round((z + 180) / 6)
    as.numeric(paste0("326", utm_zone))
  }


  # Get region from Cancensus -----------------------------------------------

  provinces <- cancensus::get_census(susbuildr::current_census,
                                     regions = list(C = 01),
                                     level = "PR", geo_format = "sf",
                                     quiet = TRUE)
  master_polygon_centroid <- sf::st_centroid(master_polygon)
  prov_code <- sf::st_intersection(provinces, master_polygon_centroid)$GeoUID


  # Return ------------------------------------------------------------------

  return(list(master_polygon = master_polygon,
              geos = geos,
              crs = crs,
              province_cancensus_code = list(PR = prov_code)))
}
