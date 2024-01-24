#' Create master polygon
#'
#' @param all_regions <`named list`> A name list of all the regions wanted. The value
#' of all level must either be a valid `regions` call argument to the
#' \code{\link[cancensus]{get_census}} function, e.g. \code{list(CMA = 24462)}
#' for Montreal, a link to a shapefile, e.g. \code{list("geometry/island.shp"},
#' or an already sf data.frame.
#' @param crs <`numeric`> Optional. EPSG coordinate reference system to be
#' assigned, e.g. \code{32618} for Montreal. Optional, defaults to the UTM zone
#' retrieved by the centroid of the master_polygon created. If not supplied, it
#' is derived from the centroid of all the regions through a simple mathematical
#' approach.
#' @param prov_code <`numeric`> Is the provinde code from cancensus (GeoUID)
#' already known? If so, enter it. It can help if the centroid of the master
#' polygon of the region is in a lake! Then no province is filtered.
#'
#' @return The output is a named list of length 5: The master polygon,
#' all the individual regions present in \code{all_regions}, the crs, the
#' `regions` argument to \code{\link[cancensus]{get_census}} of the province
#' in which the centroid of the master polygon falls, and the carographic
#' DA version.
#'
#' @export
create_master_polygon <- function(all_regions, crs = NULL, prov_code = NULL) {
  # Download or retrieve the geo sf -----------------------------------------

  regions <-
    sapply(all_regions, \(x) {
      z <- if (is.data.frame(x)) {
        x
      } else if (!is.list(x)) {
        sf::read_sf(x)
      } else {
        out <- cancensus::get_census(cc.buildr::current_census,
                                     regions = x,
                                     # DA for a better spatial coverage
                                     level = names(x),
                                     geo_format = "sf",
                                     quiet = TRUE
        )
        names(out)[names(out) == "GeoUID"] <- "ID"
        cc.data::census_switch_full_geo(
          df = out,
          scale_name = names(x)
        )
      }
      z <- sf::st_union(z)
      z <- z[sf::st_is(z, "MULTIPOLYGON") | sf::st_is(z, "POLYGON")]
      z <- sf::st_transform(z, 4326)
      z <- sf::st_cast(z, "MULTIPOLYGON")
      z <- sf::st_make_valid(z)
      z
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Get crs -----------------------------------------------------------------

  crs <- if (!is.null(crs)) {
    crs
  } else {
    z <- sf::st_centroid(regions[[1]])
    z <- sf::st_coordinates(z)[1]
    utm_zone <- round((z + 180) / 6)
    as.numeric(paste0("326", utm_zone))
  }


  # Make valid master_polygon -----------------------------------------------

  regions_crs <-
    sapply(regions, sf::st_transform, crs, simplify = FALSE, USE.NAMES = TRUE)
  master_polygon <- Reduce(sf::st_union, regions_crs)
  master_polygon <- master_polygon[
    sf::st_geometry_type(master_polygon) == "MULTIPOLYGON" |
      sf::st_geometry_type(master_polygon) == "POLYGON"
  ]
  master_polygon <- sf::st_make_valid(master_polygon)
  master_polygon <- sf::st_transform(master_polygon, 4326)


  # Get region from Cancensus -----------------------------------------------

  provinces <- cancensus::get_census(cc.buildr::current_census,
                                     regions = list(C = 01),
                                     level = "PR", geo_format = "sf",
                                     quiet = TRUE
  )
  master_polygon <- sf::st_make_valid(master_polygon)
  master_polygon_centroid <- sf::st_centroid(master_polygon)
  if (is.null(prov_code))
    prov_code <- sf::st_intersection(provinces, master_polygon_centroid)$GeoUID

  if (length(prov_code) == 0) {
    stop("Provide manually the cancensus province code (GeoUID).")
  }

  # Get cartogrtaphic DA ----------------------------------------------------

  DA_carto <- get_census_cc(
    master_polygon = master_polygon,
    census_dataset = cc.buildr::current_census,
    regions = list(PR = prov_code),
    level = "DA",
    crs = crs,
    cartographic = TRUE,
    area_threshold = 0.05
  )

  DA_carto <- sf::st_transform(DA_carto, crs)
  DA_carto <- sf::st_union(DA_carto)


  # Return ------------------------------------------------------------------

  return(list(
    master_polygon = master_polygon,
    regions = regions,
    crs = crs,
    province_cancensus_code = list(PR = prov_code),
    DA_carto = DA_carto
  ))
}
