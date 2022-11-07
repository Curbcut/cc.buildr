#' Get CMHC zones in a certain CMA
#'
#' @param cancensus_region A list containing a named vector. It must be a valid
#' `regions` call argument to the \code{\link[cancensus]{get_census}} function,
#' e.g. \code{list(CMA = 24462)} for Montreal.
#' @param crs <`numeric`> Optional. EPSG coordinate reference system to be
#' assigned, e.g. \code{32618} for Montreal. Optional, defaults to the UTM zone
#' retrieved by the centroid of the master_polygon created. If not supplied, it
#' is derived from the centroid of all the regions through a simple mathematical
#' approach.
#'
#' @return An sf data.frame of two columns: name of the zone and geometry.
#' @export
get_cmhc_zones <- function(cancensus_region, crs = NULL) {

  # Get CMA shapefile
  master <- {
    z <- cancensus::get_census(susbuildr::current_census,
      regions = cancensus_region,
      # DA for a better spatial coverage
      level = "DA",
      geo_format = "sf",
      quiet = TRUE
    )
    z <- sf::st_union(z)
    z <- sf::st_transform(z, 4326)
    z <- sf::st_make_valid(z)
    z <- sf::st_cast(z, "MULTIPOLYGON")
    z
  }

  # Get crs
  crs <- if (!is.null(crs)) {
    crs
  } else {
    z <- suppressWarnings(sf::st_centroid(master[[1]]))
    z <- sf::st_coordinates(z)[1]
    utm_zone <- round((z + 180) / 6)
    as.numeric(paste0("326", utm_zone))
  }

  # Transform and prepare spatial geographies
  zones <- cmhc::get_cmhc_geography(level = "ZONE")
  zones <- sf::st_transform(zones, crs)
  zones <- sf::st_make_valid(zones)
  master <- sf::st_transform(master, crs)
  if (length(master) > 1) master <- sf::st_union(master)
  master <- sf::st_make_valid(master)

  # Filter in using the master, and cut it with its boundaries
  zones <- suppressWarnings(sf::st_intersection(zones, master))
  zones <- sf::st_cast(zones, "MULTIPOLYGON")
  # Filter out borders of other zones
  zones$new_area <- susbuildr::get_area(zones$Shape)
  zones$area_prop <- zones$new_area / zones$Shape_Area
  zones <- zones[zones$area_prop > 0.0001, "ZONE_NAME_EN"]
  names(zones)[1] <- "name"
  sf::st_geometry(zones) <- "geometry"

  # Return
  zones
}
