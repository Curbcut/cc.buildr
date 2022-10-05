#' Add a scale
#'
#' Using `additional_scale()` is useful both for interpolating population and
#' households using dissemination areas, but also for ensuring that the new
#' scale is uniform, containing all the columns expected in an added scale.
#'
#' @param additional_table <`sf data.frame`> An sf data.frame of two columns:
#' \code{name} and \code{geometry}. Ideally, the name of each zone should be
#' unique.
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame from which
#' population and households will be interpolated.
#' @param ID_prefix <`character`> A character of length 1. In order to keep
#' the identifiers unique on the platform, the prefix used before the
#' \code{seq_along()} that will be called to create unique identifiers.
#' e.g. \code{"ward"}
#' @param name_2 <`character`> A character of length 1. \code{name_2} will be
#' used to define the zone along with the \code{name}. The display on Sus would
#' be e.g. `{name_2} of {name}`, or `Ward of Davenport`.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return An sf data.frame with population and households interpolated from
#' the \code{DA_table}, containing all columns necessary to import on the
#' platform.
#' @export
additional_scale <- function(additional_table, DA_table, ID_prefix, name_2,
                             crs) {
  if (!all(names(additional_table) == c("name", "geometry"))) {
    stop("`additional_table` must strictly have a name and a geometry column.")
  }

  # Interpolate DA_table to get population and households
  das <- DA_table[, c("households", "population")]
  das <- sf::st_transform(das, crs)
  das <- sf::st_set_agr(das, "constant")
  # Add DA area
  das$area <- susbuildr::get_area(das$geometry)
  # Add new table area
  additional_table <- sf::st_transform(additional_table, crs)
  intersected_table <- suppressWarnings(sf::st_intersection(additional_table, das))
  intersected_table$new_area <- susbuildr::get_area(intersected_table$geometry)

  # Get proportion of area per zone, and get population and households
  intersected_table$area_prop <- intersected_table$new_area / intersected_table$area

  intersected_table$pop <- intersected_table$area_prop * intersected_table$population
  intersected_table$hou <- intersected_table$area_prop * intersected_table$households

  intersected_table <- lapply(unique(additional_table$name), \(x) {
    z <- intersected_table[intersected_table$name == x, ]
    data.frame(
      name = x,
      population = round(sum(z$pop)),
      households = round(sum(z$hou))
    )
  })
  intersected_table <- do.call(rbind, intersected_table)

  # Consolidate and clean output
  additional_table$ID <- paste(ID_prefix, seq_along(additional_table$name),
    sep = "_"
  )
  additional_table <- merge(additional_table, intersected_table, by = "name")
  additional_table$name_2 <- name_2
  additional_table <- sf::st_transform(additional_table, 4326)
  additional_table[, c(
    "ID", "name", "name_2", "population", "households",
    "geometry"
  )]
}
