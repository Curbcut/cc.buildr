#' Add a scale
#'
#' Using `additional_scale()` is useful both for interpolating population and
#' households using dissemination areas, but also for ensuring that the new
#' scale is uniform, containing all the columns expected in an added scale.
#'
#' @param additional_table <`sf data.frame`> An sf data.frame of two or three columns:
#' \code{name} and \code{geometry} or \code{ID}, \code{name} and \code{geometry}.
#' Ideally, the name of each zone should be unique.
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame from which
#' population and households will be interpolated.
#' @param ID_prefix <`character`> A character of length 1. In order to keep
#' the identifiers unique on the platform, the prefix used before the
#' \code{seq_along()} that will be called to create unique identifiers.
#' e.g. \code{"ward"}. If there is already an ID in the `additional_table`,
#' the ID_prefix will be appended in front of it.
#' @param name_2 <`character`> A character of length 1. \code{name_2} will be
#' used to define the zone along with the \code{name}. The display on Sus would
#' be e.g. `{name_2} of {name}`, or `Ward of Davenport`.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param DA_carto <`sf data.frame`> The cartographic version of DAs, one of the
#' output of \code{\link{create_master_polygon}}.
#'
#' @return An sf data.frame with population and households interpolated from
#' the \code{DA_table}, containing all columns necessary to import on the
#' platform.
#' @export
additional_scale <- function(additional_table, DA_table, ID_prefix, name_2,
                             crs, DA_carto) {
  if (!all(names(additional_table) == c("name", "geometry"))) {
    if (!all(names(additional_table) == c("ID", "name", "geometry"))) {
      stop(paste0(
        "`additional_table` must only have a name and a geometry ",
        "column, or an ID, name and geometry column."
      ))
    }
  }

  additional_table <- sf::st_transform(additional_table, crs = crs)
  additional_table <- digital_to_cartographic(additional_table, DA_carto,
                                              crs = crs)

  # Interpolate DA_table to get population and households
  das <- DA_table[, c("ID", "households", "population")]
  das_transformed <- sf::st_transform(das, crs)
  das <- sf::st_set_agr(das_transformed, "constant")

  # Add ID to the scale
  additional_table$ID <- if (!"ID" %in% names(additional_table)) {
    paste(ID_prefix, seq_along(additional_table$name), sep = "_")
  } else {
    paste(ID_prefix, additional_table$ID, sep = "_")
  }

  # Interpolate population and households using DAs
  additional_table <-
    interpolate_from_area(
      to = additional_table,
      from = DA_table,
      additive_vars = c("population", "households"),
      crs = crs
    )

  # Add area
  additional_table$area <- get_area(additional_table)

  # Consolidate output
  additional_table$name_2 <- name_2
  additional_table <- sf::st_transform(additional_table, 4326)
  additional_table[, c(
    "ID", "name", "name_2", "population", "households", "area",
    "geometry", "geometry_digital"
  )]
}
