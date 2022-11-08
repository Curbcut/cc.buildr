#' Build streets dataframe
#'
#' `build_streets()` uses a DA table to download street data from
#' Open Street Map.
#'
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame from which boundaries
#' are used to filter the streets, and add other DA information.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return Returns a data.frame of all street segments in the boundaries of the
#' \code{"DA_table"}, with \code{"ID"}, \code{"name_2"}, \code{"DAUID"},
#' \code{"osm_ID"} and, \code{"geometry"} columns.
#' @export
build_streets <- function(DA_table, crs) {

  # Get DA_table bbox
  DA_table_bbox <- sf::st_bbox(DA_table)

  # Transform DA_table to desired projection
  DA_table <- sf::st_transform(DA_table, crs)

  # Osm street retrieval ----------------------------------------------------
  # Retrieve ALL OSM street features within bounding box
  street_osm <-
    DA_table_bbox |>
    osmdata::opq(timeout = 200) |>
    osmdata::add_osm_feature(key = "highway") |>
    osmdata::osmdata_sf()

  # Make a uniform dataframe off the street linestrings and multilinestrings
  valid_streets <-
    future.apply::future_lapply(
      list(street_osm$osm_lines, street_osm$osm_multilines),
      \(x) {
        suppressWarnings({
          # Keep 'cars' street
          x <- x[
            x$highway %in% c("motorway", "trunk", "primary", "secondary", "tertiary",
                             "residential", "unclassified", "service",
                             "motorway_link", "trunk_link", "primary_link",
                             "secondary_link", "tertiary_link"), ]
          x <- x[, c("osm_id", "geometry")]
          names(x) <- c("osm_ID", "geometry")
          x <- sf::st_cast(x, "MULTILINESTRING")
          sf::st_make_valid(x)
        })
      }, future.seed = NULL
    )
  street_nw <- do.call(rbind, valid_streets)
  street_nw <- sf::st_transform(street_nw, crs)
  street_nw <- street_nw[!sf::st_is_empty(street_nw$geometry), ]
  if (length(unique(street_nw$osm_ID)) != nrow(street_nw)) {
    street_nw <- unique(street_nw)
  }
  street_nw <- sf::st_cast(street_nw, "MULTILINESTRING")
  # Filter street in our geometry
  street_nw <- sf::st_filter(street_nw, DA_table)


  # Slice streets -----------------------------------------------------------

  # Very useful if there is a future backend
  street_grid <- sf::st_make_grid(street_nw, n = c(16, 8))

  pb <- progressr::progressor(steps = length(street_grid))
  street_list <-
    future.apply::future_lapply(street_grid, \(x) {
    zone_x <- sf::st_filter(street_nw, x)

    if (nrow(zone_x) > 0) {
      nodes <- sf::st_intersection(zone_x)
      nodes <- nodes[sf::st_is(nodes$geometry, "POINT"), ]
      if (nrow(nodes) > 0) {
        zone_x <- lwgeom::st_split(zone_x, nodes)
        zone_x <- suppressWarnings(st_collection_extract(zone_x, "LINESTRING"))
      }
    } else nodes <- NULL

    pb()
    return(list(zone_x, nodes))

  })

  # Bind streets and nodes coming from the past lapply operation
  street <- lapply(street_list, `[[`, 1)
  street <- street[sapply(street, nrow) > 0]
  street <- future.apply::future_lapply(street, sf::st_cast, "MULTILINESTRING",
                                        future.seed = NULL)
  street <- data.table::rbindlist(street)
  street <- unique(tibble::as_tibble(street))
  street <- sf::st_as_sf(street)
  nodes <- lapply(street_list, `[[`, 2)
  nodes <- nodes[!sapply(nodes, is.null)]
  nodes <- nodes[sapply(nodes, nrow) > 0]
  nodes <- data.table::rbindlist(nodes)
  nodes <- unique(tibble::as_tibble(nodes))
  nodes <- sf::st_as_sf(nodes)

  # Check edges which crossed a grid boundary
  street_grid_edges <- sf::st_cast(street_grid, "LINESTRING")
  edges_to_check <- sf::st_filter(street_nw, street_grid_edges)$osm_ID

  # Re-split these edges
  new_street <- street_nw[street_nw$osm_ID %in% edges_to_check, ]
  new_street <- lwgeom::st_split(new_street, nodes)
  new_street <- st_collection_extract(new_street, "LINESTRING")

  # Merge with other results
  street <- rbind(street[!street$osm_ID %in% edges_to_check, ], new_street)
  street <- sf::st_cast(street, "MULTILINESTRING")

  # Add ID
  street$ID <- paste0("street_", seq_along(street$osm_ID))
  street <- street[, c("ID", "osm_ID", "geometry")]
  row.names(street) <- NULL


  # Add DA ID ---------------------------------------------------------------

  DA_table <- sf::st_transform(DA_table, crs)[, "ID"]
  names(DA_table)[1] <- "DAUID"

  street_centroids <- suppressWarnings(sf::st_centroid(street))
  da_joined <- sf::st_join(street_centroids, DA_table)
  da_joined <- sf::st_drop_geometry(da_joined[, c("ID", "DAUID")])
  street <- merge(street, da_joined, by = "ID")
  street <- street[!is.na(street$DAUID), ]


  # Consolidate and clean output --------------------------------------------

  street$name <- NA_character_
  street$name_2 <- NA_character_
  street <- street[, c(
    "ID", "name", "name_2", "DAUID", "osm_ID", "geometry"
  )]
  street <- sf::st_make_valid(street)
  street <- street[!sf::st_is_empty(street$geometry), ]
  street <- sf::st_transform(street, 4326)
  street <- sf::st_set_agr(street, "constant")
  street <- tibble::as_tibble(street)
  street <- sf::st_as_sf(street)

  return(street)
}
