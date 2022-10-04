#' Create master polygon
#'
#' @param all_tables <`named list`> The name of the named list \code{all_tables}
#' is used to retrieve the shapefiles in the supplied folder.
#' @param shapefiles_folder <`character`> The file path of where to find
#' the shapefiles of the geometries to read (names of all_table).
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return The output is a named list of length 2: The master polygon
#' and all the individual geos present in \code{all_tables}.
#' @export
create_master_polygon <- function(all_tables, shapefiles_folder, crs) {

  shp_present <- list.files(shapefiles_folder)
  shp_present <- subset(shp_present, grepl("\\.shp$", shp_present))
  shp_present <- gsub("\\.shp$", "", shp_present)

  # Error check
  if (!all(names(all_tables) %in% shp_present)) {
    missing_shp <- names(all_tables)[which(!names(all_tables) %in% shp_present)]
    stop(paste0("The shapefile for `", missing_shp,
                "` is missing in `shapefiles_folder`."))
  }

  # A polygon covering all our geographies
  geos <-
    sapply(names(all_tables), \(x) {
      if (!grepl("/$", shapefiles_folder))
        shapefiles_folder <- paste0(shapefiles_folder, "/")
      z <- sf::read_sf(paste0(shapefiles_folder, x, ".shp"))
      z <- sf::st_transform(z, crs)
      z <- sf::st_union(z)
      z <- sf::st_make_valid(z)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Master polygon creation
  master_polygon <- Reduce(sf::st_union, geos)
  master_polygon <- sf::st_transform(master_polygon, 4326)

  return(list(master_polygon = master_polygon, geos = geos))
}
