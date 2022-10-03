#' Filter spatially scales in their parent geometry
#'
#' @param all_tables <`named list`> A named list of character. The names are
#' the \code{geo} e.g. CMA, island, city, ... and the vectors are all the scales
#' available in these geo e.g. CSD, CT, DA, building, ...
#' @param all_scales <`named list`> A named list containing all the dataframes
#' present in the \code{all_tables} list.
#' @param geos <`named list`> A named list of all the unioned geometry
#' (the names of \code{all_tables}). The second element of the list returned
#' by \code{\link[susbuildr]{create_master_polygon}}.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return A list of the same length as there are in \code{all_tables}, containing
#' spatially filtered dataframes with updated name_2 if needed.
#' @export
consolidate_scales <- function(all_tables, all_scales, geos, crs) {

  # Filter spatially the scales per their geo -------------------------------

  out <-
    mapply(\(geo, scales) {
      sapply(scales, \(scale) {
        unioned_geo <- sf::st_transform(geos[[geo]], crs)
        df <- sf::st_transform(all_scales[[scale]], crs)

        # Filter spatially with the unioned geo
        df_centroids <- suppressWarnings(sf::st_centroid(df))
        ids_in <- sf::st_filter(df_centroids, unioned_geo)$ID
        df <- df[df$ID %in% ids_in, ]

        sf::st_transform(df, 4326)

      }, simplify = FALSE, USE.NAMES = TRUE)
    }, names(all_tables), all_tables, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  # Update name_2 if the top level of the geo isn't census data -------------

  out <-
    mapply(\(geo, scales) {
      sapply(scales, \(scale) {
        # If the top level of the scales is CSD, do not modify
        if (all_tables[[geo]][1] == "CSD") return(out[[geo]][[scale]])

        # If the top level is not CSD, update name_2
        top_level <- sf::st_transform(out[[geo]][[1]], crs)
        df <- sf::st_transform(out[[geo]][[scale]], crs)

        # Filter spatially with the top level geo to intersect
        df <- df[, names(df) != "name_2"]
        df_centroids <- suppressWarnings(sf::st_centroid(df))
        top_level <- top_level[, c("name", "geometry")]
        names(top_level)[1] <- "name_2"
        df_name_2 <-
          suppressWarnings(sf::st_intersection(df_centroids, top_level)) |>
          sf::st_drop_geometry()

        # Merge, consolidate, clean
        df <- merge(df_name_2, df[, c("ID", "geometry")], by = "ID")
        df <- sf::st_as_sf(df)
        df[, c("ID", "name", "name_2", names(df)[!names(df) %in% c("ID", "name", "name_2")])]

      }, simplify = FALSE, USE.NAMES = TRUE)
    }, names(all_tables), all_tables, SIMPLIFY = FALSE, USE.NAMES = TRUE)

  return(out)

}
