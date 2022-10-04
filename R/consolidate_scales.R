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
#' spatially filtered dataframes with updated name_2 and IDs if needed.
#' @export
consolidate_scales <- function(all_tables, all_scales, geos, crs) {

  # Add own ID to scales, and rename census ---------------------------------

  uniform_IDs <-
    mapply(\(scale_name, scale_df) {

      # For all column names that end with `UID`, change it to `_ID`
      if (sum(grepl("UID$", names(scale_df))) > 0)
        names(scale_df)[grepl("UID$", names(scale_df))] <-
          gsub("UID", "_ID", names(scale_df)[grepl("UID$", names(scale_df))])

      # Add own ID to scale
      scale_df[[paste0(scale_name, "_ID")]] <- scale_df$ID

      # Reorder columns
      names_ids <-
        c("ID", "name", "name_2", names(scale_df)[grepl("_ID$", names(scale_df))])
      rest_cols <- names(scale_df)[!names(scale_df) %in% names_ids]
      scale_df[, c(names_ids, rest_cols)]

    }, names(all_scales), all_scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  # Filter spatially the scales per their geo -------------------------------

  spatially_filtered <-
    mapply(\(geo, scales) {
      sapply(scales, \(scale) {
        unioned_geo <- sf::st_transform(geos[[geo]], crs)
        df <- sf::st_transform(uniform_IDs[[scale]], crs)

        # Filter spatially with the unioned geo
        df_centroids <- suppressWarnings(sf::st_centroid(df))
        ids_in <- sf::st_filter(df_centroids, unioned_geo)$ID
        df <- df[df$ID %in% ids_in, ]

        sf::st_transform(df, 4326)

      }, simplify = FALSE, USE.NAMES = TRUE)
    }, names(all_tables), all_tables, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  # Update name_2 and IDs if the top level of the geo isn't census data ------

  out <-
    mapply(\(geo, scales) {
      sapply(scales, \(scale) {
        # If the top level of the scales is CSD, do not modify
        if (all_tables[[geo]][1] == "CSD") return(spatially_filtered[[geo]][[scale]])

        # If the top level is not CSD, update name_2
        top_level <- sf::st_transform(spatially_filtered[[geo]][[1]], crs)
        df <- sf::st_transform(spatially_filtered[[geo]][[scale]], crs)

        # Filter spatially with the top level geo to intersect
        df <- df[, names(df) != "name_2"]
        df_centroids <- suppressWarnings(sf::st_centroid(df))
        top_level <- top_level[, c("name", names(top_level)[
          grepl("_ID$", names(top_level))], "geometry")]
        names(top_level)[1] <- "name_2"
        df_name_2 <-
          suppressWarnings(sf::st_intersection(df_centroids, top_level)) |>
          sf::st_drop_geometry()

        # Re-bind geometry
        df <- merge(df_name_2, df[, c("ID", "geometry")], by = "ID")
        df <- sf::st_as_sf(df)

        # Reorder column names
        names_id <- c("ID", "name", "name_2")
        all_ids <- names(df)[names(df) %in% paste0(all_tables[[geo]], "_ID")]
        all_ids <- paste0(all_tables[[geo]], "_ID")[
          paste0(all_tables[[geo]], "_ID") %in% all_ids]
        rest_cols <- names(df)[!names(df) %in% c(names_id, all_ids)]
        rest_cols <- rest_cols[!grepl("_ID$", rest_cols)]

        df[, c(names_id, all_ids, rest_cols)]

      }, simplify = FALSE, USE.NAMES = TRUE)
    }, names(all_tables), all_tables, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  # Return the final output -------------------------------------------------

  return(out)

}
