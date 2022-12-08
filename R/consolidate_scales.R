#' Filter spatially scales in their parent geometry
#'
#' Necessary 1. to spatially filter scales in their parent geometry, 2. to update
#' x_ID and name_2 for non-census scales, and 3. to update x_ID and name_2
#' after a census scale has been split, e.g. with boroughs in the City of
#' Montreal.
#'
#' @param all_tables <`named list`> A named list of character. The names are
#' the \code{geo} e.g. CMA, island, city, ... and the vectors are all the scales
#' available in these geo e.g. CSD, CT, DA, building, ...
#' @param all_scales <`named list`> A named list containing all the dataframes
#' present in the \code{all_tables} list.
#' @param regions <`named list`> A named list of all the unioned geometry
#' (the names of \code{all_tables}). The second element of the list returned
#' by \code{\link[cc.buildr]{create_master_polygon}}.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return A list of the same length as there are in \code{all_tables}, containing
#' spatially filtered dataframes with updated name_2 and IDs if needed.
#' @export
consolidate_scales <- function(all_tables, all_scales, regions, crs) {

  ## Add own ID to scales, and rename census ---------------------------------

  uniform_IDs <-
    future.apply::future_mapply(\(scale_name, scale_df) {

      # For all column names that end with `UID`, change it to `_ID`
      if (sum(grepl("UID$", names(scale_df))) > 0) {
        names(scale_df)[grepl("UID$", names(scale_df))] <-
          gsub("UID", "_ID", names(scale_df)[grepl("UID$", names(scale_df))])
      }

      # Add own ID to scale
      scale_df[[paste0(scale_name, "_ID")]] <- scale_df$ID

      # Reorder columns
      if (!"name_2" %in% names(scale_df)) scale_df$name_2 <- NA
      names_ids <-
        c("ID", "name", "name_2", names(scale_df)[grepl("_ID$", names(scale_df))])
      rest_cols <- names(scale_df)[!names(scale_df) %in% names_ids]
      sf::st_as_sf(scale_df)[, c(names_ids, rest_cols)]
    }, names(all_scales), all_scales, SIMPLIFY = FALSE, USE.NAMES = TRUE,
    future.seed = TRUE)


  ## Filter spatially the scales per their geo -------------------------------

  spatially_filtered <-
    future.apply::future_mapply(\(geo, scales) {
      sapply(scales, \(scale) {
        if (scale %in% c("street", "building")) {
          return(uniform_IDs[[scale]])
        }

        unioned_geo <- sf::st_transform(regions[[geo]], crs)
        df <- sf::st_transform(uniform_IDs[[scale]], crs)

        # Filter spatially with the unioned geo.
        df_points_on_surface <- suppressWarnings(sf::st_point_on_surface(df))
        ids_in <- sf::st_filter(df_points_on_surface, unioned_geo)$ID

        df[df$ID %in% ids_in, ]
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, names(all_tables), all_tables, SIMPLIFY = FALSE, USE.NAMES = TRUE,
    future.seed = TRUE)

  spatially_filtered <-
    map_over_scales(
      all_scales = spatially_filtered,
      fun = \(geo = geo, scales = scales,
        scale_name = scale_name, scale_df = scale_df) {
        if (scale_name %in% c("street", "building"))
          return(scale_df[scale_df$DA_ID %in% scales$DA$DA_ID, ])

        return(scale_df)
      }
    )


  ## Update name_2 and IDs for all cases (catch-all cases, like split scales) ----

  out <-
    map_over_scales(
      all_scales = spatially_filtered,
      fun = \(geo = geo, scales = scales,
              scale_name = scale_name, scale_df = scale_df) {
        if (scale_name %in% c("street", "building")) {
          # If street or building is missing an ID:
          above_levels <- names(scales)[2:(which(names(scales) == scale_name) - 1)]
          other_ids <- lapply(above_levels, \(above_lvl) {
            if (paste0(above_lvl, "_ID") %in% names(scale_df)) return()

            above_lvl_df <- scales[[above_lvl]]
            above_lvl_df <- above_lvl_df[paste0(above_lvl, "_ID")]
            above_lvl_df <- sf::st_transform(above_lvl_df, crs)
            df <- sf::st_transform(scale_df, crs)

            df_points_on_surface <-
              suppressWarnings(sf::st_point_on_surface(df))
            merged_centroids <- sf::st_join(df_points_on_surface, above_lvl_df)

            sf::st_drop_geometry(merged_centroids[c("ID", paste0(above_lvl, "_ID"))])
          })
          merge_ <-  function(x, y) merge(x, y, by = "ID")
          out <- Reduce(merge_, other_ids[!sapply(other_ids, is.null)], init = scale_df)

          return(out)
        }
        if (scale_name == names(scales)[1]) {
          return(scale_df)
        }

        top_level <- sf::st_transform(scales[[1]], crs)
        df <- sf::st_transform(scale_df, crs)

        # Join name_2 and ID of the top level
        top_level <- top_level[, c("name", paste0(names(scales[1]), "_ID"))]
        names(top_level)[1] <- "name_2"
        df <- df[, !names(df) %in% names(top_level)]

        df_points_on_surface <-
          suppressWarnings(sf::st_point_on_surface(df))
        merged_centroids <- sf::st_join(df_points_on_surface, top_level)
        out <- merge(sf::st_drop_geometry(merged_centroids),
          df[, c("ID", "geometry")],
          by = "ID"
        )

        # Add any missing ID in the scales (apart from the top_level already
        # taken care of)
        above_levels <- names(scales)[2:(which(names(scales) == scale_name) - 1)]
        other_ids <- lapply(above_levels, \(above_lvl) {
          if (paste0(above_lvl, "_ID") %in% names(out)) return()

          above_lvl_df <- scales[[above_lvl]]
          above_lvl_df <- above_lvl_df[paste0(above_lvl, "_ID")]
          df <- out

          df_points_on_surface <-
            suppressWarnings(sf::st_point_on_surface(df))
          merged_centroids <- sf::st_join(df_points_on_surface, above_lvl_df)

          sf::st_drop_geometry(merged_centroids[c("ID", paste0(above_lvl, "_ID"))])
        })
        merge_ <-  function(x, y) merge(x, y, by = "ID")
        out <- Reduce(merge_, other_ids[!sapply(other_ids, is.null)], init = out)

        # Take out _IDs that aren't in the scales (e.g., CSD)
        all_ids <- names(out)[grepl("_ID$", names(out))]
        drop_ids <- all_ids[!all_ids %in% paste0(names(scales), "_ID")]

        # Return
        out[, !names(out) %in% drop_ids]
      }
    )

  # + Add DA information to smaller scales (street and building)
  out <-
    map_over_scales(
      all_scales = out,
      fun = \(geo = geo, scales = scales,
        scale_name = scale_name, scale_df = scale_df) {
        if (!scale_name %in% c("street", "building"))
          return(scale_df)

        scale_above_building <-
          names(scales)[which(names(scales) == scale_name) - 1]

        das <- sf::st_drop_geometry(scales[[scale_above_building]])
        ids <- das[, c("name_2", names(das)[grepl("_ID$", names(das))])]
        df <- scale_df[, !names(scale_df) %in% names(ids)[
          names(ids) != paste0(scale_above_building, "_ID")]]

        merge(df, ids, by = paste0(scale_above_building, "_ID"))
      }
    )


  ## Get the CRS back to WGS 84 ---------------------------------------------

  out <-
    map_over_scales(
      all_scales = out,
      fun = \(scale_df = scale_df, ...) sf::st_transform(scale_df, 4326)
    )


  ## Reorder all columns ----------------------------------------------------

  out <-
    reorder_columns(all_scales = out)


  # Return the final output -------------------------------------------------

  return(out)
}
