#' Filter spatially scales in their parent geometry
#'
#' Necessary 1. to spatially filter scales in their parent geometry, 2. to update
#' x_ID and name_2 for non-census scales, and 3. to update x_ID and name_2
#' after a census scale has been split, e.g. with boroughs in the City of
#' Montreal.
#'
#' @param all_tables <`named list`> A named list of character. The names are
#' the \code{region} e.g. CMA, island, city, ... and the vectors are all the scales
#' available in these geo e.g. CSD, CT, DA, building, ...
#' @param all_scales <`named list`> A named list containing all the dataframes
#' present in the \code{all_tables} list.
#' @param regions <`named list`> A named list of all the unioned geometry
#' (the names of \code{all_tables}). The second element of the list returned
#' by \code{\link[cc.buildr]{create_master_polygon}}.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param match_with_centroids_regions <`character vector`> As some of the scales
#' now span over water, the `sf::st_point_on_surface` method of matching
#' which scales fit in which scales above (to attach CT_ID to DAs) doesn't necessarily work.
#' Centraide's zone do not span on water, and so a CT with its centroid in water
#' won't get matched to a Centraide zone. The calculation is much more intensive, so
#' this argument is used to bypass the new method. E.g. `grid` ONLY span land, and
#' all bottom scales of grid fit perfectly in the upper scales. In this case,
#' use `match_with_centroids_regions = "grid"` to speed up the process.
#'
#' @return A list of the same length as there are in \code{all_tables}, containing
#' spatially filtered dataframes with updated name_2 and IDs if needed.
#' @export
consolidate_scales <- function(all_tables, all_scales, regions, crs, match_with_centroids_regions) {
  ## Make sure IDs are unique ------------------------------------------------

  subset_ID <- sapply(all_scales, `[[`, "ID")
  subset_ID <- unlist(subset_ID)
  duplicated_IDs <- sum(table(subset_ID) > 1)

  if (duplicated_IDs > 0) {
    stop(paste0(
      "`ID` aren't unique between all the scales. Curbcut makes ",
      "multiple assumptions that IDs ARE unique accross all the ",
      "dataframes on the plateform."
    ))
  }

  ## Add own ID to scales, and rename census ---------------------------------

  uniform_IDs <-
    future.apply::future_mapply(
      \(scale_name, scale_df) {
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
      }, names(all_scales), all_scales,
      SIMPLIFY = FALSE, USE.NAMES = TRUE,
      future.seed = TRUE
    )


  ## Filter spatially the scales per their geo -------------------------------

  spatially_filtered <-
    future.apply::future_mapply(
      \(geo, scales) {
        sapply(scales, \(scale) {
          if (scale %in% c("street", "building")) {
            return(uniform_IDs[[scale]])
          }

          unioned_geo <- sf::st_transform(regions[[geo]], crs)
          df <- sf::st_transform(uniform_IDs[[scale]], crs)

          # Be more strict on the spatial filtering of the first level.
          out <- if (identical(scales[[1]], scale)) {
            spatial_filtering(
              df = df,
              crs = crs,
              master_polygon = unioned_geo,
              ID_col = "ID",
              area_threshold = 0.20
            )
          } else {
            # Filter spatially with the unioned geo.
            spatial_filtering(
              df = df,
              crs = crs,
              master_polygon = unioned_geo,
              ID_col = "ID",
              area_threshold = 0.01
            )
          }

          out
        }, simplify = FALSE, USE.NAMES = TRUE)
      }, names(all_tables), all_tables,
      SIMPLIFY = FALSE, USE.NAMES = TRUE,
      future.seed = TRUE
    )

  spatially_filtered <-
    map_over_scales(
      parallel = FALSE,
      all_scales = spatially_filtered,
      fun = \(geo = geo, scales = scales,
        scale_name = scale_name, scale_df = scale_df) {
        if (scale_name %in% c("street", "building")) {
          return(scale_df[scale_df$DA_ID %in% scales$DA$DA_ID, ])
        }

        return(scale_df)
      }
    )


  ## Update name_2 and IDs for all cases (catch-all cases, like split scales) ----

  out <-
    map_over_scales(
      parallel = TRUE,
      all_scales = spatially_filtered,
      fun = \(geo = geo, scales = scales,
              scale_name = scale_name, scale_df = scale_df) {
        if (scale_name %in% c("street", "building")) {
          # If street or building is missing an ID:
          above_levels <- names(scales)[2:(which(names(scales) == scale_name) - 1)]
          other_ids <- lapply(above_levels, \(above_lvl) {
            if (paste0(above_lvl, "_ID") %in% names(scale_df)) {
              return()
            }

            above_lvl_df <- scales[[above_lvl]]
            above_lvl_df <- above_lvl_df[paste0(above_lvl, "_ID")]
            above_lvl_df <- sf::st_transform(above_lvl_df, crs)
            df <- sf::st_transform(scale_df, crs)

            df_points_on_surface <-
              suppressWarnings(sf::st_point_on_surface(df))
            merged_centroids <- sf::st_join(df_points_on_surface, above_lvl_df)

            sf::st_drop_geometry(merged_centroids[c("ID", paste0(above_lvl, "_ID"))])
          })
          merge_ <- function(x, y) merge(x, y, by = "ID")
          out <- Reduce(merge_, other_ids[!sapply(other_ids, is.null)], init = scale_df)

          return(out)
        }
        if (scale_name == names(scales)[1]) {
          return(scale_df)
        }

        top_level <- sf::st_transform(scales[[1]], crs)
        df <- sf::st_transform(scale_df, crs)

        # Join name_2 and ID of the top level
        get_largest_intersection <- function(x, other) {
          intersections <- sf::st_intersection(x, other)
          areas <- sf::st_area(intersections)
          index <- which.max(areas)
          out <- sf::st_drop_geometry(intersections)[index, names(other)[names(other) != "geometry"]]
          if (nrow(out) == 0) {
            dist <- sf::st_distance(x, other)
            index <- which.min(dist)
            out <- sf::st_drop_geometry(other)[index, ]
          }
          if (nrow(out) == 0) {
            out <- tibble::add_row(out)
            for (n in names(other)) {
              out[[n]] <- NA
            }
          }

          out
        }

        top_level <- top_level[, c("name", paste0(names(scales[1]), "_ID"))]
        names(top_level)[1] <- "name_2"
        df <- df[, !names(df) %in% names(top_level)]

        if (geo %in% match_with_centroids_regions) {
          df_points_on_surface <-
            suppressWarnings(sf::st_point_on_surface(df))
          merged_centroids <- sf::st_join(df_points_on_surface, top_level)
          out <- merge(sf::st_drop_geometry(merged_centroids),
                       df[, c("ID", "geometry")],
                       by = "ID"
          )
        } else {
          which_fit <- lapply(seq_along(df$geometry), \(r) get_largest_intersection(df[r, ], top_level))
          which_fit <- Reduce(rbind, which_fit)
          out <- cbind(df, which_fit)
          out <- tibble::as_tibble(out)
          out <- sf::st_as_sf(out)
        }


        # Add any missing ID in the scales (apart from the top_level already
        # taken care of)
        above_levels <- names(scales)[2:(which(names(scales) == scale_name) - 1)]
        other_ids <- lapply(above_levels, \(above_lvl) {
          if (paste0(above_lvl, "_ID") %in% names(out)) {
            return()
          }

          above_lvl_df <- scales[[above_lvl]]
          above_lvl_df <- above_lvl_df[paste0(above_lvl, "_ID")]
          df <- out

          df_points_on_surface <-
            suppressWarnings(sf::st_point_on_surface(df))
          merged_centroids <- sf::st_join(df_points_on_surface, above_lvl_df)

          sf::st_drop_geometry(merged_centroids[c("ID", paste0(above_lvl, "_ID"))])
        })
        merge_ <- function(x, y) merge(x, y, by = "ID")
        out <- Reduce(merge_, other_ids[!sapply(other_ids, is.null)], init = out)

        # Return
        out
      }
    )

  # + Add DA information to smaller scales (street and building)
  out <-
    map_over_scales(
      all_scales = out,
      fun = \(geo = geo, scales = scales,
        scale_name = scale_name, scale_df = scale_df) {
        if (!scale_name %in% c("street", "building")) {
          return(scale_df)
        }

        scale_above_building <-
          names(scales)[which(names(scales) == scale_name) - 1]

        das <- sf::st_drop_geometry(scales[[scale_above_building]])
        ids <- das[, c("name_2", names(das)[grepl("_ID$", names(das))])]
        df <- scale_df[, !names(scale_df) %in% names(ids)[
          names(ids) != paste0(scale_above_building, "_ID")
        ]]

        merge(df, ids, by = paste0(scale_above_building, "_ID"))
      }
    )


  ## Get the CRS back to WGS 84 ---------------------------------------------

  out <-
    map_over_scales(
      all_scales = out,
      fun = \(scale_df = scale_df, ...) sf::st_transform(scale_df, 4326)
    )


  ## Post-processing (make sure all geometry types are right) ---------------

  out <- post_processing(out)


  ## Add a centroid vector column -------------------------------------------

  out <-
    map_over_scales(
      all_scales = out,
      fun = \(scale_df = scale_df, scale_name = scale_name, ...) {
        df <- sf::st_make_valid(scale_df)

        if (scale_name %in% c("street", "building")) {
          return(df)
        }

        centroids <- suppressWarnings(lapply(
          sf::st_centroid(df)$geometry,
          sf::st_coordinates
        ))
        centroids <- lapply(centroids, as.vector)
        df$centroid <- centroids
        df
      }
    )


  # Return the final output -------------------------------------------------

  return(out)
}
