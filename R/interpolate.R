#' Interpolate from a census geometry to all scales above
#'
#' @param data <`data.frame`> Containing any number of column with data,
#'  and an ID that corresponds to the base scale, e.g. \code{"DA_ID"}.
#' @param base_scale <`character`> The census scale at which the data is arranged.
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#' @param weight_by <`character`> The denominator for which data should be
#' interpolated. Defaults to `households`. The other option is `population`.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param existing_census_scales <`character vector`> Which are the census scales
#' existing in the project. To detect for which scales data needs to be intersected.
#' (The boundaries of the census geometries fit into each other, but this is not
#' the case for the other geometries, hence the need to identify them).
#'
#' @return Returns a list of length 2. The first is the same list that is fed in
#' `all_scales`, with the columns from `data` interpolated in. The second
#' is an interpolated reference, to know for which scales the data has been
#' interpolated.
#' @export
interpolate_from_census_geo <- function(data, base_scale, all_scales,
                                        weight_by = "households", crs,
                                        existing_census_scales =
                                          c("CSD", "CT", "DA", "DB")) {

  ## Only interpolate for bigger geometries than the base one ----------------

  all_tables <- susbuildr::reconstruct_all_tables(all_scales)
  construct_for <-
    lapply(all_tables, \(scales) scales[seq_len(which(scales == base_scale))])
  scales_to_interpolate <-
    mapply(\(scales, kept_scales) {
      scales[names(scales) %in% kept_scales]
    }, all_scales, construct_for, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  ## Interpolate over all the scales -----------------------------------------

  interpolated <-
    sapply(scales_to_interpolate, \(scales) {

      # Get the base scale and clean up columns
      base <- merge(scales[[base_scale]], data, by = paste0(base_scale, "_ID"))
      base <- sf::st_transform(base, crs)
      base$area <- susbuildr::get_area(base$geometry)
      base <- sf::st_set_agr(base, "constant")
      ids <- names(base)[grepl("_ID$", names(base))]
      other_cols <- names(base)[names(base) %in% c(weight_by, names(data))]
      other_cols <- other_cols[!other_cols %in% ids]
      base <- base[, c(ids, other_cols, "area")]

      # Get only data column names
      data_col_names <- names(data)[!grepl("ID$", names(data))]

      # Interpolate to other census scales --------------------------------------

      census_interpolated <-
        mapply(\(scale_name, scale_df) {
          # If the scale is already the one containing data, merge and return
          if (scale_name == base_scale) {
            return(merge(scale_df, data, by = paste0(base_scale, "_ID")))
          }
          # If the scale is not a census scale, do nothing
          if (!scale_name %in% existing_census_scales) {
            return(scale_df)
          }

          # Preparation
          scale_id <- paste0(scale_name, "_ID")
          from <- sf::st_drop_geometry(base)

          # Group by scale_id, and calculate a weighted.mean using the weight_by
          # argument.
          summarized <-
            lapply(data_col_names, \(col_name) {
              dat <- stats::ave(
                from, from[[scale_id]],
                FUN = \(x) stats::weighted.mean(x[[col_name]], x[[weight_by]],
                  na.rm = TRUE
                )
              )[col_name]
              dat[[scale_id]] <- from[[scale_id]]
              # Get unique values per zone
              unique(dat)
            })

          # Merge all data out of the weighted averages
          out <- if (length(summarized) > 1) {
            do.call(merge, summarized)
          } else {
            summarized[[1]]
          }

          # Merge to the existing data
          merge(scale_df, out, by = scale_id)
        }, names(scales), scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)

      # Interpolate to non-census scales ----------------------------------------

      non_census_scales <- names(scales)[!names(scales) %in% existing_census_scales]
      if (length(non_census_scales) == 0) {
        return(census_interpolated)
      }

      all_scales_interpolated <-
        mapply(\(scale_name, scale_df) {
          # If the scale is a census scale, just return it
          if (!scale_name %in% non_census_scales) {
            return(scale_df)
          }

          # Preparation
          scale_df <- sf::st_transform(scale_df, crs)
          scale_df <- sf::st_set_agr(scale_df, "constant")
          trim_df <- scale_df[, names(scale_df)[names(scale_df) != weight_by]]
          trim_base <- base[, c(data_col_names, weight_by, "area")]

          # Intersect and calculate area proportion
          intersected <- sf::st_intersection(trim_df, trim_base)
          intersected$area_prop <-
            get_area(intersected$geometry) / intersected$area

          # Proportion of 'weight_by' in the base scale
          intersected$n_weight_by <- intersected[[weight_by]] * intersected$area_prop

          # Group by ID, and calculate a weighted.mean using the weight_by argument.
          intersected <- sf::st_drop_geometry(intersected)
          summarized <-
            lapply(data_col_names, \(col_name) {
              dat <- stats::ave(
                intersected, intersected$ID,
                FUN = \(x) stats::weighted.mean(x[[col_name]], x[["n_weight_by"]],
                  na.rm = TRUE
                )
              )[col_name]
              dat$ID <- intersected$ID
              # Get unique values per zone
              unique(dat)
            })

          # Merge all data out of the weighted averages
          out <- if (length(summarized) > 1) {
            do.call(merge, summarized)
          } else {
            summarized[[1]]
          }

          # Merge to the existing data
          merge(scale_df, out, by = "ID")
        }, names(census_interpolated), census_interpolated,
        SIMPLIFY = FALSE, USE.NAMES = TRUE
        )

      all_scales_interpolated
    }, simplify = FALSE, USE.NAMES = TRUE)


  ## Reorder all columns ----------------------------------------------------

  interpolated <- susbuildr::reorder_columns(all_scales = interpolated)


  ## Get the CRS back to WGS 84 ---------------------------------------------

  interpolated <-
    susbuildr::map_over_scales(
      all_scales = interpolated,
      fun = \(scale_df = scale_df, ...) sf::st_transform(scale_df, 4326)
    )


  ## Reattach non-interpolated scales ---------------------------------------

  all_scales_reattached <-
    mapply(\(needed_tables, interpolated_tables, all_tables) {
      sapply(needed_tables, \(table_name) {
        if (table_name %in% names(interpolated_tables)) {
          return(interpolated_tables[[table_name]])
        }
        return(all_tables[[table_name]])
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, all_tables, interpolated, all_scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  ## Create interpolated references ------------------------------------------

  interpolated_ref <-
    sapply(construct_for, \(scales) {
      sapply(scales, \(scale) {
        if (scale != base_scale) {
          return(base_scale)
        }
        return(FALSE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)


  ## Return ------------------------------------------------------------------

  return(list(
    scales = all_scales_reattached,
    interpolated_ref = interpolated_ref
  ))
}
