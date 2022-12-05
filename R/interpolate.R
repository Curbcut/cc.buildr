#' Interpolate variables from a census geometry to all scales above
#'
#' @param data <`data.frame`> Containing any number of column with data,
#'  and an ID that corresponds to the base scale, e.g. \code{"DA_ID"}.
#' @param base_scale <`character`> The census scale at which the data is arranged.
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#' @param weight_by <`character`> The denominator for which average variables should
#' be interpolated. Defaults to `households`. The other option is `population`.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param existing_census_scales <`character vector`> Which are the census scales
#' existing in the project. To detect for which scales data needs to be intersected.
#' (The boundaries of the census geometries fit into each other, but this is not
#' the case for the other geometries, hence the need to identify them).
#' @param average_vars <`character vector`> Corresponds to the column names
#' of the variables that are to be interpolated as an average, like a percentage,
#' a median, an index, etc. weighted by the `weight_by` argument.
#' @param additive_vars <`character vector`> Corresponds to the column names of
#' the variables that are 'count' variables. In the case of this function, the output
#' value of a CSD would be the sum of the values of the DAs or CTs that are present
#' inside the CSD.
#'
#' @return Returns a list of length 3. The first is the same list that is fed in
#' `all_scales`, with the columns from `data` interpolated in. The second is
#' a data.frame of scales reference, to know for which scales the data is
#' available. It can directly be fed to the \code{scales}
#' argument of \code{\link[cc.buildr]{add_variable}}.The third
#' is a data.frame of interpolation reference, to know for which scales the data
#' has been interpolated. It can directly be fed to the \code{interpolated}
#' argument of \code{\link[cc.buildr]{add_variable}}.
#' @export
interpolate_from_census_geo <- function(data, base_scale, all_scales,
                                        weight_by = "households", crs,
                                        existing_census_scales =
                                          c("CSD", "CT", "DA", "DB"),
                                        average_vars = c(),
                                        additive_vars = c()) {

  ## Only interpolate for bigger geometries than the base one
  all_tables <- reconstruct_all_tables(all_scales)
  construct_for <-
    lapply(all_tables, \(scales)  {
      if (!base_scale %in% scales) {
        return()
      }
      scales[seq_len(which(scales == base_scale))]
    })
  scales_to_interpolate <-
    mapply(\(scales, kept_scales) {
      scales[names(scales) %in% kept_scales]
    }, all_scales, construct_for, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  scales_to_interpolate <-
    scales_to_interpolate[sapply(scales_to_interpolate, \(x) length(x) > 0)]

  ## Interpolate over all the scales
  interpolated <-
    future.apply::future_sapply(scales_to_interpolate, \(scales) {
      if (length(scales) == 0) {
        return()
      }

      # Get the base scale and clean up columns
      base <-
        merge(scales[[base_scale]], data, by = paste0(base_scale, "_ID"))
      base <- sf::st_transform(base, crs)
      base$area <- get_area(base$geometry)
      base <- sf::st_set_agr(base, "constant")
      ids <- names(base)[grepl("_ID$", names(base))]
      other_cols <- names(base)[names(base) %in% c(weight_by, names(data))]
      other_cols <- other_cols[!other_cols %in% ids]
      base <- base[, c(ids, other_cols, "area")]

      # Get only data column names
      data_col_names <- names(data)[!grepl("ID$", names(data))]

      # Interpolate to other census scales
      census_interpolated <-
        mapply(\(scale_name, scale_df) {
          # If the scale is already the one containing data, merge and return
          if (scale_name == base_scale) {
            return(merge(scale_df, data,
              by = paste0(base_scale, "_ID"),
              all.x = TRUE
            ))
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
          # TKTK Review if I can send just column values in the lapply instead of
          # retrieving the whole dataframe each time
          data_col_names_avg <- data_col_names[data_col_names %in% average_vars]
          summarized_avg <-
            lapply(data_col_names_avg, \(col_name) {
              from <- as.data.frame(from)
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

          # Group by scale_id and calculate a count
          data_col_names_add <- data_col_names[data_col_names %in% additive_vars]
          summarized_add <-
            lapply(data_col_names_add, \(col_name) {
              from <- as.data.frame(from)
              dat <- stats::ave(
                from, from[[scale_id]],
                FUN = \(x) sum(x[[col_name]], na.rm = TRUE)
              )[col_name]
              dat[[scale_id]] <- from[[scale_id]]
              # Get unique values per zone
              unique(dat)
            })

          summarized <- c(summarized_avg, summarized_add)

          # Merge all data out of the weighted averages
          out <- if (length(summarized) > 1) {
            Reduce(merge, summarized)
          } else {
            summarized[[1]]
          }

          # Merge to the existing data
          merge(scale_df, out, by = scale_id, all.x = TRUE)
        }, names(scales), scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)

      # Interpolate to non-census scales

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
          data_col_names_avg <- data_col_names[data_col_names %in% average_vars]
          summarized_avg <-
            lapply(data_col_names_avg, \(col_name) {
              intersected <- as.data.frame(intersected)
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

          data_col_names_add <- data_col_names[data_col_names %in% additive_vars]
          summarized_add <-
            lapply(data_col_names_add, \(col_name) {
              intersected <- as.data.frame(intersected)
              intersected[[col_name]] <-
                intersected[[col_name]] * intersected$area_prop
              dat <- stats::ave(
                intersected[c("ID", col_name)], intersected$ID,
                FUN = \(x) sum(x[[col_name]], na.rm = TRUE))[col_name]
              dat$ID <- intersected$ID
              # Get unique values per zone
              unique(dat)
            })

          summarized <- c(summarized_avg, summarized_add)

          # Merge all data out of the weighted averages
          out <- if (length(summarized) > 1) {
            Reduce(merge, summarized)
          } else {
            summarized[[1]]
          }

          # Merge to the existing data
          merge(scale_df, out, by = "ID", all.x = TRUE)
        }, names(census_interpolated), census_interpolated,
        SIMPLIFY = FALSE, USE.NAMES = TRUE
        )

      all_scales_interpolated
    }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)


  ## Reorder all columns
  interpolated <-
    reorder_columns(all_scales = interpolated)


  ## Get the CRS back to WGS 84
  interpolated <-
    map_over_scales(
      all_scales = interpolated,
      fun = \(scale_df = scale_df, ...) sf::st_transform(scale_df, 4326)
    )


  ## Reattach non-interpolated scales
  all_scales_reattached <-
    mapply(\(needed_tables, interpolated_tables, all_tables) {
      sapply(needed_tables, \(table_name) {
        if (table_name %in% names(interpolated_tables)) {
          return(interpolated_tables[[table_name]])
        }
        return(all_tables[[table_name]])
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, all_tables, interpolated, all_scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  ## Scales at which the data is available
  avail_scales <-
    map_over_scales(interpolated,
      fun = \(geo = geo, scale_name = scale_name, ...) {
        tibble::tibble(
          geo = geo,
          scale = scale_name
        )
      }
    )
  avail_scales <-
    sapply(avail_scales, \(x) do.call(rbind, x),
      simplify = FALSE, USE.NAMES = TRUE
    ) |>
    (\(x) do.call(rbind, x))()
  row.names(avail_scales) <- NULL


  ## Create interpolated references as a data.frame
  interpolated_ref <-
    sapply(construct_for, \(scales) {
      sapply(scales, \(scale) {
        if (scale != base_scale) {
          return(base_scale)
        }
        return(FALSE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  interpolated_ref <-
    map_over_scales(interpolated_ref,
      fun = \(geo = geo, scale_name = scale_name,
        scale_df = scale_df, ...) {
        tibble::tibble(
          geo = geo,
          scale = scale_name,
          interpolated_from = scale_df
        )
      }
    )
  interpolated_ref <-
    sapply(interpolated_ref, \(x) do.call(rbind, x),
      simplify = FALSE, USE.NAMES = TRUE
    ) |>
    (\(x) do.call(rbind, x))()
  row.names(interpolated_ref) <- NULL


  ## Return
  return(list(
    scales = all_scales_reattached,
    avail_scales = avail_scales,
    interpolated_ref = interpolated_ref
  ))
}

#' Interpolate ADDITIVE variables using area
#'
#' @param to <`sf data.frame`> Table for which additive data must be
#' interpolated
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame from which
#' population and households will be interpolated.
#' @param additive_vars <`vector of character`> Columns from `DA_table` from which
#' data should be interpolated to the `to` data.frame. Must be ADDITIVE variables,
#' e.g. population and households (default).
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return Returns the `to` data.frame with the added or modified columns that
#' have been interpolated from the `DA_table`.
#' @export
interpolate_from_area <- function(to, DA_table,
                                  additive_vars = c("population", "households"),
                                  crs) {

  # Interpolate DA_table to get population and households
  das <- DA_table[, additive_vars]
  das <- sf::st_transform(das, crs)
  das <- sf::st_set_agr(das, "constant")
  # Add DA area
  das$area <- get_area(das$geometry)
  # Add new table area
  destination <- sf::st_transform(to, crs)
  destination <-
    destination[, names(destination)[!names(destination) %in% additive_vars]]
  intersected_table <- suppressWarnings(sf::st_intersection(destination, das))
  intersected_table$new_area <- get_area(intersected_table$geometry)

  # Get proportion of area per zone
  intersected_table$area_prop <- intersected_table$new_area / intersected_table$area
  intersected_table <- sf::st_drop_geometry(intersected_table)

  # Interpolate additive variables
  with_additive_vars <- lapply(additive_vars, \(x) {
    intersected_table[[x]] <- intersected_table$area_prop * intersected_table[[x]]
    intersected_table[x]
  })
  with_additive_vars <-
    cbind(intersected_table["ID"], Reduce(cbind, with_additive_vars))
  interpolated <-
    lapply(unique(destination$ID), \(ID) {
      z <- with_additive_vars[with_additive_vars$ID == ID, ]
      vars <- sapply(additive_vars, \(var) {
        round(sum(z[[var]], na.rm = TRUE))
      }, simplify = FALSE, USE.NAMES = TRUE)
      vars <- Reduce(cbind, vars)

      out <- cbind(tibble::tibble(ID = ID), vars)
      names(out) <- c("ID", additive_vars)
      out
    })
  interpolated <- Reduce(rbind, interpolated)

  # Return
  merge(to[, names(to)[!names(to) %in% additive_vars]], interpolated, by = "ID")
}
