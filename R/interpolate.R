#' Fast Weighted Mean for Interpolation
#'
#' This function computes the weighted mean for each group of IDs interpolated
#' within a data frame. The data frame must have columns for ID, weight, and
#' value. The output is a data frame containing the IDs and their corresponding
#' weighted means.
#'
#' @param df <`data.frame`> A data frame containing the data to be interpolated.
#' It must have columns corresponding to the specified id_col, weight_col, and
#' value_col.
#' @param id_col <`character`> A string representing the name of the ID column
#' in the input data frame.
#' @param weight_col <`character`> A string representing the name of the weight
#' column in the input data frame.
#' @param value_col <`character`> A string representing the name of the value
#' column in the input data frame.
#'
#' @return A data frame with two columns: the ID column (with the same name as
#' in the input data frame) and a column containing the weighted means, named
#' after the input value_col
interpolate_fast_weighted_mean <- function(df, id_col, weight_col, value_col) {
  # Split the data by IDs
  split_data <- split(df, df[[id_col]])

  # Calculate the weighted average for each group
  weighted_avgs <- sapply(split_data, function(x) {
    stats::weighted.mean(x[[value_col]], x[[weight_col]], na.rm = TRUE)
  })

  # Return the results as a data frame
  result <- data.frame(ID = names(weighted_avgs), avg = as.vector(weighted_avgs))
  names(result)[2] <- value_col

  return(result)
}

#' Fast Summation for interpolation
#'
#' This function computes the the summation for each group within a
#' data frame. The data frame must have columns for ID and the column to be
#' summed. The output is a data frame containing the IDs and their corresponding
#' summed values.
#'
#' @param col_name <`character`> A string representing the name of the column
#' to be summed in the input data frame.
#' @param data <`data.frame`> A data frame containing the data to be summed. It
#' must have columns corresponding to the specified id_col and col_name.
#' @param id_col <`character`> A string representing the name of the ID column
#' in the input data frame.
#' @param weight_col <`character`> A string representing the name of the weight
#' column in the input data frame.
#'
#' @return A data frame with two columns: the ID column (with the same name as
#' in the input data frame) and a column containing the summed values, named
#' after the input col_name.
interpolate_fast_additive_sum <- function(col_name, data, id_col, weight_col) {
  # Extract the necessary columns
  col_df <- data[c(id_col, col_name)]

  # Weight by
  col_df[[col_name]] <- col_df[[col_name]] * data[[weight_col]]

  # Calculate the sum for each group
  summed_data <- stats::aggregate(col_df[, col_name],
                                  by = list(col_df[[id_col]]),
                                  FUN = sum, na.rm = TRUE)

  # Rename the columns
  colnames(summed_data) <- c("ID", col_name)

  # Return the results as a data frame
  return(summed_data)
}

#' Interpolate variables from a census geometry to all scales above
#'
#' @param data <`data.frame`> Containing any number of column with data,
#'  and an ID that corresponds to the base scale, e.g. \code{"DA_ID"}.
#' @param base_scale <`character`> The census scale at which the data is arranged.
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#' @param weight_by <`character`> The denominator for which average variables should
#' be interpolated. Defaults to `households`. The other options are `population` and
#' `area`. If `area`, \code{\link[cc.buildr]{interpolate_custom_geo}} will be
#' used.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param existing_census_scales <`character vector`> Which are the census scales
#' existing in the project. To detect for which scales data needs to be intersected.
#' (The boundaries of the census geometries fit into each other, but this is not
#' the case for the other geometries, hence the need to identify them).
#' @param only_regions <`character vector`> All the regions for which data should
#' be interpolated and appended. Defults to all the regions in `all_scales`.
#' @param average_vars <`character vector`> Corresponds to the column names
#' of the variables that are to be interpolated as an average, like a percentage,
#' a median, an index, etc. weighted by the `weight_by` argument.
#' @param additive_vars <`character vector`> Corresponds to the column names of
#' the variables that are 'count' variables. In the case of this function, the output
#' value of a CSD would be the sum of the values of the DAs or CTs that are present
#' inside the CSD.
#'
#' @return Returns a list of length 4. The first is the same list that is fed in
#' `all_scales`, with the columns from `data` interpolated in. The second is
#' a data.frame of scales reference, to know for which scales the data is
#' available. It can directly be fed to the \code{scales}
#' argument of \code{\link[cc.buildr]{add_variable}}.The third
#' is a data.frame of interpolation reference, to know for which scales the data
#' has been interpolated. It can directly be fed to the \code{interpolated}
#' argument of \code{\link[cc.buildr]{add_variable}}. The fourth is a character
#' vector of all regions at which the data will be available.
#' @export
interpolate_from_census_geo <- function(data, base_scale, all_scales,
                                        weight_by = "households", crs,
                                        existing_census_scales =
                                          c("CSD", "CT", "DA", "DB"),
                                        only_regions = names(all_scales),
                                        average_vars = c(),
                                        additive_vars = c()) {
  ## Catch errors
  if (!paste0(base_scale, "_ID") %in% names(data)) {
    stop(paste0(
      "Census scale must be clear in the column name of the ",
      "identifier, e.g. `DA_ID`."
    ))
  }

  ## Only interpolate for bigger geometries than the base one
  all_tables <- reconstruct_all_tables(all_scales)
  all_tables <- all_tables[names(all_tables) %in% only_regions]
  construct_for <-
    lapply(all_tables, \(scales)  {
      if (!base_scale %in% scales) {
        return()
      }
      scales[seq_len(which(scales == base_scale))]
    })

  ## In the case weight_by is `area`
  if (weight_by == "area") {
    return(interpolate_custom_geo(
      data = data,
      all_scales = all_scales,
      crs = crs,
      only_regions = only_regions,
      average_vars = average_vars,
      additive_vars = additive_vars,
      name_interpolate_from = base_scale,
      construct_for = construct_for
    ))
  }


  scales_to_interpolate <- mapply(\(geo, scales) {
    all_scales[[geo]][names(all_scales[[geo]]) %in% scales]
  }, names(construct_for), construct_for, SIMPLIFY = FALSE)
  scales_to_interpolate <-
    mapply(\(scales, kept_scales) {
      scales[names(scales) %in% kept_scales]
    }, scales_to_interpolate, construct_for, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  scales_to_interpolate <-
    scales_to_interpolate[sapply(scales_to_interpolate, \(x) length(x) > 0)]
  ## Interpolate over all the scales
  progressr::with_progress({
    pb <- progressr::progressor(length(scales_to_interpolate))

    interpolated <-
      sapply(scales_to_interpolate, \(scales) {
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

            summarized_avg <- lapply(average_vars, \(col_name) {
              # Extract the necessary columns
              col_df <- from[c(scale_id, weight_by, col_name)]

              # Calculate the weighted average for the current column
              interpolate_fast_weighted_mean(col_df, scale_id, weight_by, col_name)
            })

            # Calculate the sum for each column using lapply
            summarized_add <- lapply(additive_vars, interpolate_fast_additive_sum,
                                     data = from, id_col = scale_id, weight_col = weight_by)

            # Concatenate both
            summarized <- c(summarized_avg, summarized_add)

            # Merge all data out of the weighted averages
            out <- if (length(summarized) > 1) {
              # Create a function to not always use 'merge', as it can be quite slow
              # when there are many datasets. Filter out NAs from both left and right,
              # look if the ID columns are identical and if they are, use cbind by
              # taking out the ID column of the right table. If the ID column is not
              # identical, use the merge function.
              merg_ <- function(x, y) {
                x <- x[!is.na(x$ID), ]
                y <- y[!is.na(y$ID), ]
                if (identical(x$ID, y$ID)) {
                  cbind(x, y[2])
                }
                else {
                  merge(x, y, by = "ID")
                }
              }
              Reduce(merg_, summarized)
            } else {
              summarized[[1]]
            }

            # Merge to the existing data
            merge(scale_df, out, by = "ID", all.x = TRUE)
          }, names(scales), scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)

        # Interpolate to non-census scales
        non_census_scales <- names(scales)[!names(scales) %in% existing_census_scales]
        if (length(non_census_scales) == 0) {
          return(census_interpolated)
        }

        all_scales_interpolated <-
          mapply(
            \(scale_name, scale_df) {
              # If the scale is a census scale, just return it
              if (!scale_name %in% non_census_scales) {
                return(scale_df)
              }

              # Preparation
              scale_id <- paste0(scale_name, "_ID")
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

              summarized_avg <- lapply(average_vars, \(col_name) {
                # Extract the necessary columns
                col_df <- intersected[c(scale_id, weight_by, col_name)]

                # Calculate the weighted average for the current column
                interpolate_fast_weighted_mean(col_df, scale_id, weight_by, col_name)
              })

              # Calculate the sum for each column using lapply
              summarized_add <- lapply(additive_vars, interpolate_fast_additive_sum,
                                       data = intersected, id_col = scale_id, weight_col = weight_by)

              # Concatenate both
              summarized <- c(summarized_avg, summarized_add)

              # Merge all data out of the weighted averages
              out <- if (length(summarized) > 1) {
                # Create a function to not always use 'merge', as it can be quite slow
                # when there are many datasets. Filter out NAs from both left and right,
                # look if the ID columns are identical and if they are, use cbind by
                # taking out the ID column of the right table. If the ID column is not
                # identical, use the merge function.
                merg_ <- function(x, y) {
                  x <- x[!is.na(x$ID), ]
                  y <- y[!is.na(y$ID), ]
                  if (identical(x$ID, y$ID)) {
                    cbind(x, y[2])
                  }
                  else {
                    merge(x, y, by = "ID")
                  }
                }
                Reduce(merg_, summarized)
              } else {
                summarized[[1]]
              }

              # Merge to the existing data
              merge(scale_df, out, by = "ID", all.x = TRUE)
            }, names(census_interpolated), census_interpolated,
            SIMPLIFY = FALSE, USE.NAMES = TRUE
          )

        pb()
        all_scales_interpolated
      }, simplify = FALSE, USE.NAMES = TRUE)
  })

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
    map_over_scales(
      all_scales = all_scales,
      fun = \(geo = geo, scale_name = scale_name, ...) {
        int_df <- interpolated[[geo]][[scale_name]]
        if (!is.null(int_df)) {
          return(int_df)
        }
        all_scales[[geo]][[scale_name]]
      }
    )


  ## Scales at which the data is available
  avail_df <-
    map_over_scales(interpolated,
      fun = \(geo = geo, scale_name = scale_name, ...) {
        paste(geo, scale_name, sep = "_")
      }
    ) |> unlist() |> unname()


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
          df = paste(geo, scale_name, sep = "_"),
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
    avail_df = avail_df,
    interpolated_ref = interpolated_ref,
    regions = names(construct_for)
  ))
}

#' Interpolate variables using area
#'
#' @param to <`sf data.frame`> Table for which data must be interpolated
#' @param from <`sf data.frame`> A \code{DA} sf data.frame from which
#' variables will be interpolated.
#' @param average_vars <`character vector`> Corresponds to the column names
#' of the variables that are to be interpolated as an average, like a percentage,
#' a median, an index, etc. weighted by the `weight_by` argument.
#' @param additive_vars <`character vector`> Corresponds to the column names of
#' the variables that are 'count' variables. In the case of this function, the output
#' value of a CSD would be the sum of the values of the DAs or CTs that are present
#' inside the CSD.
#' @param round_additive <`logical`> If addive variables should be rounded,
#' e.g. the population or count of households.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return Returns the `to` data.frame with the added or modified columns that
#' have been interpolated from the `from`.
#' @export
interpolate_from_area <- function(to, from,
                                  average_vars = c(),
                                  additive_vars = c(),
                                  round_additive = TRUE,
                                  crs) {
  # Interpolate the `from` table to get additive variables
  das <- from[, c(average_vars, additive_vars)]
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
  weight_by <- "area_prop"
  intersected_table[[weight_by]] <-
    intersected_table$new_area / intersected_table$area
  intersected_table <- sf::st_drop_geometry(intersected_table)

  summarized_avg <- lapply(average_vars, \(col_name) {
    # Extract the necessary columns
    col_df <- intersected_table[c("ID", weight_by, col_name)]

    # Calculate the weighted average for the current column
    interpolate_fast_weighted_mean(col_df, "ID", weight_by, col_name)
  })

  # Calculate the sum for each column using lapply
  summarized_add <- lapply(additive_vars, interpolate_fast_additive_sum,
                           data = intersected_table, id_col = "ID",
                           weight_col = weight_by)

  # Concatenate both
  summarized <- c(summarized_avg, summarized_add)

  # Merge all data out of the weighted averages
  out <- if (length(summarized) > 1) {
    # Create a function to not always use 'merge', as it can be quite slow
    # when there are many datasets. Filter out NAs from both left and right,
    # look if the ID columns are identical and if they are, use cbind by
    # taking out the ID column of the right table. If the ID column is not
    # identical, use the merge function.
    merg_ <- function(x, y) {
      x <- x[!is.na(x$ID), ]
      y <- y[!is.na(y$ID), ]
      if (identical(x$ID, y$ID)) {
        cbind(x, y[2])
      }
      else {
        merge(x, y, by = "ID")
      }
    }
    Reduce(merg_, summarized)
  } else {
    summarized[[1]]
  }

  # Return
  merge(to[, names(to)[!names(to) %in% c(additive_vars, average_vars)]], out,
    by = "ID", all = TRUE
  )
}

#' Interpolate variables from a census geometry to all scales above
#'
#' Used to interpolate any polygons to all the used scale in the instance of
#' Curbcut. Data is interpolated to a particular scale if the average size
#' of the `data` polygons is smaller than the average size of any scale in
#' `all_scales`.
#'
#' @param data <`data.frame`> Containing any number of column with data,
#' and an ID that corresponds to the source scale, e.g. \code{"DA_ID"}.
#' @param name_interpolate_from <`character`> The name of the scale from which
#' data has been interpolated, e.g. `grid`. It MUST be an entry in the
#' scales_dictionary.
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param only_regions <`character vector`> All the regions for which data should
#' be interpolated and appended. Defults to all the regions in `all_scales`.
#' @param average_vars <`character vector`> Corresponds to the column names
#' of the variables that are to be interpolated as an average, like a percentage,
#' a median, an index, etc. weighted by the `weight_by` argument.
#' @param additive_vars <`character vector`> Corresponds to the column names of
#' the variables that are 'count' variables. In the case of this function, the output
#' value of a CSD would be the sum of the values of the DAs or CTs that are present
#' inside the CSD.
#' @param construct_for <`list`> A list where each region is a level, and
#' each of them has a vector character of scales for which the `data` should be
#' interpolated to. Used by \code{\link[cc.buildr]{interpolate_from_census_geo}}.
#' It bypasses the process where `interpolate_custom_geo` only interpolates
#' for geometries bigger than the data.
#'
#' @return Returns a list of length 4. The first is the same list that is fed in
#' `all_scales`, with the columns from `data` interpolated in. The second is
#' a data.frame of scales reference, to know for which scales the data is
#' available. It can directly be fed to the \code{scales}
#' argument of \code{\link[cc.buildr]{add_variable}}.The third
#' is a data.frame of interpolation reference, to know for which scales the data
#' has been interpolated. It can directly be fed to the \code{interpolated}
#' argument of \code{\link[cc.buildr]{add_variable}}. The fourth is a character
#' vector of all regions at which the data will be available.
#' @export
interpolate_custom_geo <- function(data, all_scales, crs,
                                   only_regions = names(all_scales),
                                   average_vars = c(),
                                   additive_vars = c(),
                                   name_interpolate_from,
                                   construct_for = NULL) {
  ## Only interpolate for geometries bigger than the data one
  all_tables <- reconstruct_all_tables(all_scales)
  all_tables <- all_tables[names(all_tables) %in% only_regions]

  # If construct_for is NULL
  if (is.null(construct_for)) {
    construct_for <- all_scales[names(all_scales) %in% only_regions]
    construct_for <- map_over_scales(
      all_scales = construct_for,
      fun = \(geo = geo, scales = scales,
        scale_df = scale_df, scale_name = scale_name) {
        data <- sf::st_transform(data, crs)
        scale_df <- sf::st_transform(scale_df, crs)
        if (mean(get_area(data), na.rm = TRUE) <=
          mean(get_area(scale_df), na.rm = TRUE)) {
          scale_name
        } else {
          return()
        }
      }
    )
    construct_for <- lapply(construct_for, \(x) unlist(x, use.names = FALSE))
  }

  scales_to_interpolate <- mapply(\(geo, scales) {
    all_scales[[geo]][names(all_scales[[geo]]) %in% scales]
  }, names(construct_for), construct_for, SIMPLIFY = FALSE)
  scales_to_interpolate <-
    mapply(\(scales, kept_scales) {
      scales[names(scales) %in% kept_scales]
    }, scales_to_interpolate, construct_for, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  scales_to_interpolate <-
    scales_to_interpolate[sapply(scales_to_interpolate, \(x) length(x) > 0)]

  ## Interpolate over all the scales
  interpolated <- map_over_scales(
    all_scales = scales_to_interpolate,
    fun = \(geo = geo, scales = scales,
      scale_df = scale_df, scale_name = scale_name) {
      interpolate_from_area(
        to = scale_df,
        from = data,
        average_vars = average_vars,
        additive_vars = additive_vars,
        crs = crs
      )
    }
  )

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
    map_over_scales(
      all_scales = all_scales,
      fun = \(geo = geo, scale_name = scale_name, ...) {
        int_df <- interpolated[[geo]][[scale_name]]
        if (!is.null(int_df)) {
          return(int_df)
        }
        all_scales[[geo]][[scale_name]]
      }
    )


  ## Scales at which the data is available
  avail_df <-
    map_over_scales(interpolated,
                    fun = \(geo = geo, scale_name = scale_name, ...) {
                      paste(geo, scale_name, sep = "_")
                    }
    ) |> unlist() |> unname()


  ## Create interpolated references as a data.frame
  interpolated_ref <-
    sapply(construct_for, \(scales) {
      sapply(scales, \(scale) {
        if (scale != name_interpolate_from) {
          return(name_interpolate_from)
        }
        return(FALSE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  interpolated_ref <-
    map_over_scales(interpolated_ref,
      fun = \(geo = geo, scale_name = scale_name,
        scale_df = scale_df, ...) {
        tibble::tibble(
          df = paste(geo, scale_name, sep = "_"),
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
    avail_df = avail_df,
    interpolated_ref = interpolated_ref,
    regions = names(construct_for)
  ))
}
