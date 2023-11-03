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

  # Remove rows with NA entry
  df <- df[!apply(is.na(df[c(value_col, weight_col)]), 1, any), ]

  # Split the data by IDs
  split_data <- split(df, df[[id_col]])

  # Calculate the weighted average for each group
  weighted_avgs <- sapply(split_data, function(x) {
    stats::weighted.mean(x[[value_col]], x[[weight_col]], na.rm = TRUE)
  })

  # Return the results as a data frame
  result <- data.frame(ID = names(weighted_avgs),
                       avg = as.vector(weighted_avgs))
  names(result)[2] <- value_col

  return(result)
}

#' Fast Summation for Interpolation
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
#' column in the input data frame. Used only when the weight_col is using areas.
#' Defaults to NULL.
#'
#' @return A data frame with two columns: the ID column (with the same name as
#' in the input data frame) and a column containing the summed values, named
#' after the input col_name.
interpolate_fast_additive_sum <- function(col_name, data, id_col,
                                          weight_col = NULL) {

  # Remove rows with NA entry
  data <- data[!apply(is.na(data[c(col_name, if (!is.null(weight_col)) weight_col)]), 1, any), ]

  # Extract the necessary columns
  col_df <- data[c(id_col, col_name)]

  # Weight by
  if (!is.null(weight_col)) {
    col_df[[col_name]] <- col_df[[col_name]] * data[[weight_col]]
  }

  # Calculate the sum for each group
  summed_data <- stats::aggregate(col_df[, col_name],
                                  by = list(col_df[[id_col]]),
                                  FUN = sum, na.rm = TRUE
  )

  # Rename the columns
  colnames(summed_data) <- c("ID", col_name)

  # Return the results as a data frame
  return(summed_data)
}

#' Determine which scales are greater in size than the base scale
#'
#' This function compares the average size of a base scale to other scales,
#' transforming them to the same coordinate reference system (CRS) for an accurate
#' comparison. It then returns the names of the scales which are greater than
#' (or very close to) the base scale, including itself..
#'
#' @param base_scale <`data.frame`> A spatial data frame representing the base
#' scale to be compared.
#' @param all_scales <`named list`> A list of spatial data frames representing
#' the scales to be compared.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return A character vector containing the names of the scales that are greater
#' in size than the base scale.
#' @export
scales_greater_than <- function(base_scale, all_scales, crs) {

  greater_than <- sapply(all_scales, \(scale_df) {
    base_scale <- sf::st_transform(base_scale, crs)
    scale_df <- sf::st_transform(scale_df, crs)

    # scale_df got area?
    area <- if ("area" %in% names(scale_df)) scale_df$area else get_area(scale_df)

    base_dat_avg_size <- mean(get_area(base_scale), na.rm = TRUE)
    scale_df_avg_size <- mean(area, na.rm = TRUE)

    base_dat_avg_size <= scale_df_avg_size * 1.01
  }, simplify = TRUE, USE.NAMES = FALSE)

  greater_than <- names(greater_than)[greater_than]
  greater_than
}

#' Interpolate Variables from a Census Geometry to All Higher Scales
#'
#' @param data <`data.frame`> Containing any number of column with data,
#'  and an ID that corresponds to the base scale, e.g. \code{"DA_ID"}.
#' @param base_scale <`character`> The census scale at which the data is
#' arranged.
#' @param all_scales <`named list`> A named list of scales.
#' @param weight_by <`character`> The denominator for which average variables
#' should be interpolated. Defaults to `households`. The other options are
#' `population` and `area`. If `area`,
#' \code{\link[cc.buildr]{interpolate_custom_geo}} will be used.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param existing_census_scales <`character vector`> Which are the census
#' scales existing in the project. To detect for which scales data needs to be
#' intersected. (The boundaries of the census geometries fit into each other,
#' but this is not the case for the other geometries, hence the need to
#' identify them).
#' @param average_vars <`character vector`> Corresponds to the column names
#' of the variables that are to be interpolated as an average, like a
#' percentage, a median, an index, etc. weighted by the `weight_by` argument.
#' @param additive_vars <`character vector`> Corresponds to the column names of
#' the variables that are 'count' variables. In the case of this function, the
#' output value of a CSD would be the sum of the values of the DAs or CTs that
#' are present inside the CSD.
#' @param only_scales <`character vector`> All the scales for which data should
#' be interpolated and appended. Defults to all the scales in `all_scales`.
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
                                        average_vars = c(),
                                        additive_vars = c(),
                                        only_scales = names(all_scales)) {
  ## Catch errors
  if (!paste0(base_scale, "_ID") %in% names(data)) {
    stop(paste0(
      "Census scale must be clear in the column name of the ",
      "identifier, e.g. `DA_ID`."
    ))
  }

  ## Only interpolate for bigger geometries than the base one
  scales_inter <- all_scales[names(all_scales) %in% only_scales]
  base_scale_df <- scales_inter[[base_scale]]
  construct_for <- scales_greater_than(base_scale = base_scale_df,
                                       all_scales = scales_inter,
                                       crs = crs)

  ## In the case weight_by is `area`
  if (weight_by == "area") {
    return({
      data_sf <- all_scales[[base_scale]]["ID"]
      base_scale_id <- sprintf("%s_ID", base_scale)
      names(data_sf)[1] <- base_scale_id
      data_sf <- merge(data_sf, sf::st_drop_geometry(data), by = base_scale_id, all = TRUE)

      interpolate_custom_geo(
        data = data_sf,
        all_scales = all_scales,
        crs = crs,
        only_regions = only_regions,
        average_vars = average_vars,
        additive_vars = additive_vars,
        name_interpolate_from = base_scale,
        construct_for = construct_for
      )
    })
  }

  ## Which are the scales that should be interpolated (as tibble)
  scales_to_interpolate <- all_scales[which(names(all_scales) %in% construct_for)]

  ## Interpolate over all the scales

  # Get the base scale and clean up columns
  base <-
    merge(all_scales[[base_scale]], data, by = paste0(base_scale, "_ID"))
  base <- sf::st_transform(base, crs)
  base$area <- get_area(base$geometry)
  base <- sf::st_set_agr(base, "constant")
  ids <- names(base)[grepl("_ID$", names(base))]
  other_cols <- names(base)[names(base) %in% c(weight_by, names(data))]
  other_cols <- other_cols[!other_cols %in% ids]
  base <- base[, c(ids, other_cols, "area")]

  # Get only data column names
  data_col_names <- names(data)[!grepl("ID$", names(data))]

  # Start with th census scales
  census_scales <-
    names(scales_to_interpolate)[names(scales_to_interpolate) %in% existing_census_scales]
  census_scales <- all_scales[census_scales]

  census_interpolated <-
    mapply(\(scale_name, scale_df) {
      # If the scale is already the one containing data, merge and return
      if (scale_name == base_scale) {
        return(merge(scale_df, data,
                     by = paste0(base_scale, "_ID"),
                     all.x = TRUE
        ))
      }

      # Preparation
      scale_id <- paste0(scale_name, "_ID")
      from <- sf::st_drop_geometry(base)

      summarized_avg <- lapply(average_vars, \(col_name) {
        # Extract the necessary columns
        col_df <- from[c(scale_id, weight_by, col_name)]

        # Calculate the weighted average for the current column
        interpolate_fast_weighted_mean(col_df, scale_id, weight_by,
                                       col_name)
      })

      # Calculate the sum for each column using lapply
      summarized_add <- lapply(additive_vars,
                               interpolate_fast_additive_sum,
                               data = from, id_col = scale_id)

      # Concatenate both
      summarized <- c(summarized_avg, summarized_add)

      # Merge all data out of the weighted averages
      out <- if (length(summarized) > 1) {
        # Create a function to not always use 'merge', as it can be quite
        # slow when there are many datasets. Filter out NAs from both left
        # and right, look if the ID columns are identical and if they are,
        # use cbind by taking out the ID column of the right table. If the
        # ID column is not identical, use the merge function.
        merg_ <- function(x, y) {
          x <- x[!is.na(x$ID), ]
          y <- y[!is.na(y$ID), ]
          if (identical(x$ID, y$ID)) {
            cbind(x, y[2])
          } else {
            merge(x, y, by = "ID")
          }
        }
        Reduce(merg_, summarized)
      } else {
        summarized[[1]]
      }

      # Merge to the existing data
      merge(scale_df, out, by = "ID", all.x = TRUE)

    }, names(census_scales), census_scales)


  # Interpolate to non-census scales
  non_census_scales <-
    names(scales_to_interpolate)[!names(scales_to_interpolate) %in% existing_census_scales]

  non_census_scales <- all_scales[non_census_scales]

  non_census_interpolated <-
    mapply(
      \(scale_name, scale_df) {

        # Preparation
        scale_id <- paste0(scale_name, "_ID")
        scale_df <- sf::st_transform(scale_df, crs)
        scale_df <- sf::st_set_agr(scale_df, "constant")
        trim_df <- scale_df[, names(scale_df)[
          names(scale_df) != weight_by]]
        trim_base <- base[, c(data_col_names, weight_by, "area")]

        # Intersect and calculate area proportion
        intersected <- sf::st_intersection(trim_df, trim_base)
        intersected$area_prop <-
          get_area(intersected$geometry) / intersected$area

        # Proportion of 'weight_by' in the base scale
        intersected$n_weight_by <-
          intersected[[weight_by]] * intersected$area_prop

        # Group by ID, and calculate a weighted.mean using `weight_by`
        intersected <- sf::st_drop_geometry(intersected)

        summarized_avg <- lapply(average_vars, \(col_name) {
          # Extract the necessary columns
          col_df <- intersected[c(scale_id, weight_by, col_name)]

          # Calculate the weighted average for the current column
          interpolate_fast_weighted_mean(df = col_df, id_col = scale_id,
                                         weight_col = weight_by,
                                         value_col = col_name)
        })

        # Calculate the sum for each column using lapply
        summarized_add <- lapply(additive_vars,
                                 interpolate_fast_additive_sum,
                                 data = intersected, id_col = scale_id)

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
            } else {
              merge(x, y, by = "ID")
            }
          }
          Reduce(merg_, summarized)
        } else {
          summarized[[1]]
        }

        # Merge to the existing data
        merge(scale_df, out, by = "ID", all.x = TRUE)
      }, names(non_census_scales), non_census_scales,
      SIMPLIFY = FALSE, USE.NAMES = TRUE
    )

  interpolated <- c(census_interpolated, non_census_interpolated)


  ## Reorder all columns
  interpolated <- lapply(interpolated, reorder_columns)


  ## Get the CRS back to WGS 84
  interpolated <- lapply(interpolated, sf::st_transform, 4326)


  ## Reattach non-interpolated scales
  missing_scales <- all_scales[!names(all_scales) %in% names(interpolated)]
  all_scales_reattached <- c(interpolated, missing_scales)


  ## Scales at which the data is available
  avail_scale <- names(interpolated)


  ## Create interpolated references as a data.frame
  interpolated_ref <- tibble::tibble(
    scale = names(interpolated),
    interpolated_from = rep(base_scale, length(interpolated))
  )
  interpolated_ref$interpolated_from[interpolated_ref$scale == base_scale] <- FALSE

  ## Return
  return(list(
    scales = all_scales_reattached,
    avail_scale = avail_scale,
    interpolated_ref = interpolated_ref
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
  das$DA_area <- get_area(das$geometry)
  # Add new table area
  destination <- sf::st_transform(to, crs)
  destination <-
    destination[, names(destination)[!names(destination) %in% additive_vars]]
  intersected_table <- suppressWarnings(sf::st_intersection(destination, das))
  intersected_table$new_area <- get_area(intersected_table$geometry)

  # Get proportion of area per zone
  weight_by <- "area_prop"
  intersected_table[[weight_by]] <-
    intersected_table$new_area / intersected_table$DA_area
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
    weight_col = weight_by
  )

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
      } else {
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

#' Interpolate variables from any geometry to all scales bigger in area
#'
#' Used to interpolate any polygons to all the used scale in the instance of
#' Curbcut. Data is interpolated to a particular scale if the average size
#' of the `data` polygons is smaller than the average size of any scale in
#' `all_scales`.
#'
#' @param data <`data.frame`> Containing any number of column with data,
#' and an ID that corresponds to the source scale, e.g. \code{"DA_ID"}.
#' @param name_interpolate_from <`character`> The name of the scale from which
#' data has been interpolated, e.g. `DA`. If it is an entry in the scales_dictionary,
#' the entry of the `plur` column will be used as text. If it isn't, the character
#' of `name_interpolate_from` will be used, e.g.
#' `...has been spatially interpolated from green space polygons` where
#' `green space polygons` would be the value of `name_interpolate_from`.
#' @param all_scales <`named list`> A named list of scales.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param only_scales <`character vector`> All the scales for which data should
#' be interpolated and appended. Defults to all the scales in `all_scales`.
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
                                   only_scales = names(all_scales),
                                   average_vars = c(),
                                   additive_vars = c(),
                                   name_interpolate_from,
                                   construct_for = NULL) {

  if (is.null(construct_for)) {
    ## Only interpolate for bigger geometries than the base one
    scales_inter <- all_scales[names(all_scales) %in% only_scales]
    construct_for <- scales_greater_than(base_scale = data,
                                         all_scales = scales_inter,
                                         crs = crs)
  }

  ## Scales to interpolate
  scales_to_interpolate <- all_scales[names(all_scales) %in% construct_for]

  ## Interpolate over all the scales
  interpolated <- lapply(scales_to_interpolate, \(scale_df) {
    interpolate_from_area(
      to = scale_df,
      from = data,
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs
    )
  })


  ## Reorder all columns
  interpolated <- lapply(interpolated, reorder_columns)


  ## Get the CRS back to WGS 84
  interpolated <- lapply(interpolated, sf::st_transform, 4326)


  ## Reattach non-interpolated scales
  missing_scales <- all_scales[!names(all_scales) %in% names(interpolated)]
  all_scales_reattached <- c(interpolated, missing_scales)


  ## Scales at which the data is available
  avail_scale <- names(interpolated)


  ## Create interpolated references as a data.frame
  interpolated_ref <- tibble::tibble(
    scale = names(interpolated),
    interpolated_from = rep(name_interpolate_from, length(interpolated))
  )
  interpolated_ref$interpolated_from[interpolated_ref$scale == name_interpolate_from] <- FALSE


  ## Return
  return(list(
    scales = all_scales_reattached,
    avail_scale = avail_scale,
    interpolated_ref = interpolated_ref
  ))
}
