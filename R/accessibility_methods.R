#' Get number of point per DA
#'
#' Takes a named list of `sf` points. The name of the list is the name of the
#' variable. We use this function to know how many `X` are in each DA.
#'
#' @param point_data <`named list`> List of `sf` dataframes. The only thing
#' that's used is the spatial features of each element of the list. Spatial
#' features must be points. The names of each element of the list is the name
#' of the variables.
#' @param DA_table A \code{DA} sf data.frame which will be used to intersect
#' each element of the `point_data` list.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return Outputs a single dataframe with the same number of rows as there
#' are in `DA_table`. Every element of the `point_data` list is a column in the
#' output, and the name of the column is the name of each element of the
#' `point_data` list.
#' @export
accessibility_point_per_DA <- function(point_data, DA_table, crs) {


  # Prepare data ------------------------------------------------------------

  point_data <- lapply(point_data, sf::st_transform, crs)
  DA_table <- sf::st_transform(DA_table, crs)["ID"]


  # Sum point number per DA -------------------------------------------------

  # Intersect
  names(DA_table)[1] <- "DA_ID"
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(point_data))
    point_data <- future.apply::future_lapply(point_data, \(x) {
      out <- suppressWarnings(sf::st_intersection(x, DA_table)) |>
        sf::st_drop_geometry()
      pb()
      out
    }, future.seed = NULL)
  })

  # Count industry per DA
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(point_data))
    point_DA <-
      future.apply::future_lapply(point_data, \(x) {

        df <- as.data.frame(table(x$DA_ID))
        colnames(df) <- c("DA_ID", "count")

        pb()
        df <- df[df$count > 0, ]
        tibble::as_tibble(df)
      }, future.seed = NULL)
  })


  # One variable, one dataframe ---------------------------------------------

  point_DA <- mapply(\(name, df) {
    names(df)[2] <- name
    df
  }, names(point_DA), point_DA, SIMPLIFY = FALSE)


  # Merge to single dataframe -----------------------------------------------

  point_DA <- Reduce(\(a, b) merge(a, b, by = "DA_ID", all = TRUE), point_DA)

  # Populate all the DA table
  out_df <- merge(sf::st_drop_geometry(DA_table),
                  point_DA, all = TRUE)
  out_df[is.na(out_df)] <- 0


  # Return ------------------------------------------------------------------

  return(out_df)

}

#' Get travel time dataframes
#'
#' @param modes <`character vector`> Modes for which to download the travel
#' times
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database.
#'
#' @return Returns a named list. Each mode is an element, and inside each
#' DA has its own dataframe with time to get to each other DA.
#' @export
accessibility_get_travel_times <- function(modes = c("foot", "bike", "car",
                                                     "transit"),
                                           region_DA_IDs) {
  # Get travel times --------------------------------------------------------

  # Get travel times for walk, bike and car
  foot_bike_car <- modes[modes %in% c("foot", "bicycle", "car")]

  # Available traveltimes
  all_tables <- sapply(foot_bike_car, \(mode) {
    sapply(region_DA_IDs, \(ID) {
      paste("ttm", mode, ID, sep = "_")
    })
  })
  all_tables <- all_tables[all_tables %in% cc.data::db_list_tables()]

  # Retrieve from the server
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(foot_bike_car) * length(region_DA_IDs))
    ttm <- sapply(foot_bike_car, \(mode) {
      future.apply::future_sapply(region_DA_IDs, \(ID) {
        out <- cc.data::db_read_all_table(paste("ttm", mode, ID, sep = "_"))
        names(out)[2] <- "duration"
        # Convert seconds to minutes
        out[[2]] <- out[[2]]/60
        pb()

        return(out)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  })


#   # # Get travel times for bike
#   if ("transit" %in% modes) {
#   # # ttm_transit <- #TKTK
#   #
#   # # Available traveltimes
#   # all_tables <- sapply(TKTKTK, \(mode) {
#   #   sapply(TKKTTK, \(ID) {
#   #     paste("ttm", mode, ID, sep = "_")
#   #   })
#   # })
#   # all_tables <- all_tables[all_tables %in% cc.data::db_list_tables()]
#   #
#   # # Combine travel times
#   # ttm <- c(ttm, ttm_transit)
# #
#   # Add day time to transit modes
#   # transit_pwd, etc.
#   }

  return(ttm)
}


#' Accessibility of amenities by travel time, to one dataframe
#'
#' This function adds the data from the point layer to the travel time
#' matrices, and summarizes the amount of reachable amenities in certain
#' time intervals.
#'
#' @param point_per_DA A dataframe containing the number of data points per DA.
#' The first two columns should be `ID` and `geometry`, and the remaining
#' columns should be the number of points per amenity type.
#' @param traveltimes A list of dataframes of travel times by mode. The output
#' of \code{\link[cc.buildr]{accessibility_get_travel_times}}.
#' @param time_intervals A vector with the time intervals to use (in
#' minutes). Defaults to `c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)`.
#'
#' @return A dataframe containing the number of accessible amenities per
#' DA in different time intervals, for different modes of transport.
#'
#' @export
accessibility_add_intervals <- function(point_per_DA,
                                        traveltimes,
                                        time_intervals = which(1:60 %% 5 == 0)) {


  # Prepare data to feed to the iterations ----------------------------------

  modes <- names(traveltimes)
  region_DA_IDs <- unique(as.vector(sapply(traveltimes, names)))


  # Use travel times to put the point data in intervals ---------------------

  progressr::with_progress({
    pb <- progressr::progressor(
      steps = sum(sapply(traveltimes, length)) * length(time_intervals))

    ttm_data <- sapply(modes, \(mode) {
      Reduce(rbind, future.apply::future_lapply(region_DA_IDs, \(ID) {
        # For every mode and ID, iterate oveer all intervals to sum the
        # amount of reachable amenities
        out <- sapply(time_intervals, \(interval) {

          df <- merge(traveltimes[[mode]][[ID]], point_per_DA)
          df <- df[df$duration <= interval, ]
          colsumed <- colSums(df[3:ncol(df)])

          names(colsumed) <- paste("access", mode, interval, names(colsumed),
                                   sep = "_")

          pb()

          do.call(tibble::tibble, c(ID = as.character(ID),
                                    mapply(\(n, i) i,
                                           names(colsumed),
                                           colsumed,
                                           SIMPLIFY = FALSE, USE.NAMES = TRUE)))
        }, simplify = FALSE, USE.NAMES = TRUE)
        out <- Reduce(\(a, b) merge(a, b, by = "ID"), out)

        return(out)
      })
      )
    }, simplify = FALSE, USE.NAMES = TRUE)

    # Merge all modes
    ttm_data <- Reduce(\(a, b) merge(a, b, by = "ID"), ttm_data)
  })


  # Return ------------------------------------------------------------------

  return(ttm_data)

}
