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
    point_DA,
    all = TRUE
  )
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
accessibility_get_travel_times <- function(modes = c(
                                             "foot", "bicycle", "car",
                                             "transit"
                                           ),
                                           region_DA_IDs) {
  # Get travel times --------------------------------------------------------

  if ("transit" %in% modes) {
    gtfs_time <- c("opwe", "pwe", "nwd", "nwe", "opwd", "pwd")
    modes <- modes[modes != "transit"]
    modes <- c(modes, paste("transit", gtfs_time, sep = "_"))
  }

  # Available traveltimes
  all_tables <- sapply(modes, \(mode) {
    sapply(region_DA_IDs, \(ID) {
      paste("ttm", mode, ID, sep = "_")
    })
  })
  all_tables <- all_tables[all_tables %in% cc.data::db_list_tables()]

  # Retrieve from the server
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(all_tables))
    ttm <- future.apply::future_sapply(modes, \(mode) {
      all <- all_tables[grepl(mode, all_tables)]
      sapply(all, \(tb) {
        out <- cc.data::db_read_all_table(tb)
        names(out)[2] <- "duration"
        # Convert seconds to minutes
        out[[2]] <- out[[2]] / 60
        pb()

        return(out)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  })

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
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database.
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
                                        region_DA_IDs = region_DA_IDs,
                                        traveltimes,
                                        time_intervals = which(1:60 %% 5 == 0)) {
  # Prepare data to feed to the iterations ----------------------------------

  modes <- names(traveltimes)


  # Use travel times to put the point data in intervals ---------------------
  # ARE THERE SOME DAS THAT ARE CUT HERE? I'M PASSING FROM A 6.5K DAS TO 4.5'
  progressr::with_progress({
    pb <- progressr::progressor(steps = sum(sapply(traveltimes, length)) + length(traveltimes))

    ttm_data <- future.apply::future_lapply(modes, \(mode) {
      # Grab the right traveltimes and iterate over all DAs
      ttm <- traveltimes[[mode]]
      along_ttm <- lapply(seq_along(ttm), \(x) {
        # Grab the DA ID from the ttm name
        DA <- gsub(".*_(?=\\d)", "", names(ttm)[[x]], perl = TRUE)
        tt <- ttm[[x]]
        # Add self
        tt <- rbind(tibble::tibble(DA_ID = DA, duration = 0), tt)

        # Calculate the amount of amenities accessible in every time intervals
        intervaled <- sapply(as.character(time_intervals), \(ti) {
          z <- tt[tt$duration <= as.numeric(ti), ]

          accessible <-
            point_per_DA[point_per_DA$DA_ID %in% z$DA_ID, 2:ncol(point_per_DA)]

          colsumed <- colSums(accessible)

          out <- tibble::tibble(DA_ID = DA)

          for (i in names(colsumed)) {
            out[[sprintf("access_%s_%s_%s", mode, ti, i)]] <-
              colsumed[[i]]
          }

          return(out[2:ncol(out)])
        }, simplify = FALSE, USE.NAMES = FALSE)
        pb()
        Reduce(cbind, list(tibble::tibble(DA_ID = DA), intervaled))
      })
      out <- Reduce(rbind, along_ttm)
      pb()
      out
    })
  })

  ttm_data <- Reduce(
    \(x, y) merge(x, y, all.x = TRUE, all.y = TRUE, by = "DA_ID"),
    ttm_data
  )
  # If NA values, then no amenities are available (0)
  ttm_data[is.na(ttm_data)] <- 0

  # Return ------------------------------------------------------------------

  return(ttm_data)
}
