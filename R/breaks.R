### BREAK FUNCTIONS #######################################################

#' Append q3 column to the dataframe
#'
#' @param df <`data.frame`> Contains all columns in `vars`.
#' @param vars <`vector of character`> Contains all variable names from which
#' to add q3s. Must fit with a name in `df`.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `_\\d{4}$` for
#' years.
#'
#' @return Returns the same data.frame as df with q3 columns appended.
#' @export
add_q3 <- function(df, vars, time_regex = "_\\d{4}$") {
  # Get all q3s arranged in a dataframe
  q3s <-
    sapply(vars, \(var) {
      if (!var %in% names(df)) {
        return()
      }
      rough_rank(df[[var]], 3)
    }, simplify = FALSE, USE.NAMES = TRUE)
  q3s <- q3s[!sapply(q3s, is.null)]
  q3s <- tibble::as_tibble(q3s)

  # Change names to get q3 between the variable name and the timeframe
  names(q3s) <- paste0(
    gsub(time_regex, "", names(q3s)), "_q3",
    if (time_regex != "") {
      sapply(names(q3s), \(x) {
        loc <- regexpr(time_regex, x)
        if (loc < 0) {
          return("")
        }
        substring(x, loc)
      })
    }
  )

  out <- tibble::as_tibble(cbind(df, q3s))
  if ("sf" %in% class(df)) out <- sf::st_as_sf(out)
  out
}

#' Get q3 break values
#'
#' @param df <`data.frame`> Contains all columns in `vars` as well as their
#' q3s. The result of \code{\link[cc.buildr]{add_q3}}.
#' @param vars <`vector of character`> Contains all variable names from which
#' to add q3s. Must fit with a name in `df`.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `_\\d{4}$` for
#' years.
#'
#' @return A data.frame where each column in a var, and the rows are the q3
#' breaks.
#' @export
get_breaks_q3 <- function(df, vars, time_regex = "_\\d{4}$") {
  tb <- sapply(vars, \(var) {
    if (!var %in% names(df)) {
      return()
    }
    dat <- sf::st_drop_geometry(df)

    var_q3_regex <- paste0(gsub(time_regex, "", var), "_q3")
    time <- if (time_regex == "") "" else stringr::str_extract(var, time_regex)
    var_q3 <- paste0(var_q3_regex, time)
    dat <- dat[, c(var, var_q3)]
    names(dat) <- c("v", "q3")

    if (sum(is.na(dat$v)) == nrow(dat)) {
      return(rep(NA_real_, 4))
    }

    out <- c(
      min(dat$v, na.rm = TRUE),
      min(dat$v[dat$q3 == 2], na.rm = TRUE),
      min(dat$v[dat$q3 == 3], na.rm = TRUE),
      max(dat$v, na.rm = TRUE)
    ) |> suppressWarnings()

    ifelse(is.infinite(out), NA_real_, out)
  }, simplify = FALSE, USE.NAMES = TRUE)

  tb <- tb[!sapply(tb, is.null)]
  tibble::as_tibble(tb)
}


# q5 breaks ---------------------------------------------------------------

#' Append q5 column to a dataframe
#'
#' @param df <`data.frame`> Contains all columns in `vars`.
#' @param breaks <`data.frame`> A data.frame containing all the q5 breaks. The
#' output of \code{\link[cc.buildr]{get_breaks_q5}}.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `_\\d{4}$` for
#' years.
#' @param breaks_base <`names list`> If we want variables to use the breaks of
#' another variable, we can supply a named list. The name is the variable that will
#' use the breaks of the value variable. ex. in access, we want all variables of
#' same theme to use the same breaks, no matter the transportation time. This
#' would be how we would do the list, if we want to use the 30 minutes break as
#' the basis for all the other transport time.
#' `list(access_transit_pwd_25_food_misc_2023 = "access_transit_pwd_30_food_misc_2023", access_transit_pwd_20_food_misc_2023 = "access_transit_pwd_30_food_misc_2023", ...)`
#'
#' @return Returns the same data.frame as df with q5 columns appended.
#' @export
add_q5 <- function(df, breaks, time_regex = "_\\d{4}$", breaks_base) {
  all_q5s <- lapply(names(breaks), \(var) {
    var_regex <- paste0("^", var, time_regex)
    df_var <- names(df)[grepl(var_regex, names(df))]
    df_var <- df_var[!grepl("_q3$|_q5$", df_var)]

    q5s <- lapply(df_var, \(v) {
      vals <- df[[v]]
      if (all(is.na(vals))) {
        return({
          out <- tibble::tibble(var = vals)
          time <- stringr::str_extract(v, time_regex)
          names(out) <- paste0(var, "_q5", time)
          out
        })
      }

      # Which breaks to use?
      breaks_subset_var <- breaks_base[[var]]

      # Grab the correct breaks
      brks <- breaks[[breaks_subset_var]]

      # Attach the breaks. Make sure the lower and upper limit are included
      # in the q5s even if they are lower/higher than the break by temporarily
      # editing the lower and higher breaks
      min_val <- min(vals, na.rm = TRUE)
      if (is.na(brks[[1]])) {
        brks[[1]] <- 0
      } else {
        if (min_val < brks[[1]]) brks[[1]] <- min_val
      }
      max_val <- max(vals, na.rm = TRUE)
      if (is.na(brks[[length(brks)]])) {
        brks[[length(brks)]] <- 0
      } else {
        if (max_val > brks[[length(brks)]]) brks[[length(brks)]] <- max_val
      }
      brks[is.na(brks)] <- 0
      q5s <- if (all(brks == 0)) {
        rep(0, length(vals))
      } else {
        as.numeric(cut(vals, brks, include.lowest = TRUE))
      }

      out <- tibble::tibble(var = q5s)
      time <- if (time_regex == "") "" else stringr::str_extract(v, time_regex)
      names(out) <- paste0(var, "_q5", time)
      return(out)
    })

    Reduce(cbind, q5s)
  })

  to_bind <- if (length(all_q5s) > 0) do.call(cbind, all_q5s) else all_q5s[[1]]

  out <- tibble::as_tibble(cbind(df, to_bind))
  if ("sf" %in% class(df)) out <- sf::st_as_sf(out)
  out
}

#' Find pretty q5 breaks
#'
#' @param min_val <`numeric`>
#' @param max_val <`numeric`>
#'
#' @return Returns a numeric vector with pretty q5 break values.
#' @export
find_breaks_q5 <- function(min_val, max_val) {
  breaks <- unlist(lapply(-4:7, \(x) (10^x) * c(0.75, 1, 1.5, 2, 2.5, 3, 4, 5, 6)))
  breaks <- unique(breaks)

  range <- max_val - min_val
  break_val <- range / 5
  break_val <- breaks[as.numeric(cut(break_val, breaks)) + 1]
  break_digits <- floor(log10(break_val))
  new_min <- floor(min_val / (10^break_digits)) * 10^break_digits

  return(c(new_min + 0:5 * break_val))
}

#' Find pretty q5 breaks
#'
#' @param dist <`numeric`> Distribution (numerics) with no NAs.
#' @param min_val <`numeric`>
#' @param max_val <`numeric`>
#'
#' @return Returns a numeric vector with pretty q5 break values. It ensures that
#' each break point is different by adjusting the rounding base dynamically
#' based on the difference between each quantile and its predecessor. The while
#' loop ensures that if a new break point is the same as the last one after
#' rounding, the base for rounding will be decreased, increasing the precision
#' until a unique break point is found. Note that this might lead to break
#' points with a high level of precision, especially in data sets with many
#' similar values.
#' @export
find_breaks_quintiles_q5 <- function(min_val, max_val, dist) {

  # Take out min and max values (outliers)
  no_outliers <- dist[dist > min_val & dist < max_val]
  if (length(unique(no_outliers)) < 10) {
    no_outliers <- dist
  }
  no_outliers <- unique(no_outliers)

  # Calculate quintiles
  q <- stats::quantile(no_outliers, probs = seq(0, 1, by = 0.2))

  # Create empty breaks vector
  breaks <- numeric(length(q))

  # Initialize first break and previous_q
  previous_q <- 0

  # Loop through all the quantiles
  for (i in 1:length(q)) {

    # Check if difference between current quantile and previous one is zero
    if (q[i] - previous_q == 0) {
      round_base <- 1
    } else {
      # Determine the rounding base for each quantile difference
      round_base <- 10^floor(log10(abs(q[i] - previous_q)))
    }

    # Create a "pretty" break ensuring it's different from the previous one
    new_break <- round(q[i] / round_base) * round_base

    # If it's the first break and it's equal to zero, do nothing.
    # If the new break is the same as the previous one, decrease the rounding base
    # until they are different
    if (!(i == 1 && new_break == 0)) {
      while (new_break %in% breaks) {
        round_base <- round_base / 10
        new_break <- round(q[i] / round_base) * round_base
      }
    }

    # Assign the new break to the breaks vector
    breaks[i] <- new_break
    previous_q <- new_break
  }

  # Check if the first break is much closer to 0 than the second break
  if (breaks[2] / breaks[1] > 10) {
    breaks[1] <- 0
  }

  # If the minimum value was already 0
  if (min_val == 0) {
    breaks[1] <- 0
  }

  # Make sure the order is lower to higher
  breaks <- breaks[order(breaks)]

  return(unname(breaks))

}

#' Get q5 break values
#'
#' @param df <`data.frame`> Contains all columns in `vars`.
#' @param vars <`vector of character`> Contains all variable names from q5s should
#' be calculated. Must fit with a name in `df`.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `_\\d{4}$` for
#' years.
#' @param use_quintiles <`logical`> Should the breaks be pretty and constant made
#' by splitting the data in 5 using the minimum and maximal value, or should we use
#' a quintile approach. The latter is better suited for non-normal distributions.
#'
#' @return A data.frame where each column is a var, and the rows are the q5
#' @export
get_breaks_q5 <- function(df, vars, time_regex = "_\\d{4}$", use_quintiles = FALSE) {
  # Calculate q5 only using MOST RECENT year
  unique_vars <- unique(gsub(time_regex, "", vars))
  unique_vars_regex <- paste0("^", unique_vars, time_regex)
  unique_vars <- sapply(unique_vars_regex, \(x) {
    all_years <- vars[grepl(x, vars)]
    all_years_ordered <- all_years[order(all_years)]
    all_years_ordered[length(all_years_ordered)]
  }, USE.NAMES = FALSE)

  q5s <- sapply(unique_vars, \(u_var) {
    # Extract the variable in a numeric vector
    as_vec <- df[[u_var]]
    as_vec <- stats::na.omit(as_vec)

    # Calculate minimum and maximum
    cat_min <- min(as_vec)
    cat_max <- max(as_vec)

    # Calculate mean and standard deviation with outliers filtered out
    prep_mean <- as_vec[!as_vec < stats::quantile(as_vec, .01)]
    prep_mean <- prep_mean[!prep_mean > stats::quantile(prep_mean, .99)]
    var_mean <- mean(prep_mean)
    standard_d <- stats::sd(prep_mean)

    # Create pretty breaks
    min_val <- max(var_mean - (4 * standard_d), cat_min)
    max_val <- min(var_mean + (4 * standard_d), cat_max)

    # If use quintiles, use a different function
    if (use_quintiles) {
      breaks <- find_breaks_quintiles_q5(min_val, max_val, dist = as_vec)
    } else {
      breaks <- find_breaks_q5(min_val, max_val)

      if (length(as_vec) > 3 & length(as_vec) < 500) {
        if (stats::shapiro.test(as_vec)$statistic < 0.5) {
          warning(sprintf(paste0("The distribution for '%s' does not seem to be a normal ",
                                 "distribution. Consider using the `use_quintiles` ",
                                 "argument."), u_var))
        }
      }

    }

    # IF NORMAL DISTRIBUTION
    out <- tibble::tibble(var = breaks)
    # IF NOT NORMAL DISTRIBUTION (SHOULD BE AN ARGUMENT TO WITH_BREAKS FUNCTION)
    # QUINTILES!
    names(out) <- gsub(time_regex, "", u_var)
    return(out)
  }, simplify = FALSE, USE.NAMES = TRUE)

  tibble::as_tibble(Reduce(cbind, q5s))
}

#' Calculate all breaks when all_scales is a list of regions and scales
#'
#' @param all_scales <`named_list`> A named list of scales. The first level is
#' the geo, and the second is the scales. They must contain all columns of `vars`
#' @param vars <`vector of character`> Contains all variable names from which
#' to add q3s. Must fit with a name in `df`.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `_\\d{4}$` for
#' years. If the variable does not have a timeframe, enter an empty string (`""`).
#' @param types <`list`> A named list of variable types (e.g., "pct", "avg", "count", "ind").
#' The names of the list should match the variable names in \code{vars} (without the dates).
#' @param rank_name <`character vector`> If the type is `ind`, how should every ranks
#' from 1 to 5 (q5) be named. Defaults to c("Low", "Below average", "Average",
#' "Above average", "High")
#' @param rank_name_short <`character vector`> Same as rank_name but shorter
#' to not take too much space in the legend labels. Defaults to c("Low", "B. average",
#' "Average", "A. average", "High")
#' @param use_quintiles <`logical`> Should the breaks be pretty and constant made
#' by splitting the data in 5 using the minimum and maximal value, or should we use
#' a quintile approach. The latter is better suited for non-normal distributions.
#' @param breaks_base <`names list`> If we want variables to use the breaks of
#' another variable, we can supply a named list. The name is the variable that will
#' use the breaks of the value variable. ex. in access, we want all variables of
#' same theme to use the same breaks, no matter the transportation time. This
#' would be how we would do the list, if we want to use the 30 minutes break as
#' the basis for all the other transport time.
#' `list(access_transit_pwd_25_food_misc_2023 = "access_transit_pwd_30_food_misc_2023", access_transit_pwd_20_food_misc_2023 = "access_transit_pwd_30_food_misc_2023", ...)`
#'
#' @return Returns a list of length 4. The first is the same data.frame as df
#' with q3 and q5 columns appended. The second is the q3 breaks table, and the third
#' is the q5 breaks table. The fourth is vectors of characters of all dates at
#' which variables are available for.
#' @export
calculate_breaks <- function(all_scales, vars, time_regex = "_\\d{4}$",
                             types = NULL,
                             rank_name = c(
                               "Very low", "Low", "Moderate",
                               "High", "Very high"
                             ),
                             rank_name_short = c(
                               "Very low", "Low", "Moderate",
                               "High", "Very high"
                             ),
                             breaks_base = NULL,
                             use_quintiles = FALSE) {
  if (time_regex != "") {
    if (sum(sapply(vars, \(var) grepl(time_regex, var))) == 0) {
      stop(paste0(
        "The time_regex argument `", time_regex, "` isn't found in",
        " the variables name. Add the time, or set the `time_regex` ",
        "argument to an empty string: `''`."
      ))
    }
  }

  if (is.null(breaks_base)) {
    vars_unique <- unique(gsub(time_regex, "", vars))
    breaks_base <- sapply(vars_unique, paste, simplify = FALSE, USE.NAMES = TRUE)
  }

  # Append q3s
  out_tables <- map_over_scales(
    all_scales = all_scales,
    fun = \(scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(scale_df)
      }
      add_q3(scale_df, vars, time_regex = time_regex)
    }
  )

  # Get breaks
  tables_q3 <- map_over_scales(
    all_scales = out_tables,
    fun = \(scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(tibble::tibble())
      }
      get_breaks_q3(scale_df, vars, time_regex = time_regex)
    }
  )
  tables_q5 <- map_over_scales(
    all_scales = out_tables,
    fun = \(scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(tibble::tibble())
      }
      get_breaks_q5(scale_df, vars, time_regex, use_quintiles = use_quintiles)
    }
  )

  # Append q5s
  out_tables <- map_over_scales(
    all_scales = out_tables,
    fun = \(geo = geo, scale_name = scale_name, scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(scale_df)
      }
      add_q5(
        df = scale_df,
        breaks = tables_q5[[geo]][[scale_name]],
        time_regex = time_regex,
        breaks_base = breaks_base
      )
    }
  )

  # Arrange the breaks tables
  # Unique variables
  unique_vars <- unique(gsub(time_regex, "", vars))

  # q3
  q3_breaks_table <-
    sapply(unique_vars, \(var) {
      map_over_scales(
        all_scales = tables_q3,
        fun = \(geo = geo, scale_name = scale_name, scale_df = scale_df, ...) {
          var_regex <- paste0("^", var, time_regex)
          var_all_years <- names(scale_df)[grepl(var_regex, names(scale_df))]
          out <- lapply(var_all_years, \(v) {
            date <- if (time_regex == "") "" else stringr::str_extract(v, time_regex)
            date <- gsub("^_", "", date)
            tibble::tibble(
              df = paste(geo, scale_name, sep = "_"),
              date = if (date == "") NA else date,
              rank = seq_len(nrow(scale_df)) - 1,
              var = scale_df[[v]]
            )
          })
          Reduce(rbind, out)
        }
      )
    }, simplify = FALSE, USE.NAMES = TRUE)
  q3_breaks_table <-
    sapply(q3_breaks_table, \(x) {
      out <-
        sapply(x, \(y) {
          do.call(rbind, y)
        }, simplify = FALSE, USE.NAMES = TRUE)
      do.call(rbind, out)
    }, simplify = FALSE, USE.NAMES = TRUE)
  row.names(q3_breaks_table) <- NULL

  # q5
  q5_breaks_table <-
    sapply(unique_vars, \(var) {
      map_over_scales(
        all_scales = tables_q5,
        fun = \(geo = geo, scale_name = scale_name, scale_df = scale_df, ...) {
          if (nrow(scale_df) == 0) {
            return(tibble::tibble())
          }
          # Which breaks to use?
          breaks_subset_var <- breaks_base[[var]]

          out <- tibble::tibble(
            df = paste(geo, scale_name, sep = "_"),
            rank = seq_len(nrow(scale_df)) - 1,
            var = scale_df[[breaks_subset_var]]
          )

          # If type is `ind`, add rank_name and rank_name_short
          if (!is.null(types) && "ind" %in% types[[var]]) {
            out$rank_name <- c(NA, rank_name)
            out$rank_name_short <- c(NA, rank_name_short)
          }

          return(out)
        }
      )
    }, simplify = FALSE, USE.NAMES = TRUE)
  q5_breaks_table <-
    sapply(q5_breaks_table, \(x) {
      out <-
        sapply(x, \(y) {
          do.call(rbind, y)
        }, simplify = FALSE, USE.NAMES = TRUE)
      do.call(rbind, out)
    }, simplify = FALSE, USE.NAMES = TRUE)
  row.names(q5_breaks_table) <- NULL

  # Available dates
  avail_dates <-
    sapply(q3_breaks_table, \(var_q3_breaks_table) {
      unique(var_q3_breaks_table$date)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Return
  return(list(
    scales = out_tables,
    q3_breaks_table = q3_breaks_table,
    q5_breaks_table = q5_breaks_table,
    avail_dates = avail_dates
  ))
}
