### BREAK FUNCTIONS #######################################################

#' Append q3 column to the dataframe
#'
#' @param df <`data.frame`> Contains all columns in `vars`.
#' @param vars <`vector of character`> Contains all variable names from which
#' to add q3s. Must fit with a name in `df`.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `\\d{4}` for
#' years.
#'
#' @return Returns the same data.frame as df with q3 columns appended.
#' @export
add_q3 <- function(df, vars, time_regex = "\\d{4}") {
  time_regex_end <- paste0("_", time_regex, "$")

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
    gsub(time_regex_end, "", names(q3s)), "_q3",
    sapply(names(q3s), \(x) {
      loc <- regexpr(time_regex_end, x)
      if (loc < 0) {
        return("")
      }
      substring(x, loc)
    })
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
#'
#' @return A data.frame where each column in a var, and the rows are the q3
#' breaks.
#' @export
get_breaks_q3 <- function(df, vars) {
  tb <- sapply(vars, \(var) {
    if (!var %in% names(df)) {
      return()
    }
    dat <- sf::st_drop_geometry(df)
    dat <- dat[, grepl(var, gsub("_q3", "", names(dat)))]
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
#' a timeframe, placed at the end of the `vars` vector. e.g. `\\d{4}` for
#' years.
#'
#' @return Returns the same data.frame as df with q5 columns appended.
#' @export
add_q5 <- function(df, breaks, time_regex = "\\d{4}") {
  time_regex_end <- paste0("_", time_regex, "$")

  all_q5s <- mapply(\(var, values) {
    values[1] <- -Inf
    values[length(values)] <- Inf

    q5s <- if (length(df[[var]][!is.na(df[[var]])]) == 0) {
      rep(NA, nrow(df))
    } else {
      as.numeric(cut(df[[var]], values, include.lowest = TRUE))
    }

    q5s <- tibble::as_tibble(q5s)

    # Change names to get q3 between the variable name and the timeframe
    names(q5s) <- paste0(
      gsub(time_regex_end, "", var), "_q5",
      sapply(var, \(x) {
        loc <- regexpr(time_regex_end, x)
        if (loc < 0) {
          return("")
        }
        substring(x, loc)
      })
    )

    q5s
  }, names(breaks), breaks, SIMPLIFY = FALSE, USE.NAMES = TRUE)

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
  breaks <- unlist(lapply(-3:7, \(x) (10^x) * c(1, 1.5, 2, 2.5, 3, 4, 5, 6)))

  range <- max_val - min_val
  break_val <- range / 5
  break_val <- breaks[as.numeric(cut(break_val, breaks)) + 1]
  break_digits <- floor(log10(break_val))
  new_min <- floor(min_val / (10^break_digits)) * 10^break_digits

  return(c(new_min + 0:5 * break_val))
}

#' Get q5 break values
#'
#' @param df <`data.frame`> Contains all columns in `vars`.
#' @param vars <`vector of character`> Contains all variable names from q5s should
#' be calculated. Must fit with a name in `df`.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `\\d{4}` for
#' years.
#'
#' @return A data.frame where each column in a var, and the rows are the q3
#' @export
get_breaks_q5 <- function(df, vars, time_regex = "\\d{4}") {
  # Calculate q5 using ALL years
  time_regex_end <- paste0("_", time_regex, "$")
  unique_vars <- unique(gsub(time_regex_end, "", vars))

  q5s <- sapply(unique_vars, \(u_var) {
    # Extract the variable in a numeric vector
    df <- sf::st_drop_geometry(df)
    df_no_q3 <- df[!grepl("_q3", names(df))]
    as_vec <- unlist(df_no_q3[grepl(
      paste0(u_var, c("_[0-9]+$", "$"),
        collapse = "|"
      ),
      names(df_no_q3)
    )], use.names = FALSE)
    as_vec <- stats::na.omit(as_vec)

    # Calculate minimum and maximum
    cat_min <- min(as_vec)
    cat_max <- max(as_vec)

    # Calculate mean and standard deviation with outliers filtered out
    prep_mean <- as_vec[!as_vec <= stats::quantile(as_vec, .01)]
    prep_mean <- prep_mean[!prep_mean >= stats::quantile(prep_mean, .99)]
    var_mean <- mean(prep_mean)
    standard_d <- stats::sd(prep_mean)

    # Create pretty breaks
    min_val <- max(var_mean - (4 * standard_d), cat_min)
    max_val <- min(var_mean + (4 * standard_d), cat_max)

    find_breaks_q5(min_val, max_val)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Make a list with the according q5
  tb <- sapply(vars, \(var) {
    if (!var %in% names(df)) {
      return()
    }
    unlist(q5s[which(gsub(time_regex_end, "", var) == names(q5s))],
      use.names = FALSE
    )
  }, simplify = FALSE, USE.NAMES = TRUE)

  tb <- tb[!sapply(tb, is.null)]
  tibble::as_tibble(tb)
}

#' Calculate all breaks when all_scales is a list of regions and scales
#'
#' @param all_scales <`named_list`> A named list of scales. The first level is
#' the geo, and the second is the scales. They must contain all columns of `vars`
#' @param vars <`vector of character`> Contains all variable names from which
#' to add q3s. Must fit with a name in `df`.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `\\d{4}` for
#' years. If the variable does not have a timeframe, enter an empty string (`""`).
#'
#' @return Returns a list of length 4. The first is the same data.frame as df
#' with q3 and q5 columns appended. The second is the q3 breaks table, and the third
#' is the q5 breaks table. The fourth is vectors of characters of all dates at
#' which variables are available for.
#' @export
calculate_breaks <- function(all_scales, vars, time_regex = "\\d{4}") {

  if (time_regex != "") {
    if (sum(sapply(vars, \(var) grepl(time_regex, var))) == 0)
      stop(paste0("The time regular expression `", time_regex, "` isn't found in",
                  " the variables name. Add the time, or set the `time_regex` ",
                  "argument to an empty string."))
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
      get_breaks_q3(scale_df, vars)
    }
  )
  tables_q5 <- map_over_scales(
    all_scales = out_tables,
    fun = \(scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(tibble::tibble())
      }
      get_breaks_q5(scale_df, vars, time_regex)
    }
  )

  # Append q5s
  out_tables <- map_over_scales(
    all_scales = out_tables,
    fun = \(geo = geo, scale_name = scale_name, scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(scale_df)
      }
      add_q5(scale_df, tables_q5[[geo]][[scale_name]], time_regex = time_regex)
    }
  )

  # Arrange the breaks tables
  # Unique variables
  time_regex_end_ <- paste0("_", time_regex, "$")
  unique_vars <- unique(gsub(time_regex_end_, "", vars))
  time_regex_end <- paste0(time_regex, "$")

  # q3
  q3_breaks_table <-
    sapply(unique_vars, \(var) {
      map_over_scales(
        all_scales = tables_q3,
        fun = \(geo = geo, scale_name = scale_name, scale_df = scale_df, ...) {
          var_all_years <- names(scale_df)[grepl(var, names(scale_df))]
          out <- lapply(var_all_years, \(v) {
            date <- {
              loc <- regexpr(time_regex_end, v)
              if (loc < 0) {
                return(NA)
              }
              substring(v, loc)
            }
            tibble::tibble(
              geo = geo,
              scale = scale_name,
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
          var_all_years <- names(scale_df)[grepl(var, names(scale_df))]
          out <- lapply(var_all_years, \(v) {
            tibble::tibble(
              geo = geo,
              scale = scale_name,
              rank = seq_len(nrow(scale_df)) - 1,
              var = scale_df[[v]]
            )
          })
          unique(Reduce(rbind, out))
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
