#' Append new data to the `data` element of scales_variables_modules
#'
#' This function processes the provided scales data, constructs a new data table
#' based on the specified unique variables, and integrates additional time-related
#' attributes. It further tests for binning breaks and whether quintiles should
#' be used. The resulting data is merged with the existing scales_variables_modules data.
#'
#' @param scales_data <`named list`> A list containing the new data for each scale.
#' @param unique_var <`character`> A character vector of unique variable names to
#' construct data for, e.g. `c('alp', 'housing_tenant', ...)` without the
#' time.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `_\\d{4}` for
#' years.
#' @param schema <`named list`> Named list of schemas available for the live app.
#' It must be a regular expression grabbing said info from the variable name. e.g.
#' `list(time = '\\d{4}$')` for grabbing year.
#' @param breaks_var <`named list`> The specific variable from which to draw the
#' breaks out of all the values. Every element of the list is using a name
#' of `unique_var` and has as value a complete variable code with schemas. e.g.
#' for unique_var `alp`, breaks_var would be `list(alp = "alp_2021")`. Defaults to
#' NULL which will default in using the last ordered column (latest date).
#'
#' @return A merged list containing the processed data tables for each scale and
#' variable.
#' @details
#' If bin occurrences suggest uneven distributions (one bin with more than 50%
#' of observations), quintiles are applied. If breaks calculations
#' are not adequate, the function stops with an error.
#' @export
data_construct <- function(scales_data, unique_var, time_regex, data_folder = "data/",
                           schema = list(time = time_regex), breaks_var = NULL) {

  # If time_regex does not end with an dollar sign, flag it.
  if (!grepl("\\$$", time_regex)) {
    warning(paste0(
      "Location of `time` in the variables needs to be at the end of the vari",
      "able string, e.g. `alp_2001`. `time_regex` needs to end with a dollar ",
      "sign, indicating the location of time at the end of the string. It has",
      " been automatically added to the `time_regex` input."
    ))
    time_regex <- sprintf("%s$", time_regex)
  }
  # If time_regex does not start with an underscore, flag it
  if (!grepl("^_", time_regex)) {
    warning(paste0(
      "Location of `time` in the variables needs to be separated by an underscore",
      ", e.g. `alp_2001`. It has been automatically added to the `time_regex` input."
    ))
    time_regex <- sprintf("_%s", time_regex)
  }

  trimed <- mapply(\(scale_name, scale_df) {
    dat <- lapply(unique_var, \(v) {

      # Construct the data table
      df <- sf::st_drop_geometry(scale_df)

      all_cols <- names(df)
      for (regex in schema) {
        all_cols <- stringr::str_remove_all(all_cols, regex)
        # Remove double underscore (schema regex induced issue)
        all_cols <- gsub("__", "_", all_cols)
      }
      # Remove if the string ends with an underscore. schema regex induced issue.
      all_cols <- gsub("_$", "", all_cols)

      # Once all the schemas removed, which columns correspond to the variable v
      cols <- df[which(all_cols == v)]
      # If the variable is not available at all, return nothing
      if (ncol(cols) == 0) return()

      df <- cbind(df["ID"], cols) |> tibble::as_tibble()
      # Order the columns
      df <- df[c(1, order(names(df)[names(df) != "ID"]) + 1)]

      # Add the column which will be used to calculate breaks
      breaks_var <-
        if (is.null(breaks_var)) names(df[ncol(df)]) else breaks_var[[v]]
      attr(df, "breaks_var") <- breaks_var

      # Add the schema regexes
      attr(df, "schema") <- schema

      # Test and see the q5 breaks (always use the latest date)
      min_val <- min(df[breaks_var], na.rm = TRUE)
      max_val <- max(df[breaks_var], na.rm = TRUE)
      breaks <- curbcut::find_breaks_q5(min_val = min_val, max_val = max_val)

      # If there are no valid observations, return the NAs
      if (all(is.na(breaks))) return({
        attr(df, "quintiles") <- FALSE
        attr(df, "breaks") <- breaks
        return(df)
      })

      # If observations are valid but breaks are impossible to draw, stop
      if (length(unique(breaks)) != 6) {
        stop(sprintf(paste0("For variable `%s` in scale `%s`, the `curbcut::find_breaks_q5()` ",
                            "function do not output 5 unique bins (6 breaks), "),
                     names(df[breaks_var]), scale_name))
      }


      # Are bins WAY to unequals?
      bin_occ <- .bincode(df[[breaks_var]], breaks, include.lowest = TRUE)
      bin_occ_pct <- table(bin_occ) / length(bin_occ)

      # If there is one bin that has more than half ot observations, use quintiles
      use_quintiles <- sum(bin_occ_pct > 0.5) > 0
      attr(df, "quintiles") <- use_quintiles

      # If use quintiles, test it
      if (use_quintiles) {
        dist_no_na <- df[[breaks_var]][!is.na(df[[breaks_var]])]
        breaks <- curbcut::find_breaks_quintiles(dist = dist_no_na, q3_q5 = "q5")
        if (length(unique(breaks)) != 6) {
          stop(sprintf(paste0("For variable `%s` in scale `%s`, the `curbcut::find_breaks_quintiles()` ",
                              "function do not output 5 unique bins (6 breaks), "),
                       v, scale_name))
        }
      }

      # Add the breaks we just calculated
      attr(df, "breaks") <- breaks

      if (all(is.na(breaks)))
        stop(sprintf(paste0("All break values are NAs for variable `%s` in ",
                            "scale `%s`. Is it a scale with only one feature?"),
                     v, scale_name))

      return(df)
    })

    # Rename
    names(dat) <- unique_var

    # Remove NULLs
    dat <- dat[!sapply(dat, is.null)]

    return(dat)
  }, names(scales_data), scales_data, SIMPLIFY = FALSE)

  # List .sqlite databases
  sqlite_dbs <- grep("\\.sqlite$", list.files(data_folder), value = TRUE)
  sqlite_dbs <- gsub("\\.sqlite$", "", sqlite_dbs)

  # Save the data
  mapply(\(scale_name, data_list) {
    if (length(data_list) == 0) return()

    # Construct the folder path for the scale
    folder <- sprintf("%s%s/", data_folder, scale_name)

    # If the folder doesn't exist, create it
    if (!dir.exists(folder)) dir.create(folder)

    mapply(\(data_name, data) {
      if (is.null(data)) return()

      # Is it to be saved in the sqlite?
      if (scale_name %in% sqlite_dbs) {
        con <- DBI::dbConnect(RSQLite::SQLite(), sprintf("%s%s.sqlite", data_folder, scale_name))

        # Write the tibble to the SQLite database as a temporary table
        DBI::dbWriteTable(con, name = data_name, value = data, overwrite = TRUE)

        # Close the database connection
        DBI::dbDisconnect(con)
      } else {
        # Construct the file path for the table
        file <- sprintf("%s%s.qs", folder, data_name)

        # Save the table
        qs::qsave(data, file = file)
      }

    }, names(data_list), data_list)

  }, names(trimed), trimed)

  # Return nothing. The data is saved.
  return(NULL)

}
