#' Append an empty variables table to the scales list
#'
#' `append_empty_variables_table()` adds a first level to the scales list, and
#' places an empty variables table to the scales list.
#'
#' @param scales_consolidated <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions. The output of
#' \code{\link[cc.buildr]{consolidate_scales}}.
#'
#' @return A list with the second index being an empty variables table.
#' @export
append_empty_variables_table <- function(scales_consolidated) {
  variables <-
    tibble::tibble(
      var_code = character(),
      type = list(),
      var_title = character(),
      var_short = character(),
      explanation = character(),
      explanation_nodet = character(),
      exp_q5 = character(),
      parent_vec = character(),
      theme = character(),
      private = logical(),
      pe_include = logical(),
      group_name = character(),
      group_diff = list(),
      dates = list(),
      avail_df = list(),
      breaks_q3 = list(),
      breaks_q5 = list(),
      region_values = list(),
      source = character(),
      interpolated = list(),
      rankings_chr = list(),
      var_measurement = list()
    )

  list(scales = scales_consolidated, variables = variables)
}

#' Add a new variable to the variables table
#'
#' @param variables <`data.frame`> The \code{variables} data.frame to which add a
#' new row.
#' @param var_code <`character`> The code used to refer to the variables, e.g.
#' \code{canale}
#' @param type <`character`> The variable type. One of \code{"ind"}, \code{"pct"},
#' \code{"avg"}, \code{"median"}, \code{"per1k"}, \code{"sqkm"} or \code{"count"}.
#' @param var_title <`character`> The variable title
#' @param var_short <`character`> A short variable title used in graphs or where
#' space is limited. Preferably ~ <12 characters.
#' @param explanation <`character`> Variable explanation. Starts with a determinant.
#'  e.g. the percentage of private dwellings occupied by tenants
#' @param explanation_nodet <`character`> Usually the same as `explanation`
#' without the determinant it starts with. It defaults to `explanation` with the `^the `
#' it starts with removed. Used in the compare panel following `a higher x`.
#' e.g. a higher 'percentage of private dwellings occupied by tenants'.
#' @param exp_q5 <`character`> String used for the explore panel explaining the
#' variable. Depends on the `type`. The rules are:
#' \itemize{
#'  \item{"pct"}{starts with a verb and follows the absolute count of the parent
#'  string. the string will read e.g. '50 households (3%) are tenants' where
#'  `are tenants` is the definition value.}
#'  \item{"dollar"}{starts with a subject and ends with a verb. It's assumed it
#'  will be followed by a dollar number. e.g. 'the average rent is 800$' where
#'  `the average rent is` is the definition value.}
#'  \item{"ind"}{starts with a verb and uses a place holder written `_X_` which
#'  would translates to, e.g. 'medium to high'. example: '50 households are living
#'  in areas with low potential for active living' where the definition would
#'  be: `are living in areas with _X_ potential for active living`}
#'  \item{"avg"}{starts with a subject and uses a place holder written `_X_` which
#'  would translates to a number. example: 'the average resident has access to 30
#'  grocery stores within 15 minutes by walk' where the definition would be:
#'  `the average resident has access to _X_ grocery stores within 15 minutes by walk`}
#'  \item{"sqkm"}{starts with a determinant and uses a placeholder written `_X_` which
#'  translates to a number. example: 'the density of green alleys is 2.28 square
#'  metres per square kilometres' where the definition would be:
#'  `the density of green alleys is _X_ square metres per square kilometres`
#'  }
#'  \item{"per1k"}{starts with a determinant and uses a placeholder written `_X_` which
#'  translates to a number. example: 'the density of green alleys is 28.7 square
#'  metres per 1000 residents' where the definition would be:
#'  `the density of green alleys is _X_ square metres per 1000 residents`
#'  }
#' }
#' @param parent_vec <`character`> Parent vector of the variable. Used for
#' the explore panel. Must be another entry in the variable table. E.g. for
#' Tenant households (%), the parent variable would be the number of
#' private households (denominator of the percentage) : `private_households`.
#' @param theme <`character`> The theme to which the variable belongs, e.g. "Housing",
#' "Urban life", ...
#' @param private <`logical`> If we have permissions to make the variable available
#' for public download.
#' @param pe_include <`logical`> Should this variable be included in the place
#' explorer? Defaults to `TRUE`.
#' @param group_name <`character`> The name of the larger group to which the
#' variable belongs. e.g. for the variable accessibility to public schools by bike,
#' the group_name would be \code{"Accessibility to schools"}
#' @param group_diff <`named list`> A named list is used to represent a variable
#' that is part of a larger group. For example, when considering accessibility
#' to public schools by bike, the larger group is Accessibility to schools, and
#' the mode of transport (bike) differentiates the subgroups. The list can be
#' constructed as follows: list("Mode of transport" = "By bike", "Public/Private" =
#' "Public") This list may contain multiple named vectors, each representing a
#' different subgroup. By default, these groups will be displayed in a dropdown
#' menu. If you prefer a slider, you should include the 'slider' class to the
#' corresponding list element value. The element should be a factor with properly
#' ordered levels. For example, let's say you are working with the shelter cost
#' to income ratio. Create a named list with a 'Shelter cost' key and a factor
#' value of '>30%', with the following levels (which is the order you want
#' displayed on the slider): c('>0%', '>30%', '>50%', '>80%'). Add the 'slider'
#' class to the value, so it is displayed as a slider. Example:
#' list("Gender" = "Female",
#'      "Shelter cost to income ratio)" = structure(
#'        factor(">0%", levels = c(">0%", ">30%", ">50%", ">80%")),
#'        class = "slider"))
#' @param dates <`character vector`> A vector of dates for which the data is available.
#' @param avail_df <`list`> All the combinations of region and scales
#' at which the data is available, e.g. `c("CMA_CSD", "CMA_CT", ...)`
#' @param breaks_q3 <`data.frame`> A data.frame with with information regarding
#' scales, date, rank, breaks. The last outputs of
#' \code{\link[cc.buildr]{calculate_breaks}}
#' @param breaks_q5 <`data.frame`> A data.frame with with information regarding
#' scales, date, rank, breaks. The last outputs of
#' \code{\link[cc.buildr]{calculate_breaks}}
#' @param region_values <`data.frame`> A data.frame with information regarding
#' the values of the overall region. Must include the region, and potentially
#' the year, val and count columns, depending on the variable type. Can be
#' created using \code{\link{variables_get_region_vals}}.
#' @param source <`character`> The source where the data comes from, e.g.
#' "McGill Geo-Social Determinants of Health Research Group"
#' @param interpolated <`data.frame`> A data.frame indicating from which scale
#' the region/scale comination has been interpolated. The non-interpolated data
#' is populated with \code{"FALSE"}. The interpolation scale must be one of the
#' scale code in the `scales_dictionary`.
#' @param rankings_chr <`character vector`> Vector of character that will be used
#' in the explore text to inform how a location ranks within the region.
#' The attach character will be its `q5` break. `ranking_chr` must be in order of
#' lower to higher. `The variable score is 90% which is unusually low for Montreal`.
#' Defaults to `c("exceptionally low", "unusually low", "just about average", "unusually high", "exceptionally high")`
#' @param var_measurement <`character list`> Data.frame where every row is an available
#' df (`aval_df`) and the second column is the measurement type. These will impact
#' both the explore panel text and the explore panel graph. Options are:
#' \itemize{
#'  \item{"scalar"}{The default. If `var_measurement` is not supplied, the row will
#'  be automatically filled with this measurement. Represents numeric data with both
#'  meaningful order and consistent distance between values. Example: age, temperature,
#'  height, weight.}
#'  \item{"ordinal"}{Represents rank or order within a dataset. It has meaningful
#'  order or sequence, but the distance between values is not consistent. It is
#'  the one used for the climate risk page at the grid scale where
#'  1 = Insignificant vulnerability, 2 = Minore, 3 = Moderate, ...
#'  Other example: survey responses with options like 'Strongly disagree', 'Disagree', ...}
#'  \item{"nominal"}{Represents categories without inherent order. No meaningful
#'  order or ranking is possible. Qualitative variables. Example: animal types,
#'  such as 'Mammal', 'Bird', 'Reptile', 'Fish'.}
#' }
#'
#' @return The same `variables` data.frame fed, with the added row.
#' @export
add_variable <- function(variables, var_code, type, var_title,
                         var_short = as.character(var_title), explanation,
                         explanation_nodet = gsub("^the ", "", explanation),
                         exp_q5, parent_vec, group_name = NA,
                         group_diff = list(), theme, private, pe_include = FALSE,
                         dates, avail_df, breaks_q3, breaks_q5,
                         region_values = NULL, source, interpolated,
                         rankings_chr = c("exceptionally low", "unusually low",
                                          "just about average", "unusually high",
                                          "exceptionally high"),
                         var_measurement = data.frame(
                           df = avail_df,
                           measurement = rep("scalar", length(avail_df)))) {
  if (var_code %in% variables$var_code) {
    stop(paste0("`", var_code, "` is a duplicate."))
  }

  if (var_code %in% variables$var_code)
    stop(paste0("`", var_code, "` is already a `var_code` present in the variables table."))

  # Necessary to not have issues with the dropdowns (unique title can't hold 2 variables)
  if (var_title %in% variables$var_title)
    stop(paste0("`", var_title, "` is already a `var_title` present in the variables table."))

  # If NULL is supplied, revert to the default
  if (all(is.null(rankings_chr))) {
    rankings_chr <- c("exceptionally low", "unusually low",
                      "just about average", "unusually high",
                      "exceptionally high")
  }

  # `var_measurement` well made
  if (!identical(names(var_measurement), c("df", "measurement"))) {
    stop("names of `var_measurement` column must be `df` and `measurement`.")
  }
  if (!all(avail_df %in% var_measurement$df)) {
    stop("One or more `avail_df` is missing in the `var_measurement` data.frame.")
  }
  if (!all(var_measurement$measurement %in% c("scalar", "ordinal", "nominal"))) {
    stop(paste0("One or more of `var_measurement$measurement` is other than ",
                "`scalar`, `ordinal` or `nominal` (the only possible options)."))
  }
  if (grepl("\\d{4}$", var_code)) {
    stop(paste0("`var_code` can not finish with 4 numerics. (This string at the end ",
                "of the variables code is reserved only to the year of the data.)"))
  }

    tibble::add_row(
      variables,
      var_code = as.character(var_code),
      type = list(type),
      var_title = as.character(var_title),
      var_short = as.character(var_short),
      explanation = as.character(explanation),
      explanation_nodet = as.character(explanation_nodet),
      exp_q5 = as.character(exp_q5),
      parent_vec = as.character(parent_vec),
      theme = as.character(theme),
      private = as.logical(private),
      pe_include = as.logical(pe_include),
      source = as.character(source),
      group_name = as.character(group_name),
      group_diff = list(group_diff),
      dates = list(dates),
      avail_df = list(avail_df),
      breaks_q3 = list(breaks_q3),
      breaks_q5 = list(breaks_q5),
      region_values = list(region_values),
      interpolated = list(interpolated),
      rankings_chr = list(rankings_chr),
      var_measurement = list(var_measurement)
    )
}

#' Get variable values for regions
#'
#' This function takes in a set of variables and scales, and iterates over
#' each variable to get its values for each region and year. The function
#' returns a data frame with the region, year, value, and count (if applicable)
#' for each variable.
#'
#' @param scales <`list`> Lists of spatial features dataframes with regions and
#' scales. List of two depths (region and scales).
#' @param vars <`character`> A character vector of variable codes. Unique variable
#' codes, no times appended.
#' @param types <`list`> A named list of variable types, one of: "`pct`", "`avg`",
#' "`median`", "`dollar`", "`count`", "`ind`", "`sqkm`", "`per1k`".
#' The names of the list should match the variable names in \code{vars}.
#' @param parent_strings <`list`> A named list of parent strings. The names of the list
#' should match the variable names in \code{vars}. The parent strings are used
#' to calculate the absolute values of certain variable types. We know the
#' absolute number of tenant households by multiplying the percentage with the
#' number of households. The parent_strings must be present in the same `df` as the
#' vars.
#' @param breaks <`named list`> A named list of dataframes of variable breaks
#' for `ind` variables. Usually the `q5_breaks_table` of \code{\link{calculate_breaks}}
#' The names of the list should match the variable names in \code{vars}.
#' @param time_regex <`character`> A regular expression used to identify the
#' years for which a variable has data. The default is "`_\\d{4}$`".
#' @param round_closest_5 <`logical`> Whether the absolute count or `pct` variables
#' should be rounded up to the closest 5, to not give impression of accuracy
#' we do not have. The census is rounded to a random 5 units.
#'
#' @return A list with dataframes of the region, year, value, and count (if
#' applicable) for each variable.
#' @export
variables_get_region_vals <- function(scales, vars, types, parent_strings = NULL,
                                      breaks = NULL, time_regex = "_\\d{4}$",
                                      round_closest_5 = TRUE) {
  progressr::with_progress({
    pb <- progressr::progressor(length(vars))

    future.apply::future_sapply(vars, \(var) {

      # Grab the variable types
      type <- unlist(types[[var]])

      # Missing arguments depending on type
      if (sum(type %in% c("pct", "avg", "median", "ind")) > 0 && is.null(parent_strings)) {
        stop(paste0("A variable of type `", type, "` must have a parent string."))
      }
      if (sum(type %in% c("ind")) > 0 && is.null(breaks)) {
        stop(paste0("A variable of type `", type, "` must have the breaks ",
                    "supplied, usually the `q5_breaks_table` output of ",
                    "`cc.buildr::calculate_breaks`."))
      }

      # Iterate the count over all regions
      region_vals <- mapply(\(region_name, region) {

        # Get the names of all the scales inside a region
        cols <- lapply(region, names)[length(region):1]
        # Grab the first scale which has a variable + parent corresponding to ours
        has_var <- sapply(cols, \(x) sum(grepl(paste0(var, time_regex), x)) > 0)
        has_parent <- sapply(cols, \(x) sum(grepl(paste0(unlist(parent_strings[var]), time_regex), x)) > 0)
        if (sum(has_var) == 0) return(data.frame())

        which_df_avail <- has_var + has_parent
        # Take the first one as it's the lowest level (more granularity in data)
        df_name <- names(which(which_df_avail == max(which_df_avail)))[[1]]
        df <- region[[df_name]]
        df_sf <- df
        df <- sf::st_drop_geometry(df)
        # Get all the years at which the variable is available
        all_var <- names(df)[grepl(paste0(var, time_regex), names(df))]
        all_var <- all_var[!grepl("_q3$|_q5$", all_var)]

        # Iterate over all years of the variable and get the right information
        # depending on the variable type
        out <- lapply(all_var, \(v) {

          out <- tibble::tibble(region = region_name,
                                year = gsub(var, "", v))
          out$year <- gsub("^_", "", out$year)

          if ("pct" %in% type) {
            parent_string <- parent_strings[[var]]
            if (is.na(parent_string) | is.null(parent_string)) {
              stop(sprintf("No parent_string found for `%s`", var))
            }
            parent_string_year <- paste0(parent_string, "_", out$year)
            no_nas <- df[!is.na(df[[parent_string_year]]) &
                           !is.na(df[[v]]), ]

            out$val <- stats::weighted.mean(no_nas[[v]], no_nas[[parent_string_year]])
            out$count <- out$val * sum(no_nas[[parent_string_year]])

            # Round if necessary
            if (round_closest_5) out$count <- round(out$count/5)*5

          } else if ("avg" %in% type || "median" %in% type || "dollar" %in% type) {
            parent_string <- parent_strings[[var]]
            if (is.na(parent_string) | is.null(parent_string)) {
              stop(sprintf("No parent_string found for `%s`", var))
            }
            parent_string_year <-
              if (out$year != "" & !parent_string %in% c("households", "population")) {
                paste0(parent_string, "_", out$year)
              } else parent_string
            no_nas <- df[!is.na(df[[parent_string_year]]) & !is.na(df[[v]]), ]

            out$val <- stats::weighted.mean(no_nas[[v]], no_nas[[parent_string_year]])
          } else if ("count" %in% type) {
            out$val <- sum(df[[v]], na.rm = TRUE)
          } else if ("ind" %in% type) {
            parent_string <- parent_strings[[var]]
            if (is.na(parent_string) | is.null(parent_string)) {
              stop(sprintf("No parent_string found for `%s`", var))
            }

            brk <- breaks[[var]]
            brk <- brk[grepl(paste0("^", region_name, "_"), brk$df), ]
            last_df <- unique(brk$df)
            # Grab the smallest scale that has both the var and the parent
            last_df <- last_df[grepl(df_name, last_df)]
            brk <- brk[brk$df == last_df, ]

            # Higher than the values of the second to last bracket
            val <- brk$var[brk$rank == 3]
            second_to_last <- df[df[[v]] > val, ]

            out$count <- sum(second_to_last[[parent_string]], na.rm = TRUE)
            out$val <- out$count / sum(df[[parent_string]], na.rm = TRUE)
          } else if ("sqkm" %in% type) {
            df <- sf:::`[.sf`(df_sf, v)
            df$area <- get_area(df)
            out$val <- stats::weighted.mean(df[[v]], df[["area"]])
          } else if ("per1k" %in% type) {
            df <- df[c(v, "population")]
            out$val <- stats::weighted.mean(df[[v]], df[["population"]])
          }

          # Switch NaN to NA
          out[is.na(out)] <- NA

          return(out)
        })

        # Make it in one ordered dataframe
        out <- Reduce(rbind, out)

        if ("year" %in% names(out))
          out <- out[order(out$year, decreasing = TRUE), ]

        return(out)

      }, names(scales), scales, SIMPLIFY = FALSE)

      pb()
      return(Reduce(rbind, region_vals))

    }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
  })
}

