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
      avail_scale = list(),
      source = character(),
      interpolated = list(),
      rankings_chr = list(),
      rank_name = list(),
      rank_name_short = list(),
      var_measurement = list(),
      breaks_q5 = list()
    )

  list(scales = scales_consolidated, variables = variables)
}

#' Add a new variable to the variables table
#'
#' @param variables <`data.frame`> The \code{variables} data.frame to which add a
#' new row.
#' @param var_code <`character`> The code used to refer to the variables, e.g.
#' \code{alp}
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
#'  \item{"pct"}{Percentage. starts with a verb and follows the absolute count of the parent
#'  string. the string will read e.g. '50 households (3%) are tenants' where
#'  `are tenants` is the definition value.}
#'  \item{"count"}{Count. starts with a verb and follows the count string (with the
#'  'parent vector' which in this case is just a string). The string will
#'  read e.g. '50 households are tenants' where `are tenants` is the definition value,
#'  and `households` is the parent vector string (at the parent_vec argument).}
#'  \item{"dollar"}{Currency (dollar). starts with a subject and ends with a verb. It's assumed it
#'  will be followed by a dollar number. e.g. 'the average rent is 800$' where
#'  `the average rent is` is the definition value.}
#'  \item{"ind"}{Index. starts with a verb and uses a place holder written `_X_` which
#'  would translates to, e.g. 'medium to high'. example: '50 households are living
#'  in areas with low potential for active living' where the definition would
#'  be: `are living in areas with _X_ potential for active living`}
#'  \item{"avg"}{Average. starts with a subject and uses a place holder written `_X_` which
#'  would translates to a number. example: 'the average resident has access to 30
#'  grocery stores within 15 minutes by walk' where the definition would be:
#'  `the average resident has access to _X_ grocery stores within 15 minutes by walk`}
#'  \item{"sqkm"}{X per square kilometres. starts with a determinant and uses a
#'  placeholder written `_X_` which translates to a number. example: 'the density
#'  of green alleys is 2.28 square  metres per square kilometres' where the definition would be:
#'  `the density of green alleys is _X_ square metres per square kilometres`}
#'  \item{"per1k"}{X per 1,000 residents. starts with a determinant and uses a
#'  placeholder written `_X_` which translates to a number. example: 'the density
#'  of green alleys is 28.7 square metres per 1000 residents' where the definition would be:
#'  `the density of green alleys is _X_ square metres per 1000 residents`}
#'  \item{"ppo"}{People per object. Can be a single word of the object.
#'   example: 'there are 4 people for every tree' where the definition of 'exp_q5'
#'   would be `tree`}
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
#' @param avail_scale <`list`> All the combinations of region and scales
#' at which the data is available, e.g. `c("CMA_CSD", "CMA_CT", ...)`
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
#' @param rank_name <`character vector`> Vector of character to characterise
#' every 5 bins (breaks). Only used in `ind` types. Defaults to NULL, which will
#' be converted into Very low (1st bin), Low (2nd), Moderate, High, Very high.
#' @param rank_name_short <`character vector`> Same as `rank_name` but shorter
#' to fit easily on the legend.
#' @param var_measurement <`character list`> Data.frame where every row is an available
#' scale (`aval_scale`) and the second column is the measurement type. These will impact
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
#' @param breaks_q5 <`character/numeric vector`> If the q5 breaks should be
#' hardcoded, insert them here. Defaults to NULL.
#' @param allow_title_duplicate <`logical`> It is necessary to not have variable title
#' duplicates in the same dropdown menu, as the title is what is displayed to the
#' user. If there is a duplicate, the user will be selecting two variable codes
#' when selection a unique dropdown choice, and it will crash the app. Defaults
#' to `FALSE`. If you are certain the duplicated variable title will never be
#' used in a dropdown (ex. it is a parent variable), then switch to `TRUE` at
#' your own risks.
#'
#' @return The same `variables` data.frame fed, with the added row.
#' @export
add_variable <- function(variables, var_code, type, var_title,
                         var_short = as.character(var_title), explanation,
                         explanation_nodet = gsub("^the ", "", explanation),
                         exp_q5, parent_vec, group_name = NA,
                         group_diff = list(), theme, private, pe_include = FALSE,
                         dates, avail_scale, source, interpolated,
                         rankings_chr = c(
                           "exceptionally low", "unusually low",
                           "just about average", "unusually high",
                           "exceptionally high"
                         ),
                         rank_name = NULL, rank_name_short = NULL,
                         var_measurement = data.frame(
                           scale = avail_scale,
                           measurement = rep("scalar", length(avail_scale))
                         ), breaks_q5 = NULL,
                         allow_title_duplicate = FALSE) {
  if (var_code %in% variables$var_code) {
    stop(paste0("`", var_code, "` is a duplicate."))
  }

  if (var_code %in% variables$var_code) {
    stop(paste0("`", var_code, "` is already a `var_code` present in the variables table."))
  }

  # Necessary to not have issues with the dropdowns (unique title can't hold 2 variables)
  if (!allow_title_duplicate) {
    if (var_title %in% variables$var_title) {
      stop(paste0(
        "`", var_title, "` is already a `var_title` present in the variables table. ",
        "If the variable is a parent variable and will never be used in the ",
        "dropdown along with the other variable sharing the same title, use the ",
        "argument `allow_title_duplicate`."
      ))
    }
  }

  # If NULL is supplied, revert to the default
  if (all(is.null(rankings_chr))) {
    rankings_chr <- c(
      "exceptionally low", "unusually low",
      "just about average", "unusually high",
      "exceptionally high"
    )
  }

  # `var_measurement` well made
  if (!identical(names(var_measurement), c("scale", "measurement"))) {
    stop("names of `var_measurement` column must be `scale` and `measurement`.")
  }
  if (!all(avail_scale %in% var_measurement$scale)) {
    stop("One or more `avail_scale` is missing in the `var_measurement` data.frame.")
  }
  if (!all(var_measurement$measurement %in% c("scalar", "ordinal", "nominal"))) {
    stop(paste0(
      "One or more of `var_measurement$measurement` is other than ",
      "`scalar`, `ordinal` or `nominal` (the only possible options)."
    ))
  }
  if (grepl("\\d{4}$", var_code)) {
    stop(paste0(
      "`var_code` can not finish with 4 numerics. (This string at the end ",
      "of the variables code is reserved only to the year of the data.)"
    ))
  }
  if ("ind" %in% type) {
    if (is.null(rank_name)) {
      rank_name <- c("Very low", "Low", "Moderate", "High", "Very high")
    }
    if (is.null(rank_name_short)) {
      rank_name_short <- c("V. low", "Low", "Mod.", "High", "V. high")
    }
  }

  if (!is.null(breaks_q5)) {
    if (length(breaks_q5) != 6) {
      stop("breaks_q5 argument should be a vector of length 6.")
    }
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
    avail_scale = list(avail_scale),
    interpolated = list(interpolated),
    rankings_chr = list(rankings_chr),
    rank_name = list(rank_name),
    rank_name_short = list(rank_name_short),
    var_measurement = list(unique(var_measurement)),
    breaks_q5 = list(breaks_q5)
  )
}

