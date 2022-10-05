#' Append an empty variables table to the scales list
#'
#' `append_empty_variables_table()` adds a first level to the scales list, and
#' places an empty variables table to the scales list.
#'
#' @param scales_consolidated <`named list`> A named list of sf data.frame
#' containing all scales listed with their geos. The output of
#' \code{\link[susbuildr]{consolidate_scales}}.
#'
#' @return A list with the second index being an empty variables table.
#' @export
append_empty_variables_table <- function(scales_consolidated) {

  variables <-
    data.frame(
      var_code = character(),
      type = character(),
      var_title = character(),
      var_short = character(),
      explanation = character(),
      group_name = character(),
      group_diff = list(),
      theme = character(),
      private = logical(),
      dates = list(),
      scales = list(),
      breaks_q3 = list(),
      breaks_q5 = list(),
      source = character(),
      interpolated = list()
    )

  list(scales = scales_consolidated, variables = variables)
}

#' Add a new variable to the variables table
#'
#' @param variables <`data.frame`> The \code{variables} data.frame to which add a
#' new column.
#' @param var_code <`character`> The code used to refer to the variables, e.g.
#' \code{canale}
#' @param type <`character`> The variable type. One of \code{"ind"}, \code{"pct"},
#' \code{"avg"}, \code{"median"}, \code{"per1k"}, \code{"sqkm"} or \code{"count"}.
#' @param var_title <`character`> The variable title
#' @param var_short <`character`> A short variable title used in graphs or where
#' space is limited. Preferably ~ <12 characters.
#' @param explanation <`character`> Variable explanation, e.g. the percentage of
#' private dwellings occupied by tenants
#' @param group_name <`character`> The name of the larger group to which the
#' variable belongs. e.g. for the variable accessibility to public schools by bike,
#' the group_name would be \code{"Accessibility to schools"}
#' @param group_diff <`named list`> A named list for when the variable is part
#' of a greater group. e.g. accessibility to public schools by bike, the bigger group
#' is `Accessibility to schools`, and bike is a group differentiation.
#' e.g. The \code{list("Mode of transport" = "By bike", "Public/Prviate" = "Public")}.
#' The list can contain multiple named vectors, multiple group differentiation.
#' @param theme <`character`> The theme to which the variable belongs, e.g. "Housing",
#' "Urban life", ...
#' @param private <`logical`> If we have permissions to make the variable available
#' for public download.
#' @param dates <`character vector`> A vector of dates for which the data is available.
#' @param scales <`data.frame`> All the scales at which the data is available
#' arranged in a data.frame of two columns: \code{geo} and \code{scale}
#' @param breaks_q3 <`data.frame`> A data.frame with with information regarding
#' scales, date, rank, breaks. The lst outputs of \code{\link[susbuildr]{calculate_breaks}}
#' @param breaks_q5 <`data.frame`> A data.frame with with information regarding
#' scales, date, rank, breaks. The lst outputs of \code{\link[susbuildr]{calculate_breaks}}
#' @param source <`character`> The source where the data comes from, e.g.
#' "McGill Geo-Social Determinants of Health Research Group"
#' @param interpolated <`data.frame`> A data.frame indicating from which scale
#' the geo/scale comination has been interpolated. The non-interpolated data
#' is populated with \code{"FALSE"}
#'
#' @return The same `variables` data.frame fed, with the added row.
#' @export
add_variable <- function(variables, var_code, type, var_title,
                         var_short = as.character(var_title), explanation,
                         group_name = NA_character_, group_diff = list(),
                         theme, private, dates, scales, breaks_q3,
                         breaks_q5, source, interpolated) {

  if (var_code %in% variables$var_code) {
    stop(paste0("`", var_code, "` is a duplicate."))
  }

  new_variable <-
    data.frame(
      var_code = as.character(var_code),
      type = as.character(type),
      var_title = as.character(var_title),
      var_sort = as.character(var_short),
      explanation = as.character(explanation),
      theme = as.character(theme),
      private = as.logical(private),
      source = as.character(source),
      group_name = as.character(group_name))

  new_variable$dates <- list(dates)
  new_variable$scales <- list(scales)
  new_variable$breaks_q3 <- list(breaks_q3)
  new_variable$breaks_q5 <- list(breaks_q5)
  new_variable$interpolated <- list(interpolated)
  new_variable$group_diff <- list(group_diff)

  rbind(variables, new_variable)

}
