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
