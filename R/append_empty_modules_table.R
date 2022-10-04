#' Append an empty modules table to the scales list
#'
#' `append_empty_modules_table()` takes the output of
#' \code{\link[susbuildr]{append_empty_variables_table}} and appends an empty
#' modules table.
#'
#' @param scales <`named list`> A named list containings the scales and the
#' empty variables table. The output of
#' \code{\link[susbuildr]{append_empty_variables_table}}
#'
#' @return A list with the second index being an empty variables table.
#' @export
append_empty_modules_table <- function(scales) {

  modules <-
    data.frame(
      id = character(),
      metadata = logical(),
      dataset_info = character()
    )

  c(scales, list(modules = modules))
}
