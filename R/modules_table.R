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
      title = character(),
      metadata = logical(),
      dataset_info = character()
    )

  c(scales, list(modules = modules))
}

#' Add a new module to the modules table
#'
#' @param modules <`data.frame`> The \code{modules} data.frame to which add a
#' new row.
#' @param id <`character`> The module id, e.g. `canale`
#' @param title <`character`> The module title which will be used to navigate
#' to that module from the navigation bar, e.g. `Active living potential`
#' @param metadata <`logical`> Indicate if metadata is available from this module,
#' if there should be further data info when the `Export data` popup is clicked
#' (information regarding if the data has been interpolated, source, etc.).
#' @param dataset_info <`character`> HTML text with further data information
#' from the module.
#'
#' @return The same `modules` data.frame fed, with the added row.
#' @export
add_module <- function(modules, id, title, metadata, dataset_info) {

  new_module <- data.frame(id = id,
                           title = title,
                           metadata = metadata,
                           dataset_inf = dataset_info)

  rbind(modules, new_module)

}
