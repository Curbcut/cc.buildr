#' Append an empty modules table to the scales list
#'
#' `append_empty_modules_table()` takes the output of
#' \code{\link[cc.buildr]{consolidate_scales}} and appends an empty
#' modules table.
#'
#' @param scales <`named list`> A named list containings the scales and the
#' empty variables table. The output of
#' \code{\link[cc.buildr]{consolidate_scales}}
#'
#' @return A list with the second index being an empty variables table.
#' @export
append_empty_modules_table <- function(scales) {
  modules <-
    tibble::tibble(
      id = character(),
      nav_title = character(),
      title_text_title = character(),
      title_text_main = character(),
      title_text_extra = character(),
      metadata = logical(),
      dataset_info = character(),
      regions = list()
    )

  c(scales, list(modules = modules))
}

#' Add a new module to the modules table
#'
#' @param modules <`data.frame`> The \code{modules} data.frame to which add a
#' new row.
#' @param id <`character`> The module id, e.g. `canale`
#' @param nav_title <`character`> The module title which will be used to navigate
#' to that module from the navigation bar, e.g. `Active living potential`
#' @param title_text_title <`character`> This title text title will be used
#' once the user is in the module, to describe what they are navigating,
#' e.g. `Active living potential: the CanALE index`.
#' to that module from the navigation bar, e.g. `Active living potential`
#' @param title_text_main <`character`> The title text main paragraph is the main
#' text used to describe the module, on the left-side panel.
#' @param title_text_extra <`character`> Further information and resources the user
#' can navigate if they press on the 'Learn more' button in the left-side panel.
#' @param metadata <`logical`> Indicate if metadata is available from this module,
#' if there should be further data info when the `Export data` popup is clicked
#' (information regarding if the data has been interpolated, source, etc.).
#' @param dataset_info <`character`> HTML text with further data information
#' from the module.
#' @param regions <`character vector`> List of all the regions the module should be
#' able to show.
#'
#' @return The same `modules` data.frame fed, with the added row.
#' @export
add_module <- function(modules, id, nav_title, title_text_title, title_text_main,
                       title_text_extra, metadata, dataset_info, regions = NULL) {
  new_module <-
    tibble::tibble(
      id = id,
      nav_title = nav_title,
      title_text_title = title_text_title,
      title_text_main = title_text_main,
      title_text_extra = title_text_extra,
      regions = if (is.null(regions)) list(NULL) else list(regions),
      metadata = metadata,
      dataset_inf = dataset_info
    )

  rbind(modules, new_module)
}
