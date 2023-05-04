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
      theme = character(),
      nav_title = character(),
      title_text_title = character(),
      title_text_main = character(),
      title_text_extra = character(),
      metadata = logical(),
      dataset_info = character(),
      regions = list(),
      var_left = list(),
      dates = list(),
      main_dropdown_title = character(),
      var_right = list()
    )

  c(scales, list(modules = modules))
}

#' Add a new module to the modules table
#'
#' @param modules <`data.frame`> The \code{modules} data.frame to which add a
#' new row.
#' @param id <`character`> The module id, e.g. `canale`
#' @param theme <`character`> The theme in which the module should be, e.g.
#' `Housing`, `Urban life`, `Transport`, ... If the module is not to be displayed,
#' keep the default empty string `""`. The module will be hidden.
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
#' @param var_left <`character vector or tibble`> This is used to create basic
#' modules. If a character vector is supplied, the module will show a single
#' dropdown with the options of the variables. If a tibble is supplied, dynamic
#' widgets will be introduces based on the values of group_diff. The columns of
#' the tibble must be `var_code`, `group_name` and `group_diff`.
#' * group_name <`character`> The name of the larger group to which the
#' variable belongs. e.g. for the variable accessibility to public schools by bike,
#' the group_name would be \code{"Accessibility to schools"}. This will be one of
#' the values in the main dropdown.
#' * group_diff <`named list`> A named list for when the variable is part
#' of a greater group. e.g. accessibility to public schools by bike, the bigger group
#' is `Accessibility to schools`, and bike is a group differentiation.
#' e.g. The \code{list("Mode of transport" = "By bike", "Public/Prviate" = "Public")}.
#' The list can contain multiple named vectors, multiple group differentiation. In the
#' case of the example, two dropdowns will be added dynamically: Modes of transport
#' and Public/Private, which will include all the values of the variables sharing
#' the same `group_name`.
#' @param dates <`numeric vector`> In the case where var_left is used, supply a umeric
#' vector of dates. NULL if there are no dates for the variables. This will create
#' a time slider widget on the module.
#' @param main_dropdown_title <`character`> In the case where var_left is used,
#' supply a character for the label of the main dropdown. NULL if there should be no
#' dropdown label.
#' @param var_right <`character vector`> Character vector of variable codes that
#' there should be in the 'compare' dropdown of the page.
#'
#'
#' @return The same `modules` data.frame fed, with the added row.
#' @export
add_module <- function(modules, id, theme = "", nav_title, title_text_title,
                       title_text_main,
                       title_text_extra, metadata, dataset_info, regions = NULL,
                       var_left = NULL, dates = NULL, main_dropdown_title = NA_character_,
                       var_right = NULL) {

  if (is.data.frame(var_left)) {
    if (!all(names(var_left) == c("var_code", "group_name", "group_diff"))) {
      stop(paste0("The tibble given to `var_left` must have the following columns: ",
                  paste0(c("var_code", "group_name", "group_diff"), collapse = ", ")))
    }
  }

  tibble::add_row(
    modules,
    id = id,
    theme = theme,
    nav_title = nav_title,
    title_text_title = title_text_title,
    title_text_main = title_text_main,
    title_text_extra = title_text_extra,
    regions = if (is.null(regions)) list(NULL) else list(regions),
    metadata = metadata,
    dataset_info = dataset_info,
    var_left = list(var_left),
    dates = list(dates),
    main_dropdown_title = main_dropdown_title,
    var_right = list(var_right)
  )

}
