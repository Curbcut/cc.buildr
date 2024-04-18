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
      preview = character(),
      metadata = logical(),
      dataset_info = character(),
      regions = list(),
      var_left = list(),
      dates = list(),
      main_dropdown_title = character(),
      add_advanced_controls = list(),
      default_var = character(),
      avail_scale_combinations = list(),
      additional_schemas = list()
    )

  c(scales, list(modules = modules))
}

#' Add a new module to the modules table
#'
#' @param modules <`data.frame`> The \code{modules} data.frame to which add a
#' new row.
#' @param id <`character`> The module id, e.g. `alp`
#' @param theme <`character`> The theme in which the module should be, e.g.
#' `Housing`, `Urban life`, `Transport`, ... If the module is not to be displayed,
#' keep the default empty string `""`. The module will be hidden.
#' @param nav_title <`character`> The module title which will be used to navigate
#' to that module from the navigation bar, e.g. `Active living potential`
#' @param title_text_title <`character`> This title text title will be used
#' once the user is in the module, to describe what they are navigating,
#' e.g. `Active living potential`.
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
#' * group_diff <`named list`> A named list is used to represent a variable
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
#' @param dates <`numeric vector`> In the case where var_left is used, supply a umeric
#' vector of dates. NULL if there are no dates for the variables. This will create
#' a time slider widget on the module.
#' @param main_dropdown_title <`character`> In the case where var_left is used,
#' supply a character for the label of the main dropdown. NULL if there should be no
#' dropdown label.
#' @param add_advanced_controls <`character vector`> Names of additional widgets
#' that should be placed in the 'Advanced controls' instead of with the 'Indicators'
#' section (Names of the names list `group_diff` of the `var_left` column.). If
#' the main dropdown should also be palced under the advanced controls, add
#' `'mnd'` in there too.
#' @param default_var <`character`> Character that represents the default variable,
#' the first the user will see when arriving on the page. Defaults to NA for
#' pages that do not display variables: stories, place explorer, ... For any
#' other page, the default_var must be one of the available variable in the
#' `var_left` column.
#' @param avail_scale_combinations <`character vector`> All available scale combinations
#' for display on the module. e.g. `c(CSD_CT_DA_building, borough_CT_DA_building ...)`.
#' These should all represent an available tileset as an autozoom, but also individual
#' tilesets and scales.
#' @param additional_schemas <`character vector`> Which widgets are part of the
#' data schema? e.g. for `access`, every access data has, in its schema,
#' `schema$transportationtime = "_\\d{1,2}_"`. As every variable has, as a widget,
#' `"Transport time"`. The value of this widget does not result in a loading of
#' a new `data`, but instead is necessary to subset which column of `data` is
#' the one under study. `additional_schemas` needs to, by itself, be a valid
#' schema, ex. `list(transportationtime = 20)`. Defaults to NULL.
#'
#'
#' @return The same `modules` data.frame fed, with the added row.
#' @export
add_module <- function(modules, id, theme = "", nav_title, title_text_title,
                       title_text_main, title_text_extra, metadata, dataset_info,
                       regions = NULL, var_left = NULL, dates = NULL,
                       main_dropdown_title = NA_character_,
                       add_advanced_controls = NULL, default_var = NA,
                       avail_scale_combinations = NULL, additional_schemas = NULL) {
  if (is.data.frame(var_left)) {
    if (!all(names(var_left) == c("var_code", "group_name", "group_diff"))) {
      stop(paste0(
        "The tibble given to `var_left` must have the following columns: ",
        paste0(c("var_code", "group_name", "group_diff"), collapse = ", ")
      ))
    }
  }
  if (id %in% modules$id) {
    stop("modules `id` must be unique")
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
    add_advanced_controls = list(add_advanced_controls),
    default_var = default_var,
    avail_scale_combinations = list(avail_scale_combinations),
    additional_schemas = list(additional_schemas)
  )
}

#' Add right variables to Modules
#'
#' This function iterates over the modules in a `scales_variables_modules$modules` object,
#' identifying and adding the appropriate right variables based on the classification
#' of left-side variables. It ensures that all right-side variable are valid and relevant.
#'
#' @param svm <`list`> scales_variables_modules list object.
#' @param exclude_scales <`character vector`> Specifies the scales to be excluded from
#' the selection process of right variables. This is useful in scenarios where the
#' absence of data for certain scales (e.g., buildings not containing specific data)
#' should not affect the determination of right variables for a module. For instance,
#' if a module's autozoom feature displays buildings with DA data, scales related to
#' buildings without data can be excluded to ensure relevant right variable selection.
#'
#' @return <`list`> The modified `svm` object with `var_right` added to each
#' module in the `modules` dataframe.
#' @export
add_var_right <- function(svm, ignore_to_choose_vr = c("^grd\\d", "^DB$", "^building$")) {
  modules <- svm$modules
  variables <- svm$variables

  vrs <- lapply(modules$id, \(id) {
    vl <- modules$var_left[modules$id == id][[1]]
    if (is.list(vl)) vl <- vl$var_code

    classification <- variables$classification[variables$var_code %in% vl]
    classification <- unique(classification)

    if (length(classification) > 1) {
      stop(sprintf("`%s` module have left variables with different classification.", id))
    }
    if (length(classification) == 0) return()

    variables_no_nas <- variables[!is.na(variables$classification), ]
    # If the classification is socio-demographic, we do not compare it with
    # other sociodemographic variables
    if (classification == "sociodemo")  {
      variables_no_nas <- variables_no_nas[
        variables_no_nas$classification != "sociodemo", ]
    }

    vr <- variables_no_nas$var_code[variables_no_nas$pe_include]

    # Only use right vars that are available at all possible scales
    scales <- modules$avail_scale_combinations[modules$id == id][[1]]
    splitted <- strsplit(scales, "_")
    splitted <- unlist(splitted)
    splitted <- splitted[!grepl(paste0(ignore_to_choose_vr, collapse = "|"), splitted)]
    possible_scales <- unique(splitted)
    possible_vr <- sapply(vr, \(v) {
      var_availability <- variables$avail_scale[variables$var_code == v][[1]]
      all(possible_scales %in% var_availability)
    })

    out_vr <- names(possible_vr[possible_vr])

    # Impossible to compare with the same variable
    out_vr[!out_vr %in% vl]
  })

  svm$modules$var_right <- vrs
  return(svm)
}

#' Add high correlation combinations to modules object
#'
#' This function calculates correlations between variables defined in the
#' `var_left` and `var_right` of each module within the `modules` dataset of the
#' provided svm object. It then updates the svm object with these high correlation
#' combinations (correlation > 0.7) for dropdown styling. The function
#' specifically targets datasets located within specified scales and excludes
#' "building" scale when multiple scales are available. It utilizes `future.apply`
#' for parallel computation of correlations, aiming to enhance performance.
#'
#' @param svm <`list`> scales_variables_modules list object.
#'
#' @return <`list`> The modified svm object with an added `high_corr_combination` list
#' to each module within the `modules` data frame. This list contains names of
#' variables in `var_right` that have a high correlation (> 0.7) with variables in
#' `var_left`, for the chosen scale.
#' @export
add_high_corr_combination <- function(svm) {
  modules <- svm$modules
  modules_corr <- lapply(modules$id, \(id) {
    # Get this module
    this_mod <- modules[modules$id == id, ]
    vl <- this_mod$var_left[[1]]
    if (is.list(vl)) vl <- vl$var_code
    vr <- this_mod$var_right[[1]]

    # Decide which scale for which to calculate correlations
    scales <- this_mod$avail_scale_combinations[[1]]
    if (is.null(scales)) return(NULL)
    scale <- if (length(scales) == 1 & !grepl("_", scales)[1]) {
      scales
    } else {
      splitted <- strsplit(scales, "_")
      splitted <- unlist(splitted)
      splitted <- splitted[!grepl("^grd|building", splitted)]
      names(strsplit(splitted, "_") |> unlist() |> table() |> which.max())[1]
    }

    # Calculate all correlations and only keep the ones with a high correlation
    future.apply::future_sapply(vl, \(l) {
      correlations <- sapply(vr, \(r) {
        lv_file_path <- sprintf("data/%s/%s.qs", scale, l)
        rv_file_path <- sprintf("data/%s/%s.qs", scale, r)

        df_lv <- qs::qread(lv_file_path)
        df_lv <- df_lv[c("ID", attributes(df_lv)$breaks_var)]
        df_rv <- qs::qread(rv_file_path)
        df_rv <- df_rv[c("ID", attributes(df_rv)$breaks_var)]

        merged_df <- merge(df_lv, df_rv, by = "ID")
        cor_value <- cor(merged_df[[2]], merged_df[[3]], use = "complete.obs")
        abs(cor_value)
      })
      names(which(correlations > 0.7))
    }, simplify = FALSE, USE.NAMES = TRUE)

  })
  svm$modules$high_corr_combination <- modules_corr
  return(svm)
}
