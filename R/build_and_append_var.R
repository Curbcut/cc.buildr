#' Build and append a new variable
#'
#' @param data <`data.frame`> Containing any number of column of THE SAME data
#' (different years possible), and an ID that corresponds to the base scale,
#' e.g. \code{"DA_ID"}.
#' @param scales_variables_modules <`names list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param base_scale <`character`> The denominator for which data should be
#' interpolated. Defaults to `households`. The other option is `population`.
#' Directly fed to \code{\link[susbuildr]{interpolate_from_census_geo}}
#' @param weight_by <`character`> The denominator for which data should be
#' interpolated. Defaults to `households`. The other option is `population`.
#' Directly fed to \code{\link[susbuildr]{interpolate_from_census_geo}}
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `\\d{4}` for
#' years.
#' @param variable_var_code <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param variable_type <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param variable_var_title <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param variable_var_short <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param variable_explanation <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param variable_theme <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param variable_private <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param variable_source <`charater`> See \code{\link[susbuildr]{add_variable}}.
#' @param module_id <`charater`> Optional. See \code{\link[susbuildr]{add_module}}.
#' @param module_title <`charater`> Optional. See \code{\link[susbuildr]{add_module}}.
#' @param module_geos <`charater`> Optional. See \code{\link[susbuildr]{add_module}}.
#' @param module_metadata <`charater`> Optional. See \code{\link[susbuildr]{add_module}}.
#' @param module_dataset_info <`charater`> Optional. See \code{\link[susbuildr]{add_module}}.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the new variable added, and possible a new
#' module.
#' @export
build_and_append_var <- function(data, scales_variables_modules, base_scale,
                                 weight_by = "households", crs,
                                 time_regex = "\\d{4}", variable_var_code,
                                 variable_type, variable_var_title,
                                 variable_var_short, variable_explanation,
                                 variable_theme, variable_private, variable_source,
                                 module_id = NULL, module_title = NULL,
                                 module_geos = NULL, module_metadata = NULL,
                                 module_dataset_info = NULL) {

  # Get list of data variables ----------------------------------------------

  var <-
    names(data)[!grepl("ID$", names(data))]

  # Check if we are really looking at a single variable.
  time_regex_end <- paste0("_", time_regex, "$")
  unique_var <- unique(gsub(time_regex_end, "", var))

  if (length(unique_var) > 1)
    stop(paste0("This function can only add a single variable. Update the ",
                "`data` argument to include only an ID column (e.g. `DA_ID`) and ",
                "single data without timeframe (e.g. `green_space_sqkm`) or ",
                "multiple timeframes of the same variable (e.g. ",
                "`housing_tenant_2016`, `housing_tenant_2021`, ...)"))


  # Interpolate DA canale data to all possible scales -----------------------

  data_interpolated <-
    susbuildr::interpolate_from_census_geo(
      data = data,
      base_scale = base_scale,
      all_scales = scales_variables_modules$scales,
      weight_by = weight_by,
      crs = crs)


  # Calculate breaks --------------------------------------------------------

  with_breaks <-
    susbuildr::calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = var)


  # Variables table ---------------------------------------------------------

  variables <-
    susbuildr::add_variable(
      variables = scales_variables_modules$variables,
      var_code = variable_var_code,
      type = variable_type,
      var_title = variable_var_title,
      var_short = variable_var_short,
      explanation = variable_explanation,
      theme = variable_theme,
      private = variable_private,
      dates = with_breaks$avail_dates[[unique_var]],
      scales = data_interpolated$avail_scales,
      breaks_q3 = with_breaks$q3_breaks_table[[unique_var]],
      breaks_q5 = with_breaks$q5_breaks_table[[unique_var]],
      source = variable_source,
      interpolated = data_interpolated$interpolated_ref)


  # Modules table -----------------------------------------------------------

  modules <-
    if (!is.null(module_id) &&
        !is.null(module_title) &&
        !is.null(module_metadata) &&
        !is.null(module_dataset_info)) {
      geos <- if (is.null(module_geos)) NULL else module_geos
      scales_variables_modules$modules |>
        susbuildr::add_module(
          id = module_id,
          title = module_title,
          geos = geos,
          metadata = module_metadata,
          dataset_info = module_dataset_info)
    } else {
      scales_variables_modules$modules
    }


  # Return ------------------------------------------------------------------

  return(list(scales = with_breaks$scales,
              variables = variables,
              modules = modules))

}
