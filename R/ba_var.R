#' Build and append a new variable
#'
#' @param data <`data.frame`> Containing any number of column of THE SAME data
#' (different years possible), and an ID that corresponds to the base scale,
#' e.g. \code{"DA_ID"}.
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param base_scale <`character`> The denominator for which data should be
#' interpolated. Defaults to `households`. The other option is `population`.
#' Directly fed to \code{\link[cc.buildr]{interpolate_from_census_geo}}
#' @param weight_by <`character`> The denominator for which data should be
#' interpolated. Defaults to `households`. The other option is `population`.
#' Directly fed to \code{\link[cc.buildr]{interpolate_from_census_geo}}
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param time_regex <`character`> Regular expression which corresponds to
#' a timeframe, placed at the end of the `vars` vector. e.g. `\\d{4}` for
#' years.
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#' @param additive_vars <`charater`> See \code{\link[cc.buildr]{interpolate_from_census_geo}}.
#' @param average_vars <`charater`> See \code{\link[cc.buildr]{interpolate_from_census_geo}}.
#' @param variable_var_code <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_type <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_var_title <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_var_short <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_explanation <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_exp_q5 <`character`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_classification <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_theme <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_private <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_source <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param variable_pe_include <`charater`> See \code{\link[cc.buildr]{add_variable}}.
#' @param module_id <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_theme <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_nav_title <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_title_text_title <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_title_text_main <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_title_text_extra <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_metadata <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_dataset_info <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_dates <`numeric vector`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param module_main_dropdown_title <`charater`> Optional. See \code{\link[cc.buildr]{add_module}}.
#' @param overwrite <`logical`> Should the data already processed and stored be
#' overwriten?
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the new variable added, and possible a new
#' module.
#' @export
ba_var <- function(data, scales_variables_modules, base_scale,
                   scales_sequences,
                   weight_by = "households", crs,
                   additive_vars = c(), average_vars = c(),
                   time_regex = "_\\d{4}$", inst_prefix, variable_var_code,
                   variable_classification,
                   variable_type, variable_var_title,
                   variable_var_short, variable_explanation,
                   variable_exp_q5,
                   variable_theme, variable_private, variable_source,
                   variable_pe_include = TRUE,
                   module_id = NULL,
                   module_theme = NULL,
                   module_nav_title = NULL,
                   module_title_text_title = NULL,
                   module_title_text_main = NULL,
                   module_title_text_extra = NULL,
                   module_metadata = NULL,
                   module_dataset_info = NULL,
                   module_dates = NULL,
                   module_main_dropdown_title = NULL,
                   overwrite = FALSE) {
  # Get list of data variables ----------------------------------------------

  var <-
    names(data)[!grepl("ID$", names(data))]

  # Check if we are really looking at a single variable.
  unique_var <- unique(gsub(time_regex, "", var))

  if (length(unique_var) > 1) {
    stop(paste0(
      "This function can only add a single variable. Update the ",
      "`data` argument to include only an ID column (e.g. `DA_ID`) and ",
      "single data without timeframe (e.g. `green_space_sqkm`) or ",
      "multiple timeframes of the same variable (e.g. ",
      "`housing_tenant_2016`, `housing_tenant_2021`, ...)"
    ))
  }


  # Interpolate data to all possible scales ---------------------------------

  data_interpolated <-
    interpolate_from_census_geo(
      data = data,
      base_scale = base_scale,
      all_scales = scales_variables_modules$scales,
      weight_by = weight_by,
      crs = crs,
      additive_vars = additive_vars,
      average_vars = average_vars,
      overwrite = overwrite,
      time_regex = time_regex,
      inst_prefix = inst_prefix
    )


  # Declare the types of the variables in a named list ----------------------

  types <- list(variable_type)
  names(types) <- unique_var


  # Data tibble -------------------------------------------------------------

  data_construct(scales_data = data_interpolated$scales,
                 unique_var = unique_var,
                 time_regex = time_regex,
                 inst_prefix = inst_prefix)


  # Variables table ---------------------------------------------------------

  ## Dates at which the data is available
  dates <- curbcut::s_extract(time_regex, var)
  dates <- gsub("^_", "", dates)

  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = variable_var_code,
      type = variable_type,
      var_title = variable_var_title,
      var_short = variable_var_short,
      explanation = variable_explanation,
      exp_q5 = variable_exp_q5,
      parent_vec = weight_by,
      theme = variable_theme,
      classification = variable_classification,
      private = variable_private,
      pe_include = variable_pe_include,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      source = variable_source,
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex)
    )


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)


  # Modules table -----------------------------------------------------------

  modules <-
    if (!is.null(module_id) &&
      !is.null(module_nav_title) &&
      !is.null(module_theme) &&
      !is.null(module_metadata) &&
      !is.null(module_dataset_info) &&
      !is.null(module_title_text_title) &&
      !is.null(module_title_text_main) &&
      !is.null(module_title_text_extra)) {
      scales_variables_modules$modules |>
        add_module(
          id = module_id,
          theme = module_theme,
          nav_title = module_nav_title,
          title_text_title = module_title_text_title,
          title_text_main = module_title_text_main,
          title_text_extra = module_title_text_extra,
          metadata = module_metadata,
          dataset_info = module_dataset_info,
          var_left = variable_var_code,
          dates = module_dates,
          main_dropdown_title = module_main_dropdown_title,
          default_var = variable_var_code,
          avail_scale_combinations = avail_scale_combinations
        )
    } else {
      scales_variables_modules$modules
    }


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))
}
