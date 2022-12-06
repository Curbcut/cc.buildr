## BUILD AND APPEND _name_ DATA

build_and_append__name_ <- function(scales_variables_modules, crs) {

  # Read and prepare data ---------------------------------------------------

  # Read the data placed in a folder in `dev/data/`
  data <- read.csv("dev/data/_name_/_name_.csv")


  # Get list of data variables ----------------------------------------------

  # Build a character vector of all data variables that will be added to all
  # scales. Average and additive vars are for interpolation. A count variable
  # like number of households is additive. The percentage of tenants is average.
  average_vars <- c("_name_") # names(data)[!grepl("ID$", names(data))]
  additive_vars <- c("_name_")
  vars <- c(average_vars, additive_vars)

  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  data_interpolated <-
    interpolate_from_census_geo(
      data = data,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "households",
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs
    )


  # Calculate breaks --------------------------------------------------------

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars
    )


  # Variables table ---------------------------------------------------------

  # For more information on how to append the information, read the
  # documentation of `add_variable`. Every variable needs to have its own entry
  # in the variables table. The following is an example.
  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "canale",
      type = "ind",
      var_title = "Can-ALE index",
      var_short = "Can-ALE",
      explanation = "the potential for active living",
      theme = "Urban life",
      private = FALSE,
      dates = with_breaks$avail_dates[[unique_var]],
      scales = data_interpolated$avail_scales,
      breaks_q3 = with_breaks$q3_breaks_table[[unique_var]],
      breaks_q5 = with_breaks$q5_breaks_table[[unique_var]],
      source = "McGill Geo-Social Determinants of Health Research Group",
      interpolated = data_interpolated$interpolated_ref
    )


  # Modules table -----------------------------------------------------------

  # Facultative. If a page is to be added accompanying this data, add modules
  # description. Read the documentation of `add_module`. If no module is to be
  # created, assign `scales_variables_modules$modules` to modules.
  modules <- scales_variables_modules$modules

  modules <-
      scales_variables_modules$modules |>
        add_module(
          id = "_name_",
          theme = "Urban life",
          nav_title = "Active living potential",
          title_text_title = "Active living potential: the CanALE index",
          title_text_main = paste0(),
          title_text_extra = paste0(),
          regions = unique(data_interpolated$avail_scales$geo),
          metadata = TRUE,
          dataset_info = paste0()
        )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}
