build_and_append_canale <- function(scales_variables_modules, crs) {

  # Interpolate DA canale data to all possible scales -----------------------

  data_interpolated <-
    susbuildr::interpolate_from_census_geo(
      data = susbuildr::canale_data,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "households",
      crs = crs)


  # Get list of data variables ----------------------------------------------

  vars <-
    names(susbuildr::canale_data)[!grepl("ID$", names(susbuildr::canale_data))]


  # Calculate breaks --------------------------------------------------------

  with_breaks <-
    susbuildr::calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars)


  # Variables table ---------------------------------------------------------

  # Dates where the data is available
  dates <- unique(with_breaks$q5_breaks_table$canale_2016$date)
  # Scales at which the data is available
  avail_scales <-
    map_over_scales(data_interpolated$interpolated_ref,
                    fun = \(geo = geo, scale_name = scale_name, ...) {
                      data.frame(geo = geo,
                                 scale = scale_name)
                    })
  avail_scales <-
    sapply(avail_scales, \(x) do.call(rbind, x),
           simplify = FALSE, USE.NAMES = TRUE) |>
    (\(x) do.call(rbind, x))()
  row.names(avail_scales) <- NULL
  # Scales at which data has been interpolated
  interpolated_key <-
    map_over_scales(data_interpolated$interpolated_ref,
                    fun = \(geo = geo, scale_name = scale_name,
                            scale_df = scale_df, ...) {
                      data.frame(geo = geo,
                                 scale = scale_name,
                                 interpolated_from = scale_df)
                    })
  interpolated_key <-
    sapply(interpolated_key, \(x) do.call(rbind, x),
           simplify = FALSE, USE.NAMES = TRUE) |>
    (\(x) do.call(rbind, x))()
  row.names(interpolated_key) <- NULL

  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "canale",
      type = "ind",
      var_title = "CanALE index",
      var_short = "CanALE",
      explanation = "the potential for active living",
      theme = "Urban life",
      private = FALSE,
      dates = dates,
      scales = avail_scales,
      breaks_q3 = with_breaks$q3_breaks_table$canale_2016,
      breaks_q5 = with_breaks$q5_breaks_table$canale_2016,
      source = "McGill Geo-Social Determinants of Health Research Group",
      interpolated = interpolated_key)


  # Modules table -----------------------------------------------------------


  modules <-
    scales_variables_modules$modules |>
    add_modules(
      id = "canale",
      metadata = TRUE,
      dataset_info =
        paste0("<p><a href = 'https://nancyrossresearchgroup.ca/research/can-ale/'>",
               "The Canadian Active Living Environments (Can-ALE)</a> dataset is ",
               "a geographic-based set of measures charac",
               "terizing the active living environments (often referred to as '",
               "walkability') of Canadian communities. The data is provided at ",
               "the dissemination area level.</p>"))


  # Return ------------------------------------------------------------------

  return(scales = with_breaks$scales,
         variables = variables,
         modules = modules)

}
