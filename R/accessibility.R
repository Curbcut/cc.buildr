#' Build and append pre-processed Access to Amenities
#'
#' Calculates the average count of amenities (such as schools, food distributors,
#' health care facilities, etc.) accessible by walk, bike, transit, or car within a
#' given amount of time and add it to `scales_variables_modules`
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database.
#' @param themes <`character vector`> All pre-processed themes to import.
#' Defaults to everything: `cc.data::accessibility_themes`
#' @param traveltimes <`named list`>A list of matrices containing travel times
#' between origin-destination pairs. The output of
#' \code{\link[cc.buildr]{accessibility_get_travel_times}}
#' @param time_intervals <`numeric vector`> A vector of time intervals
#' (in minutes) to calculate travel times for. Defaults to `c(5, 10, 15, ...,
#' 60)`. 60 minutes is the maximum.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param pe_include <`character vector`> Which final variables should appear
#' in the place explorer. Defaults to `c("access_foot_20_education_elementary", "access_foot_20_communitycentres_individual", "access_foot_15_fooddistribution_grocery", "access_car_10_healthcare_hospitals")`.
#'
#' @return A list containing the scales, variables, and modules tables.
#' @export
ba_accessibility_points <- function(scales_variables_modules,
                                    region_DA_IDs,
                                    themes = cc.data::list_accessibility_themes(),
                                    traveltimes,
                                    time_intervals = which(1:60 %% 5 == 0),
                                    pe_include = c(
                                      "access_foot_20_education_elementary",
                                      "access_foot_20_cultural_total",
                                      "access_foot_15_food_grocery",
                                      "access_car_10_healthcare_hospitals"
                                    ),
                                    crs) {
  if (max(time_intervals) > 60) {
    stop(paste0(
      "The maximum time interval available in the travel time ",
      "matrices is 60 minutes"
    ))
  }

  # Get the data from the MySQL server --------------------------------------

  # Filter the variable codes to retrieve
  dict <- cc.data::accessibility_point_dict
  vars <- dict$var[dict$theme %in% themes]

  point_per_DA <- cc.data::db_read_data(
    table = "accessibility_point_DA",
    columns = vars,
    column_to_select = "DA_ID",
    IDs = region_DA_IDs,
    crs = crs
  )


  # Arrange all the point data in final variables ---------------------------

  ttm_data <- accessibility_add_intervals(
    point_per_DA = point_per_DA,
    region_DA_IDs = region_DA_IDs,
    traveltimes = traveltimes,
    time_intervals = time_intervals
  )
  # qs::qsave(ttm_data, "test_build_mtl/ttm_data.qs")
  # ttm_data <- qs::qread("test_build_mtl/ttm_data.qs")


  # Interpolate -------------------------------------------------------------

  average_vars <- names(ttm_data)[!grepl("ID$", names(ttm_data))]
  names(ttm_data)[1] <- "DA_ID"

  data_interpolated <-
    interpolate_from_census_geo(
      data = ttm_data,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "population",
      crs = crs,
      average_vars = average_vars
    )


  # Calculate breaks --------------------------------------------------------

  types <- rep(list("avg"), length(average_vars))
  names(types) <- average_vars

  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = average_vars,
      types = types,
      time_regex = ""
    )

  # Calculate region values -------------------------------------------------

  # Parent strings
  parent_strings <- rep(list("population"), length(average_vars))
  names(parent_strings) <- average_vars

  # Region values
  region_values <- variables_get_region_vals(
    scales = with_breaks$scales,
    vars = average_vars,
    types = types,
    parent_strings = parent_strings,
    breaks = with_breaks$q5_breaks_table,
    time_regex = "",
    round_closest_5 = FALSE)


  # Variable measurements ----------------------------------------------------

  var_measurement <- data.frame(
    df = data_interpolated$avail_df,
    measurement = rep("scalar", length(data_interpolated$avail_df)))

  var_measurement$measurement[grepl("_DA$", var_measurement$df)] <-
    rep("ordinal", length(var_measurement$measurement[grepl("_DA$", var_measurement$df)]))


  # Variables table ---------------------------------------------------------

  new_variables <- lapply(average_vars, \(var) {

    dict <- cc.data::accessibility_point_dict
    dict <- dict[sapply(dict$var, grepl, var), ]

    mode <- (\(x) {
      if (grepl("_car_", var)) return("car")
      if (grepl("_foot_", var)) return("walking")
      if (grepl("_bicycle_", var)) return("bicycle")
      if (grepl("_transit_opwe_", var)) return("public transit on off-peak weekend days")
      if (grepl("_transit_pwe_", var)) return("public transit on peak weekend days")
      if (grepl("_transit_nwd_", var)) return("public transit on weekdays at night")
      if (grepl("_transit_nwe_", var)) return("public transit on weekends at night")
      if (grepl("_transit_opwd_", var)) return("public transit on off-peak weekdays")
      if (grepl("_transit_pwd_", var)) return("public transit on peak weekdays")
    })()

    time <- gsub("_", "", stringr::str_extract(var, "_\\d*_"))

    var_title <- stringr::str_to_sentence(paste0(dict$title, " accessible by ", mode))
    var_short <- stringr::str_to_sentence(dict$short)
    explanation <- paste0(
      "the number of ", tolower(dict$title),
      " an average resident of the area can reach within ", time, " minutes by ", mode
    )
    exp_q5 <- paste0(
      "the average resident has access to _X_ ", tolower(dict$title), " within ", time,
      " minutes by ", mode
    )

    theme <- (\(x) {
      if (dict$theme == "retail") return("retail stores")
      if (dict$theme == "finance") return("finance establishments")
      if (dict$theme == "food") return("food distributors")
      if (dict$theme == "healthcare") return("healthcare facilities")
      if (dict$theme == "educational") return("schools")
      if (dict$theme == "cultural") return("cultural facilities")
    })()
    group_name <- paste("Access to", theme)
    group_diff <- list(
      "Mode of transport" = stringr::str_to_sentence(mode),
      "Transportation time" = time
    )

    if (grepl("_transit_", var)) {
      timing <- (\(x) {
        if (grepl("_transit_opwe_", var)) return("Weekend traffic off-peak")
        if (grepl("_transit_pwe_", var)) return("Weekend traffic peak")
        if (grepl("_transit_nwd_", var)) return("Weekday night")
        if (grepl("_transit_nwe_", var)) return("Weekend night")
        if (grepl("_transit_opwd_", var)) return("Weekday traffic off-peak")
        if (grepl("_transit_pwd_", var)) return("Weekday traffic peak")
      })()
      group_diff <- c(group_diff, list("Timing" = timing))
    }

    # Additional group_diff
    val <- if (grepl("_total$", var)) "Total" else dict$title

    if (dict$theme == "retail") {
      group_diff <- c(group_diff, list("Retail stores type" = val))
    }
    if (dict$theme == "finance") {
      group_diff <- c(group_diff, list("Finance establishment" = val))
    }
    if (dict$theme == "food") {
      group_diff <- c(group_diff, list("Food industry" = val))
    }
    if (dict$theme == "healthcare") {
      group_diff <- c(group_diff, list("Health care facility" = val))
    }
    if (dict$theme == "educational") {
      group_diff <- c(group_diff, list("Educational establishment category" = val))
    }
    if (dict$theme == "cultural") {
      group_diff <- c(group_diff, list("Cultural facility" = val))
    }

    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = "avg",
      var_title = var_title,
      var_short = var_short,
      explanation = explanation,
      exp_q5 = exp_q5,
      group_name = group_name,
      group_diff = group_diff,
      parent_vec = "population",
      theme = "Transport",
      private = FALSE,
      pe_include = var %in% pe_include,
      region_values = region_values[[var]],
      dates = with_breaks$avail_dates[[var]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[[var]],
      breaks_q5 = with_breaks$q5_breaks_table[[var]],
      source = dict$source,
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense"),
      var_measurement = var_measurement
    ) |>
      (\(x) x[nrow(x), ])()
  })

  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "access",
      theme = "Transport",
      nav_title = "Access to amenities",
      title_text_title = "Access to amenities",
      title_text_main = paste0(
        "The time and mode of transportation needed to reach amenities plays a ",
        "large role in daily experiences and quality of life. Understanding acc",
        "ess to amenities by mode of transportation gives a glimpse into how di",
        "fferent areas are serviced and what that might imply for residents. "
      ),
      title_text_extra = paste0(
        "Curbcut has calculated travel times for walking, cycling, and driving ",
        "using the Open Source Routing Machine (OSRM) and the OpenStreetMap (OS",
        "M) street network. For transit travel times, Curbcut has employed GTFS",
        " feeds and a multimodal approach, incorporating walking times derived ",
        "from OSRM and the OSM street network. The amenities data has been sour",
        "ced from a combination of DMTI Spatial and OpenStreetMap."
      ),
      regions = data_interpolated$regions,
      metadata = TRUE,
      dataset_info = paste0(
        "<p>Curbcut has developed a comprehensive methodology for calculating t",
        "ravel times and determining accessibility to various amenities. Travel",
        " times for walking, cycling, and driving have been calculated using th",
        "e Open Source Routing Machine (OSRM) and the OpenStreetMap (OSM) stree",
        "t network. Transit travel times are calculated using GTFS feeds and a ",
        "multimodal approach that incorporates walking times from OSRM and the ",
        "OSM street network.<p>To analyze the accessibility to amenities, Curbc",
        "ut utilizes a two-step process. First, the travel time distances are c",
        "alculated using a custom function which computes the shortest travel t",
        "imes between Dissemination Area (DA) centroids and their closest neigh",
        "bors within specified distances. Next, the number of accessible amenit",
        "ies within each DA is determined by joining amenity points with the DA",
        " boundaries and counting the number of intersections.<p>The amenities ",
        "data is sourced from a combination of DMTI Spatial and OpenStreetMap, ",
        "ensuring an accurate representation of various types of amenities in t",
        "he area. This methodology allows for a detailed analysis of travel tim",
        "es and accessibility to amenities, providing valuable insights for urb",
        "an planning and development purposes."
      ),
      var_left = variables[grepl("^access_", variables$var_code),
                           c("var_code", "group_name", "group_diff")],
      main_dropdown_title = "Amenity",
      var_right = scales_variables_modules$variables$var_code[
        scales_variables_modules$variables$source == "Canadian census" &&
          !is.na(scales_variables_modules$variables$parent_vec)]
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = modules
  ))
}
