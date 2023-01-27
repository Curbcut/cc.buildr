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
#'
#' @return A list containing the scales, variables, and modules tables.
#' @export
ba_accessibility_points <- function(scales_variables_modules,
                                    region_DA_IDs,
                                    themes = cc.data::list_accessibility_themes(),
                                    traveltimes,
                                    time_intervals = which(1:60 %% 5 == 0),
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
  dict$var_code <- gsub("_\\d{4}$", "", dict$var_code)
  vars <- dict$var_code[dict$theme %in% themes]
  # Add year
  vars <- paste0(vars, "_2021")

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
    traveltimes = traveltimes,
    time_intervals = time_intervals
  )
  # ttm_data <- qs::qread("test_build_mtl/ttm_data.qs")


  # Interpolate -------------------------------------------------------------

  average_vars <- names(ttm_data)[!grepl("ID", names(ttm_data))]
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

  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = average_vars
    )


  # Variables table ---------------------------------------------------------

  vars <- unique(gsub("_\\d{4}$", "", average_vars))

  new_variables <- lapply(vars, \(var) {
    #### TKTK EXTERIOR SWIMMING POOLS (replace arenas)

    d_entry <- dict[sapply(dict$var_code, grepl, var, USE.NAMES = FALSE), ]
    d_entry$industry <- gsub(" (Seafood)", "", d_entry$industry)
    d_entry$industry <- gsub("Fire Protection", "Fire Stations", d_entry$industry)
    d_entry$industry <- gsub("Police Protection", "Police Stations", d_entry$industry)
    d_entry$industry <- gsub("Religious Organizations", "Religious Establishments", d_entry$industry)
    d_entry$industry <- gsub("Public Wifi Hotspot", "Public Wifi Hotspots", d_entry$industry)
    d_entry$industry <- gsub("Retail Establishment", "Retail Establishments", d_entry$industry)

    theme <-
      if (d_entry$theme == "arenas") "arenas" else if (d_entry$theme == "cinemas") "cinemas" else if (d_entry$theme == "communitycentres") "community centres" else if (d_entry$theme == "education") "educational facilities" else if (d_entry$theme == "firestations") "fire stations" else if (d_entry$theme == "fooddistribution") "food distributors" else if (d_entry$theme == "policeservices") "police services" else if (d_entry$theme == "religiousbuildings") "religious buildings" else if (d_entry$theme == "retail") "retail establishments" else if (d_entry$theme == "wifihotspots") "wifi hotspots" else if (d_entry$theme == "healthcare") "healthcare services"

    gsub("_", "", dict$var_code |> stringr::str_extract("_.*$"))

    subtheme <- d_entry$industry

    mode <-
      if (grepl("_car_", var)) "car" else if (grepl("_foot_", var)) "walking" else if (grepl("_bicycle_", var)) "bicycle" else if (grepl("_transit_", var)) "public transit"

    time <- gsub("_", "", stringr::str_extract(var, "_\\d*_"))

    subtheme <-
      if (grepl("amusement", var)) "Arenas" else if (grepl("motion", var)) "Cinemas" else if (grepl("individual", var)) "Community" else if (grepl("elementary", var)) "Schools" else if (grepl("colleges", var)) "Universities" else if (grepl("education_other", var)) "Other schools" else if (grepl("fire", var)) "Fire stations" else if (grepl("retail", var)) "Retail" else if (grepl("grocery", var)) "Groceries" else if (grepl("fruit", var)) "Fruits & Veg." else if (grepl("meat", var)) "Meat & Fish" else if (grepl("miscellaneous", var)) "Misc. food" else if (grepl("dairy", var)) "Dairy" else if (grepl("police", var)) "Police" else if (grepl("religious", var)) "Religious" else if (grepl("department", var)) "Department" else if (grepl("hardware", var)) "Hardware" else if (grepl("public", var)) "Wi-Fi" else if (grepl("doctors", var)) "Doctors" else if (grepl("nursing", var)) "Nursing" else if (grepl("hospitals", var)) "Hospitals" else if (grepl("healthcare_other", var)) "Other" else if (grepl("education_total", var)) "Education" else if (grepl("fooddistribution_total", var)) "Food" else if (grepl("healthcare_total", var)) "Healthcare" else if (grepl("retail_total", var)) "Retail"

    var_title <- stringr::str_to_sentence(paste0(d_entry$industry, " accessible by ", mode))
    var_short <- stringr::str_to_sentence(subtheme)
    explanation <- paste0(
      "the average count of ", tolower(d_entry$industry),
      " accessible in ", time, " minutes by ", mode
    )


    group_name <- paste("Access to", theme)
    group_diff <- list(
      "Mode of transport" = stringr::str_to_sentence(mode),
      "Transportation time" = time
    )

    # Additional group_diff
    val <-
      if (grepl("_total$", d_entry$var_code)) "Total" else unname(d_entry$industry)

    if (d_entry$theme == "fooddistribution") {
      group_diff <- c(group_diff, list("Industry" = val))
    }
    if (d_entry$theme == "education") {
      group_diff <- c(group_diff, list("Educational establishment category" = val))
    }
    if (d_entry$theme == "retail") {
      group_diff <- c(group_diff, list("Retail category" = val))
    }
    if (d_entry$theme == "healthcare") {
      group_diff <- c(group_diff, list("Health care facility" = val))
    }

    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = "avg",
      var_title = var_title,
      var_short = var_short,
      explanation = explanation,
      group_name = group_name,
      group_diff = group_diff,
      theme = "Transport",
      private = FALSE,
      dates = with_breaks$avail_dates[[var]],
      scales = data_interpolated$avail_scales,
      breaks_q3 = with_breaks$q3_breaks_table[[var]],
      breaks_q5 = with_breaks$q5_breaks_table[[var]],
      source = d_entry$source,
      interpolated = data_interpolated$interpolated_ref
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
      title_text_main =
        paste0(
          "Being able to access amenities and services in our nearby ",
          "urban environment can greatly impact our daily ",
          "experiences and quality of life. The time and mode of ",
          "transportation needed to reach these amenities plays a ",
          "large role in this. In this module, explore information ",
          "about access to schools, food distributors, health care ",
          "facilities, and more by walk, bike, transit, or car."
        ),
      title_text_extra =
        paste0(
          "In selecting different options from the drop-down menus, ",
          "insights can be gained about access to different types of ",
          "amenities by a certain mode of transportation within a ",
          "given amount of time. Using the panel on the right, you ",
          "can compare these options to socio-demographic variables. ",
          "Understanding access to amenities by mode of ",
          "transportation gives a glimpse into how different areas ",
          "are serviced and what that might imply for residents."
        ),
      regions = unique(data_interpolated$avail_scales$geo),
      metadata = TRUE,
      dataset_info =
        paste0(
          "The travel time matrices in this module were calculated ",
          "using a combination of methods, with the underlying data ",
          "coming from an Open Street Map (OSM) network file and ",
          "General Transit Feed Specification (GTFS) files."
        )
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = modules
  ))
}
