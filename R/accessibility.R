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
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param pe_include <`character vector`> Which final variables should appear
#' in the place explorer. Defaults to `c("access_foot_20_educational_elementary", "access_foot_20_communitycentres_individual", "access_foot_15_fooddistribution_grocery", "access_car_10_healthcare_hospitals")`.
#' @param default_var <`character`> The first variable the user will see when
#' they will lang on the page. Defaults to groceries accessible within a 20 minutes walk.
#'
#' @return A list containing the scales, variables, and modules tables.
#' @export
ba_accessibility_points <- function(scales_variables_modules,
                                    region_DA_IDs,
                                    themes = cc.data::list_accessibility_themes(),
                                    traveltimes,
                                    time_intervals = which(1:60 %% 5 == 0),
                                    pe_include = c(
                                      "access_foot_educational_elementary",
                                      "access_bicycle_educational_secondary",
                                      "access_foot_cultural_total",
                                      "access_foot_food_grocery",
                                      "access_car_healthcare_hospitals"
                                    ),
                                    default_var = "access_foot_food_grocery",
                                    scales_sequences,
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

  names(ttm_data)[2:ncol(ttm_data)] <- paste0(names(ttm_data)[2:ncol(ttm_data)], "_2023")

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


  # Data tibble -------------------------------------------------------------

  time_regex <- "_\\d{4}$"
  unique_var <- gsub(time_regex, "", average_vars)
  unique_var <- gsub("_\\d{1,2}$", "", unique_var)
  unique_var <- unique(unique_var)

  # Construct the breaks_var list (take the breaks from a 20 minutes traject)
  breaks_var <- lapply(unique_var, paste0, "_20_2023")
  names(breaks_var) <- unique_var

  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = data_interpolated$scales,
                         unique_var = unique_var,
                         time_regex = time_regex,
                         schema = list(time = gsub("^_", "", time_regex),
                                       transportationtime = "(?<=_)\\d{1,2}(?=_)"),
                         breaks_var = breaks_var)


  # Types and parent vectors ------------------------------------------------

  parent_strings <- rep(list("population"), length(unique_var))
  names(parent_strings) <- unique_var

  types <- rep(list("avg"), length(unique_var))
  names(types) <- unique_var


  # Variable measurements ----------------------------------------------------

  var_measurement <- data.frame(
    scale = data_interpolated$avail_scale,
    measurement = rep("scalar", length(data_interpolated$avail_scale))
  )

  var_measurement$measurement[var_measurement$scale == "DA"] <- "ordinal"


  # Variables table ---------------------------------------------------------

  progressr::with_progress({
    pb <- progressr::progressor(length(unique_var))

    new_variables <- future.apply::future_lapply(unique_var, \(var) {
      dict <- cc.data::accessibility_point_dict
      dict <- dict[sapply(dict$var, grepl, var), ]

      mode <- (\(x) {
        if (grepl("_car_", var)) {
          return("car")
        }
        if (grepl("_foot_", var)) {
          return("walking")
        }
        if (grepl("_bicycle_", var)) {
          return("bicycle")
        }
        if (grepl("_transit_opwe_", var)) {
          return("public transit on off-peak weekend days")
        }
        if (grepl("_transit_pwe_", var)) {
          return("public transit on peak weekend days")
        }
        if (grepl("_transit_nwd_", var)) {
          return("public transit on weekdays at night")
        }
        if (grepl("_transit_nwe_", var)) {
          return("public transit on weekends at night")
        }
        if (grepl("_transit_opwd_", var)) {
          return("public transit on off-peak weekdays")
        }
        if (grepl("_transit_pwd_", var)) {
          return("public transit on peak weekdays")
        }
      })()

      time <- "__schema$transportationtime__"

      var_title <- stringr::str_to_sentence(paste0(dict$title, " accessible by ", mode))
      var_short <- stringr::str_to_sentence(dict$short)

      mode_text <- (\(x) {
        if (mode == "car") {
          return("drive")
        }
        if (mode == "walking") {
          return("walk")
        }
        if (mode == "bicycle") {
          return("bike ride")
        }
        if (grepl("public transit", mode)) {
          return(gsub("public transit", "transit journey", mode))
        }
      })()
      explanation <- paste0(
        "the number of ", tolower(dict$title),
        " a resident can reach within a ", time, "-minute ", mode_text
      )
      exp_q5 <- paste0(
        "a resident has access to, on average,  _X_ ", tolower(dict$title), " within a ",
        time, "-minute ", mode_text
      )

      # Cut timing out of the mode
      mode <- stringr::str_extract(mode, "(^car$)|(^walking$)|(^bicycle$)|(^public transit)")

      theme <- (\(x) {
        if (dict$theme == "retail") {
          return("retail stores")
        }
        if (dict$theme == "finance") {
          return("finance establishments")
        }
        if (dict$theme == "food") {
          return("food distributors")
        }
        if (dict$theme == "healthcare") {
          return("healthcare facilities")
        }
        if (dict$theme == "educational") {
          return("schools")
        }
        if (dict$theme == "cultural") {
          return("cultural facilities")
        }
        if (dict$theme == "recreation") {
          return("recreational services")
        }
      })()
      group_name <- paste("Access to", theme)
      group_diff <- list(
        "Mode of transport" = stringr::str_to_sentence(mode),
        "Transportation time" = time_intervals
      )

      if (grepl("_transit_", var)) {
        timing <- (\(x) {
          if (grepl("_transit_opwe_", var)) {
            return("Weekend traffic off-peak")
          }
          if (grepl("_transit_pwe_", var)) {
            return("Weekend traffic peak")
          }
          if (grepl("_transit_nwd_", var)) {
            return("Weekday night")
          }
          if (grepl("_transit_nwe_", var)) {
            return("Weekend night")
          }
          if (grepl("_transit_opwd_", var)) {
            return("Weekday traffic off-peak")
          }
          if (grepl("_transit_pwd_", var)) {
            return("Weekday traffic peak")
          }
        })()
        group_diff <- c(group_diff, list("Timing" = timing))
      }

      # Additional group_diff
      val <- if (grepl("_total$", var)) "All" else stringr::str_to_sentence(dict$title)

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
      if (dict$theme == "recreation") {
        group_diff <- c(group_diff, list("Recreation service" = val))
      }

      pb()
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
        dates = "2023",
        avail_scale = data_interpolated$avail_scale,
        source = dict$source,
        interpolated = data_interpolated$interpolated_ref,
        rankings_chr = c(
          "exceptionally sparse", "unusually sparse",
          "just about average", "unusually dense",
          "exceptionally dense"
        ),
        var_measurement = var_measurement
      ) |>
        (\(x) x[nrow(x), ])()
    })
  })

  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))

  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)

  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "access",
      theme = "Transport",
      nav_title = "Access to amenities",
      title_text_title = "Access to amenities",
      title_text_main = paste0(
        "<p>Neighbourhood accessibility is an important contributor to quality ",
        "of life. It can be measured by how counting many amenities can be ",
        "reached from a location for a given time and mode of transportation."
      ),
      title_text_extra = paste0(
        "<p>Curbcut has calculated travel times for walking, cycling, and driving ",
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
      var_left = variables[
        grepl("^access_", variables$var_code),
        c("var_code", "group_name", "group_diff")
      ],
      main_dropdown_title = "Amenity",
      dates = "2023",
      var_right = scales_variables_modules$variables$var_code[
        scales_variables_modules$variables$source == "Canadian census" &
          !is.na(scales_variables_modules$variables$parent_vec)
      ],
      default_var = default_var,
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = data_interpolated$scales,
    variables = variables,
    modules = modules,
    data = data
  ))
}
