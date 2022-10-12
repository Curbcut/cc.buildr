#' Add a ready to use Can-ALE data and module
#'
#' @param scales_variables_modules <`names list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the Can-ALE variable added, its addition
#' in the variables table and the module table.
#' @export
ready_to_use_canale <- function(scales_variables_modules, crs) {
  build_and_append_var(
    data = susbuildr::canale_data,
    scales_variables_modules = scales_variables_modules,
    base_scale = "DA",
    weight_by = "households",
    crs = crs,
    variable_var_code = "canale",
    variable_type = "ind",
    variable_var_title = "Can-ALE index",
    variable_var_short = "Can-ALE",
    variable_explanation = "the potential for active living",
    variable_theme = "Urban life",
    variable_private = FALSE,
    variable_source = "McGill Geo-Social Determinants of Health Research Group",
    module_id = "canale",
    module_title = "Active living potential",
    module_metadata = TRUE,
    module_dataset_info =
      paste0("<p><a href = 'https://nancyrossresearchgroup.ca/research/can-ale/'>",
             "The Canadian Active Living Environments (Can-ALE)</a> dataset is ",
             "a geographic-based set of measures charac",
             "terizing the active living environments (often referred to as '",
             "walkability') of Canadian communities. The data is provided at ",
             "the dissemination area level.</p>"))
}

#' Add a ready to use Can-BICS data and module
#'
#' @param scales_variables_modules <`names list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the Can-BICS variable added, its addition
#' in the variables table and the module table.
#' @export
ready_to_use_canbics <- function(scales_variables_modules, crs) {
  build_and_append_var(
    data = susbuildr::canbics_data,
    scales_variables_modules = scales_variables_modules,
    base_scale = "DA",
    weight_by = "households",
    crs = crs,
    variable_var_code = "canbics",
    variable_type = "ind",
    variable_var_title = "Can-BICS metric",
    variable_var_short = "Can-BICS",
    variable_explanation = "the bikeway comfort and safety classification system",
    variable_theme = "Transport",
    variable_private = FALSE,
    variable_source = "Meghan Winters at Faculty of Health Sciences, Simon Fraser Universitya",
    module_id = "canbics",
    module_title = "Bikeway comfort and safety",
    module_metadata = TRUE,
    module_dataset_info =
      paste0("<p><a href = 'https://www.canada.ca/en/public-health/services/reports",
             "-publications/health-promotion-chronic-disease-prevention-canada-",
             "research-policy-practice/vol-40-no-9-2020/canbics-classification-",
             "system-naming-convention-cycling-infrastructure.html'>",
             "The Canadian Bikeway Comfort and Safety (Can-BICS) Classification System</a> dataset is ",
             "a geographic-based set of measures charac",
             "terizing the cycling infrastructure of Canadian communities. ",
             "The data is provided at ",
             "the dissemination area level.</p>"))
}

#' Add a ready to use Vacancy Rate data and module
#'
#' @param scales_variables_modules <`names list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param fun_rename_zones <`function`> Optional  function that takes in a vector of character
#' and substitutes some strings. CMHC zones' naming might be inconstant over
#' different year, e.g. `Plateau Mont-Royal` instead of `Plateau-Mont-Royal`.
#' e.g. \code{function(zones) gsub("Plateau Mont-Royal", "Plateau-Mont-Royal", zones)}.
#' @param geo_uid <`numeric`> Cancensus CMA code, which can be found using
#' \code{\link[cancensus]{list_census_regions}}.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the CMHC's vacancy rate variables added,
#' their addition in the variables table and the module table.
#' @export
ready_to_use_vac_rate <- function(scales_variables_modules, crs,
                                 fun_rename_zones = NULL, geo_uid) {

  # Relevant dimensions
  dimensions <-
    c("Bedroom Type", "Year of Construction")#, "Rent Ranges")
  dimensions_short <-
    c("bed", "year")#, "rent_range")

  # Retrieval
  cmhc <-
    sapply(2010:2021, \(yr) {
      over_year <-
        mapply(\(x, y) {
          # Get data
          out <- cmhc::get_cmhc(survey = "Rms",
                                series = "Vacancy Rate",
                                dimension = x,
                                breakdown = "Survey Zones",
                                geo_uid = geo_uid,
                                year = yr)[, 1:3]
          # Rename column and update for real percentage
          names(out)[2] <- y
          out[3] <- out[3] / 100

          # Pivot and rename
          out <- tidyr::pivot_wider(out,
                                    names_from = tidyr::all_of(y),
                                    values_from = "Value")
          names(out) <- gsub(" |-", "_", tolower(names(out)))
          names(out) <- gsub("___", "_", names(out))
          names(out) <- gsub("\\+", "plus", names(out))
          names(out) <- gsub("_units", "", names(out))
          names(out) <- gsub("bedroom", "bed", names(out))
          names(out) <- paste("vac_rate", y, names(out), yr, sep = "_")
          names(out)[1] <- "name"

          # Rename to fit geography
          if (!is.null(fun_rename_zones)) out$name <- fun_rename_zones(out$name)

          # Return
          out
        }, dimensions, dimensions_short, SIMPLIFY = FALSE, USE.NAMES = TRUE)
      cmhc <- Reduce(merge, over_year)
    }, simplify = FALSE, USE.NAMES = TRUE)

  merged <-
    Reduce(\(x, y) merge(x, y, by = "name", all.x = TRUE),
           cmhc, init =
             scales_variables_modules$scales$cmhc$cmhc_zone[, "name"])
  merged <- sf::st_drop_geometry(merged)

  # Variables
  vars <- names(merged)[!grepl("name|ID|geometry", names(merged))]
  unique_vars <- unique(gsub("_\\d{4}$", "", vars))

  # Append data
  scales_variables_modules$scales$cmhc$cmhc_zone <-
    susbuildr::merge(scales_variables_modules$scales$cmhc$cmhc_zone,
                     merged, by = "name")

  # Calculate breaks
  with_breaks <-
    calculate_breaks(
      all_scales = scales_variables_modules$scales,
      vars = vars,
      time_regex = "\\d{4}")

  # Add to the variables table
  variables <-
    lapply(unique_vars, \(var) {

      cat_title <- (\(x) {
        # Bedroom types
        if (grepl("_bed_", var)) {
          if (grepl("bachelor$", var)) return("studio apartments")
          suff <- "units"
          if (grepl("1_bed$", var)) return(paste("one-bedroom", suff))
          if (grepl("2_bed$", var)) return(paste("two-bedroom", suff))
          if (grepl("3_bed_plus$", var)) return(paste("three-bedroom and larger", suff))
          if (grepl("bed_total$", var)) return(paste("all", suff))
        }
        # Year of construction
        if (grepl("_year_", var)) {
          pre <- "housing built"
          if (grepl("before_1960$", var)) return(paste(pre, "before 1960"))
          if (grepl("1960_1979$", var)) return(paste(pre, "between 1960 and 1979"))
          if (grepl("1980_1999$", var)) return(paste(pre, "between 1980 and 1999"))
          if (grepl("2000_or_later$", var)) return(paste(pre, "after 2000"))
          if (grepl("year_total$", var)) return(paste("all housing"))
        }
      })(var)
      title <- paste("Vacancy rate in", cat_title)
      explanation <- paste("the percentage of all available",
                           gsub("^all ", "", cat_title),
                           "in a rental property that are vacant or unoccupied")

      cat_short <- (\(x) {
        # Bedroom types
        if (grepl("_bed_", var)) {
          if (grepl("bachelor$", var)) return("studio")
          if (grepl("1_bed$", var)) return("1bed")
          if (grepl("2_bed$", var)) return("2bed")
          if (grepl("3_bed_plus$", var)) return("3+bed")
          if (grepl("bed_total$", var)) return("total")
        }
        # Year of construction
        if (grepl("_year_", var)) {
          pre <- "housing built"
          if (grepl("before_1960$", var)) return("<1960")
          if (grepl("1960_1979$", var)) return(">1960<1979")
          if (grepl("1980_1999$", var)) return(">1980<1999")
          if (grepl("2000_or_later$", var)) return(">2000")
          if (grepl("year_total$", var)) return("total")
        }
      })(var)

      short <- paste("Vac. rate", cat_short)

      out <-
        susbuildr::add_variable(
          variables = scales_variables_modules$variables,
          var_code = var,
          type = "pct",
          var_title = title,
          var_short = short,
          explanation = explanation,
          theme = "Housing",
          private = FALSE,
          dates = with_breaks$avail_dates[[var]],
          scales = tibble::tibble(geo = "cmhc", scale = "zone"),
          breaks_q3 = with_breaks$q3_breaks_table[[var]],
          breaks_q5 = with_breaks$q5_breaks_table[[var]],
          source = "Canada Mortgage and Housing Corporation",
          interpolated = tibble::tibble(geo = "cmhc", scale = "zone",
                                        interpolated_from = FALSE),
          group_name = "Vacancy rate",
          group_diff = list())

      out[out$var_code == var, ]
    }) |> (\(x) Reduce(rbind, x, init = scales_variables_modules$variables))()


  # Create a module
  modules <-
    scales_variables_modules$modules |>
    susbuildr::add_module(
      id = "vac_rate",
      title = "Vacancy rate",
      geos = "cmhc",
      metadata = TRUE,
      dataset_info = "TKTK")


  # Return ------------------------------------------------------------------

  return(list(scales = with_breaks$scales,
              variables = variables,
              modules = modules))

}
