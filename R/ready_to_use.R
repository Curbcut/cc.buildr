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




# read_to_use_vac_rate <- function(scales_variables_modules,
#                                  pretty_spatial_intersect, crs,
#                                  fun_rename_zones) {
#
#   # Transform and prepare spatial geographies
#   zones <- cmhc::get_cmhc_geography(level = "ZONE")
#   zones <- sf::st_transform(zones, crs)
#   zones <- sf::st_make_valid(zones)
#   master <- sf::st_transform(pretty_spatial_intersect, crs)
#   if (length(master) > 1) master <- sf::st_union(master)
#   master <- sf::st_make_valid(master)
#
#   # Filter in using the master, and cut it with its boundaries
#   zones <- sf::st_intersection(zones, master)[, "ZONE_NAME_EN"]
#   zones <- sf::st_cast(zones, "MULTIPOLYGON")
#   names(zones)[1] <- "zone"
#
#   # Relevant dimensions
#   dimensions <-
#     c("Bedroom Type", "Year of Construction", "Structure Size", "Rent Ranges" )
#   dimensions_short <-
#     c("bed", "year", "size", "rent_range")
#
#   # Retrieval
#   cmhc <-
#     sapply(2010:2021, \(yr) {
#       over_year <-
#         mapply(\(x, y) {
#           # Get data
#           out <- cmhc::get_cmhc(survey = "Rms",
#                                 series = "Vacancy Rate",
#                                 dimension = x,
#                                 breakdown = "Survey Zones",
#                                 geo_uid = "24462",
#                                 year = yr)[, 1:3]
#           # Rename column and update for real percentage
#           names(out)[2] <- y
#           out[3] <- out[3] / 100
#
#           # Pivot and rename
#           out <- tidyr::pivot_wider(out, names_from = y, values_from = Value)
#           names(out) <- gsub(" |-", "_", tolower(names(out)))
#           names(out) <- gsub("___", "_", names(out))
#           names(out) <- gsub("\\+", "plus", names(out))
#           names(out) <- gsub("_units", "", names(out))
#           names(out) <- gsub("bedroom", "bed", names(out))
#           names(out) <- paste("vac_rate", y, names(out), yr, sep = "_")
#           names(out)[1] <- "zone"
#
#           # Rename to fit geography
#
#           out$zone <- gsub("Plateau Mont-Royal", "Plateau-Mont-Royal",
#                            out$zone)
#           out$zone <-
#             gsub(paste0("Saint-Lin-Laurentides V|Saint-Lin\u0096Laurentides V|Sai",
#                         "nt-Lin–Laurentides V"), "Saint-Lin–Laurentides V",
#                  out$zone)
#
#           # Return
#           out
#         }, dimensions, dimensions_short, SIMPLIFY = FALSE, USE.NAMES = TRUE)
#       cmhc <- Reduce(merge, over_year)
#     }, simplify = FALSE, USE.NAMES = TRUE)
#
#   merged <- Reduce(\(x, y) merge(x, y, by = "zone", all = TRUE),
#                    cmhc, init = zones)
#
#   merged <- merged[!sf::st_is_empty(merged$geometry), ]
#
#
#
#
#
# }
