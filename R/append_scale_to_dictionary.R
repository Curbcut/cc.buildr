#' Add a scale to the scales dictionary
#'
#' @param scales_dictionary <`data.frame`> An already built scales dictionary
#' data.frame, most probably built using \code{\link[cc.buildr]{census_scales_dictionary}}.
#' @param scale <`character`> The scale code name, e.g. \code{"CSD"}.
#' @param sing <`character`> The scale name in singular, e.g. \code{"city"}.
#' @param plur <`character`> The scale name in plurial, e.g. \code{"cities"}.
#' @param slider_title <`character`> The scale name used on the map slider,
#' e.g. \code{"City"}.
#' @param place_heading <`character`> The place heading on a selection, e.g.
#' for "City of Laval" -> \code{"City of {name}"}.
#' @param place_name <`character`> The place name on a selection as the first
#' string of the dynamically generated text in the explore panel. e.g. for
#' "Laval has a population of ..." -> \code{"{name}"}.
#'
#' @return The \code{scale_dictionary} data.frame that is fed with the
#' additional row.
#' @export
append_scale_to_dictionary <- function(scales_dictionary, scale, sing, plur,
                                       slider_title, place_heading,
                                       place_name) {
  # Add a new row
  scales_dictionary[nrow(scales_dictionary) + 1, ] <-
    list(scale, sing, plur, slider_title, place_heading, place_name)

  scales_dictionary
}
