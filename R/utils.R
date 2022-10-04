## COLLECTION OF UTILS FUNCTIONS ###############################################

#' Map over the depth of the list of scales
#'
#' As we follow a deep list of scales in geos, we can use this `map_over_scales()`
#' helper function to iterate functions on all the scales.
#'
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#' @param fun <`function`> A function to run on all scales.
#'
#' @return Returns the all_scales list that is fed, with the `fun` ran
#' on all the scales.
#' @export
map_over_scales <- function(all_scales, fun) {
    mapply(\(geo, scales) {
      mapply(\(scale_name, scale_df) {
        fun(geo = geo, scales = scales,
            scale_name = scale_name, scale_df = scale_df)
      }, names(scales), scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    }, names(all_scales), all_scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

#' Reconstruct all_tables
#'
#' As all the data arrange in a named list of geos > scales was created using
#' a list of character vector previously named `all_tables`, this function
#' allows to recreate the `all_tables` list of vectors using the data of
#' `all_scales`.
#'
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#'
#' @return A list of vectors of characters. The first level of the list is the
#' geo, and the second is all the scales available in that particular geo.
#' @export
reconstruct_all_tables <- function(all_scales) {
  mapply(names, all_scales, SIMPLIFY = FALSE)
}
