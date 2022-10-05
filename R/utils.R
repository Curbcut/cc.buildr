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

#' Compute geometric measurements and drop units
#'
#' @param geometry <`sf`, `sfc` or `sfg`> A column of geometries.
#' @param ... passed on to \code{\link[sf]{st_area}}
#'
#' @return A numeric vector of shape areas.
#' @export
get_area <- function(geometry, ...) {
  x <- sf::st_area(geometry)
  as.vector(x)
}

#' Reorder columns as they are expected in the workflow
#'
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#'
#' @return Returns the same list that is fed, with columns re-ordered.
#' @export
reorder_columns <- function(all_scales) {
  susbuildr::map_over_scales(
    all_scales = all_scales,
    fun = \(geo = geo, scales = scales,
            scale_name = scale_name, scale_df = scale_df) {

      scales_order <- names(scales)
      needed_present_scales <-
        scales_order[seq_len(which(scales_order == scale_name))]

      mandatory_start <- c("ID", "name", "name_2")
      all_ids <- paste0(needed_present_scales, "_ID")
      other <- character()

      if ("population" %in% names(scale_df)) other <- c(other, "population")
      if ("households" %in% names(scale_df)) other <- c(other, "households")

      rest <- names(scale_df)[!names(scale_df) %in% c(mandatory_start, all_ids,
                                                      other)]
      rest <- rest[rest != "geometry"]

      out <- scale_df[, c(mandatory_start, all_ids, other, rest, "geometry")]

      if (ncol(out) != ncol(scale_df))
        stop(
          paste0("Some columns have been dropped along the way in the use of ",
                 "x function."))

      out

    })
}
# TKTK Add a re-order columns function! -----------------------------------


