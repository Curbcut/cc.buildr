#' Verify the regions and the scales dictionary
#'
#' The use of `verify_dictionaries()` is to verify any missing entries in both
#' dictionaries.
#'
#' @param all_tables <`named list`> The name of the named list \code{all_tables}
#' is used to retrieve the shapefiles in the supplied folder.
#' @param regions_dictionary <`data.frame`> The regions dictionary built using
#' \code{\link[cc.buildr]{regions_dictionary}}
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link[cc.buildr]{build_census_scales}}
#'
#' @return Returns nothing if the test passes, or errors if it doesn't.
#' @export
verify_dictionaries <- function(all_tables, regions_dictionary,
                                scales_dictionary) {
  # Verify regions first -------------------------------------------------------

  regions <- names(all_tables)
  z <- regions[!regions %in% regions_dictionary$region]
  if (length(z) > 0) {
    stop(paste0("Missing `", regions, "` in the `regions_dictionary`"))
  }

  # Verify scales second ----------------------------------------------------

  scales <- unique(unlist(all_tables))
  z <- scales[!scales %in% scales_dictionary$scale]
  if (length(z) > 0) {
    stop(paste0("Missing `", z, "` in the `scales_dictionary`"))
  }
  if (sum(grepl("_", scales_dictionary$scale)) > 0) {
    cont <- scales_dictionary$scale[which(grepl("_", scales_dictionary$scale))]
    stop(sprintf("`%s` contains an underscore. Remove the underscore.", cont))
  }
}
