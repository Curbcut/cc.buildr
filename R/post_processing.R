#' Post process scales
#'
#' This function casts the geometry of scales to the right type, and reorders
#' the columns so that IDs and names are first.
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @return A list containing the post-processed scales, variables and modules
#' @export
post_processing <- function(scales_variables_modules) {
  # Cast to the right geometry type
  scales_variables_modules$scales <-
    map_over_scales(
      all_scales = scales_variables_modules$scales,
      fun = \(scale_df = scale_df, ...) {
        geos_types <- unique(sf::st_geometry_type(scale_df$geometry))

        if (length(geos_types) == 1) {
          return(scale_df)
        }
        if ("MULTIPOLYGON" %in% geos_types) {
          return(sf::st_cast(scale_df, "MULTIPOLYGON"))
        }
        if ("MULTILINESTRING" %in% geos_types) {
          return(sf::st_cast(scale_df, "MULTILINESTRING"))
        }
        if ("MULTIPOINT" %in% geos_types) {
          return(sf::st_cast(scale_df, "MULTIPOINT"))
        }
        return(scale_df)
      }
    )

  # Reorder columns so that IDs and names are first
  scales_variables_modules$scales <-
    reorder_columns(scales_variables_modules$scales)

  return(scales_variables_modules)
}
