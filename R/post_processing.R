#' Post process scales
#'
#' This function casts the geometry of scales to the right type, and reorders
#' the columns so that IDs and names are first.
#'
#' @param scales <`named list`> A list of all the scales ordered in regions.
#' Usually `scales_variables_modules$scales`
#'
#' @return A list containing the post-processed scales, variables and modules
#' @export
post_processing <- function(scales) {
  # Cast to the right geometry type
  scales <- lapply(scales, \(scale_df) {
    df <- scale_df#[!sf::st_is_empty(scale_df), ]
    # if (nrow(scale_df) != nrow(df)) {
    #   taken_out <- nrow(scale_df) - nrow(df)
    #   warning(paste0(
    #     "`", taken_out, "` observations have been filtered out ",
    #     "from `", scale_name, "` due to spatial features being empty."
    #   ))
    # }

    geos_types <- unique(sf::st_geometry_type(df$geometry))
    if (length(geos_types) == 1) {
      return(df)
    }
    if ("MULTIPOLYGON" %in% geos_types) {
      return(sf::st_cast(df, "MULTIPOLYGON"))
    }
    if ("MULTILINESTRING" %in% geos_types) {
      return(sf::st_cast(df, "MULTILINESTRING"))
    }
    if ("MULTIPOINT" %in% geos_types) {
      return(sf::st_cast(df, "MULTIPOINT"))
    }
    return(df)
  })

  # Order the rows using the ID (recurring assumption in Curbcut)
  scales <- lapply(scales, \(scale_df) {
      scale_df[order(scale_df$ID), ]
  })

  # Reorder the columns
  scales <- lapply(scales, reorder_columns)

  return(scales)
}
