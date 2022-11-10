#' Build census scales dictionary
#'
#' @param census_scales <`list`> A named list containing census scales built
#' with the \code{\link[cc.buildr]{build_census_scales}} function.
#'
#' @return A data.frame used as dictionary for texts on the platform.
#' @export
census_scales_dictionary <- function(census_scales) {
  dict <-
    tibble::tibble(
      scale = c("CSD", "CT", "DA", "DB"),
      sing = c(
        "city",
        "census tract",
        "dissemination area",
        "dissemination block"
      ),
      plur = c(
        "cities",
        "census tracts",
        "dissemination areas",
        "dissemination blocks"
      ),
      slider_title = c(
        "City",
        "Census tract",
        "Dissemination area",
        "Dissemination block"
      ),
      place_heading = c(
        "{name_2} of {name}",
        "Census tract {name} ({name_2})",
        "Dissemination area {name} ({name_2})",
        "Dissemination block {name} ({name_2})"
      ),
      place_name = c(
        "{name}",
        "Census tract {name}",
        "Dissemination area {name}",
        "Dissemination block {name}"
      )
    )

  dict[dict$scale %in% names(census_scales), ]
}
