#' Get details of the census vectors, used for data export
#'
#' @param census_vectors <`character vector`> Data variables that were added
#' to the scales. By default, all: \code{\link[cc.data]{census_vectors}}. Look
#' at the \code{\link[cc.data]{list_census_vectors}} to view all
#' variables explained.
#'
#' @return The wanted subset of \code{\link[cc.data]{census_vectors_details}}
#' @export
get_census_vectors_details <- function(census_vectors = cc.data::census_vectors) {
  details <- cc.data::census_vectors_details
  details[grepl(paste0(census_vectors, collapse = "|"), details$var_code), ]
}
