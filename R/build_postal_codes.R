#' Retrieve the postal codes in the region
#'
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#'
#' @return Returns all the postal codes in the region along with their centroid.
#' @export
build_postal_codes <- function(region_DA_IDs) {
  cc.data::db_read_data(
    table = "postal_codes", column_to_select = "DA_ID",
    IDs = region_DA_IDs, crs = 4326
  )
}
