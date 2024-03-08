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

#' Save the Postal Codes Data
#'
#' Saves the postal codes data for a specified region into a quickstore (qs)
#' file. If the file already exists, it can optionally overwrite it.
#'
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param overwrite <`logical`> If TRUE, will overwrite the existing file if it exists.
#' Default is FALSE.
#' @param data_folder <`character`> The folder path where the postal codes file will be saved.
#' Default is "data/".
#'
#' @return Invisible NULL. The function is used for its side effect of saving a
#' file and does not return a value.
#' @export
save_postal_codes <- function(region_DA_IDs, overwrite = FALSE, data_folder = "data/") {

  path <- sprintf("%spostal_codes.qs", data_folder)

  if (overwrite | !file.exists(path)) {
    postal_codes <- build_postal_codes(region_DA_IDs)
    postal_codes <- sf::st_drop_geometry(postal_codes)
    qs::qsave(postal_codes, path)
  }

}
