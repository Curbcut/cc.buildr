#' Append the population-weighted centroids to a DA table
#'
#' @param DA_table <`data.frame`> A DA data.frame for which to merge IDs with the
#' \code{susbuilder::da_centroids_popw} data.
#' @param name <`character`> The name of the new column. Defaults to
#' \code{"popw_centroids_coords"}.
#'
#' @return The same table that was populated comes out with a new list column
#' containing the population-weighted centroids for each DA.
#' @export
append_DA_popw_centroids <- function(DA_table, name = "popw_centroids_coords") {

  centroids <- susbuilder::da_centroids_popw
  centroids[[name]] <- lapply(centroids$geometry, sf::st_coordinates)
  centroids <- sf::st_drop_geometry(centroids)

  merge(DA_table, centroids, by = "ID")

}
