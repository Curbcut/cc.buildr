#' Create a New Spatial Grid with Reverse Geocoding and Append DA IDs
#'
#' This function takes an original spatial grid and transforms it into a new grid
#' with specified cell size. It performs reverse geocoding on each grid cell
#' and appends Dissemination Area (DA) IDs.
#'
#' @param original_grid <`sf object`> The original spatial grid in 'sf' format.
#' @param cellsize <`numeric`> The desired cell size for the new grid.
#' @param DA_table <`data.frame`> Data frame containing Dissemination Area IDs.
#' @param crs <`integer`> Coordinate Reference System code for spatial transformation.
#'
#' @return An 'sf' object representing the new grid with additional columns for
#' grid ID, place names, and DA IDs.
#' @export
new_grid <- function(original_grid, cellsize, DA_table, crs) {
  # Make the grid
  new_grid_geometry <- sf::st_make_grid(sf::st_transform(original_grid, 2950),
                                        cellsize = c(cellsize, cellsize),
                                        crs = 2950)

  # Create the new grid as sf
  new_grid <- tibble::tibble(ID = seq_along(new_grid_geometry))
  new_grid <- tibble::as_tibble(cbind(new_grid, new_grid_geometry))
  new_grid <- sf::st_as_sf(new_grid)

  centroid <- sf::st_point_on_surface(sf::st_transform(original_grid, 2950))
  index <- sf::st_intersects(centroid, new_grid)
  centroid$new_grid <- unlist(index)
  grouped <- cbind(original_grid, sf::st_drop_geometry(centroid["new_grid"]))

  new_grid <- aggregate(grouped["new_grid"], by = list(grouped$new_grid),
                        FUN = function(x) length(unique(x)))
  new_grid <- sf::st_transform(new_grid, crs = crs)
  new_grid <- new_grid["geometry"]
  new_grid$ID <- sprintf("grd%s_%s", cellsize, seq_along(new_grid$geometry))
  grd_geocode <- sf::st_transform(new_grid, 4326)
  grd_geocode <- sf::st_centroid(grd_geocode)

  # Is the local nominatim running?
  if ((length(shell(paste0("docker ps -aq -f name=^nominatim-canada$"),
                    intern = TRUE)) > 0)) {
    shell(paste0("docker start nominatim-canada"))
  } else {
    stop(paste0("There is no local version of nominatim-canada, necessary for ",
                "geocoding. Run `cc.data::rev_geocode_create_local_nominatim`."))
  }

  # Reverse geolocate every grid cell
  progressr::with_progress({
    pb <- progressr::progressor(nrow(grd_geocode))
    new_grid$name <- future.apply::future_sapply(grd_geocode$geometry, \(x) {
      pb()
      cc.data::rev_geocode_localhost(point_sf = x)
    }, future.seed = NULL)
  })
  new_grid <- new_grid[, c("ID", "name", "geometry")]

  # Append DA IDs
  new_grid <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
                                      df = new_grid,
                                      crs = crs)

  # Return
  return(new_grid)
}
