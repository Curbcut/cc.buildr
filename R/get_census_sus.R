#' Retrieve census data and filter it by the master polygon
#'
#' @param master_polygon <`sfc_MULTIPOLYGON`>. Unioned multipolygon of all the
#' geometries for which census data must be gathered.
#' @param census_dataset <`character`> The dataset to query for available
#' regions, e.g. \code{"CA16"}.
#' @param regions <`named list`> A named list of census regions to retrieve.
#' Names must be valid census aggregation levels.
#' @param level <`character`> The census aggregation level to retrieve. One of \code{"CMA"},
#' \code{"CD"}, \code{"CT"}, \code{"DA"}, \code{"DB"}, ...
#' @param var_select <`named character vector`> Variables to be selected from the
#' \code{\link[cancensus]{get_census}} cancensus function call, and renamed. Default
#' to \code{c("CTUID" = "CT_UID", "CSDUID" = "CSD_UID", "name" = "name",
#' "population" = "Population", "households" = "Households")}.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param format <`logical`> Indicating if the census call should be filter
#' by the <`master_polygon`> or not.
#'
#' @return An sf dataframe of a census scale filtered by the master polygon.
#' @export
get_census_sus <- function(master_polygon, census_dataset, regions,
                           level, var_select = c("CTUID" = "CT_UID",
                                                 "CSDUID" = "CSD_UID",
                                                 "name" = "name",
                                                 "population" = "Population",
                                                 "households" = "Households"),
                           crs, format = TRUE) {

  out <- cancensus::get_census(dataset = census_dataset, regions = regions,
                               level = level, geo_format = "sf", quiet = TRUE,
                               use_cache = TRUE)

  avail_vars <- var_select[var_select %in% names(out)]
  out <- out[, c("GeoUID", avail_vars)]
  names(out)[1] <- "ID"
  out$name <- out$ID
  out <- out[, c("ID", "name", names(out)[!names(out) %in% c("ID", "name")])]

  if (!format) return(out)

  keep_ids <- sf::st_transform(out, crs)
  keep_ids <- suppressWarnings(sf::st_point_on_surface(keep_ids))
  master_poly_crs <- sf::st_transform(master_polygon, crs)
  keep_ids <- sf::st_filter(keep_ids, master_poly_crs)
  keep_ids <- keep_ids$ID

  out[out$ID %in% keep_ids, ]
}
