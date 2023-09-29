#' Retrieve census data and filter it by the master polygon
#'
#' @param master_polygon <`sfc_MULTIPOLYGON`>. Unioned multipolygon of all the
#' geometries for which census data must be gathered.
#' @param census_dataset <`character`> The dataset to query for available
#' regions, e.g. \code{"CA16"}.
#' @param regions <`named list`> A named list of census regions to retrieve.
#' Names must be valid census aggregation levels. Preferably a whole province
#' to make sure all geometries present in the <`master_polygon`> is added.
#' e.g. \code{list(PR = 24)} for Montreal.
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
#' @param switch_full_geos <`logical`> Should the census geometries get switched
#' to the full one (the one which spans over water)?
#' @param area_threshold <`numeric`> How much of a feature should be present
#' in the master polygon to include it in the dataset? Defaults to 0.05. In
#' some cases, if a DA is small on the land but covers lots of water, this
#' threshold must be taken down.
#'
#' @return An sf dataframe of a census scale filtered by the master polygon.
#' @export
get_census_cc <- function(master_polygon, census_dataset, regions,
                          level, var_select = c(
                            "CTUID" = "CT_UID",
                            "CSDUID" = "CSD_UID",
                            "name" = "name",
                            "population" = "Population",
                            "households" = "Households"
                          ),
                          crs, format = TRUE, switch_full_geos = FALSE,
                          area_threshold = 0.05) {
  census_data <- cancensus::get_census(
    dataset = census_dataset, regions = regions,
    level = level, geo_format = "sf", quiet = TRUE,
    use_cache = TRUE
  )

  names(census_data)[names(census_data) == "GeoUID"] <- "ID"
  # Switch for the full census geometries (with water)
  if (switch_full_geos) {
    census_data <- cc.data::census_switch_full_geo(
      df = census_data,
      scale_name = level
    )
  }

  # Few corrections
  census_data <- tibble::as_tibble(census_data)
  census_data <- sf::st_as_sf(census_data)
  census_data <- sf::st_cast(census_data, "MULTIPOLYGON")

  # Select `var_select` columns
  avail_vars <- var_select[var_select %in% names(census_data)]
  census_data <- census_data[, c("ID", avail_vars)]

  # Rename `var_select` columns. Order kept due to previous selecting,
  # which ordered columns.
  names(census_data)[which(names(census_data) %in% avail_vars)] <- names(avail_vars)

  # If the level isn't named, add ID as name
  if (!"name" %in% names(census_data)) census_data$name <- census_data$ID

  # Make sure the order is kept: ID first, name second.
  census_data <- census_data[, c("ID", "name", names(census_data)[
    !names(census_data) %in% c("ID", "name")
  ])]

  # If spatial filter must not be done with the `master_polygon`
  if (!format) {
    return(census_data)
  }

  # Spatial filtering
  census_data <- spatial_filtering(
    df = census_data,
    crs = crs,
    master_polygon = master_polygon,
    ID_col = "ID",
    area_threshold = area_threshold
  )

  # Keep only the polygons part of the master_polygon
  return(census_data)
}
