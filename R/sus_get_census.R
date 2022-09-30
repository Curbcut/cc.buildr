sus_get_census <- function(master_polygon, re = list(PR = province_code),
                           sc = scale, var_select, format = TRUE) {

  out <- cancensus::get_census(dataset = census_dataset, regions = re,
                               level = sc, geo_format = "sf", quiet = TRUE)

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
