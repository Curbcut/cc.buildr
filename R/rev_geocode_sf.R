#' Reverse geocode spatial features
#'
#' This geocoding works in three steps. The province database of addresses is
#' first downloaded from the National Open Database of Addresses, with URLs to
#' download the zip files available in \code{\link[cc.buildr]{addresses_db_links}}.
#' For the missing addresses, we use the province's reverse geolocating service
#' (at the moment only available for QC and BC). Third, we reverse geocode using
#' the OSM service. The reverse geocoding is done using the centroid of every
#' spatial feature.
#'
#' @param master_polygon <`sfc_MULTIPOLYGON`> Unioned multipolygon of all the
#' geometries for which sf's addresses should be gathered. The National
#' Open Database of Addresses will then be spatially filter using it, reducing
#' computation time of \code{\link[sf]{st_nearest_feature}}
#' @param sf_df <`sf data.frame`> Any data.frame with spatial features
#' @param province_code <`character`> Province code used to download the
#' Open Database of Addresses. One of \code{"AB"}, \code{"BC"}, \code{"MB"},
#' \code{"NB"}, \code{"NT"}, \code{"NS"}, \code{"ON"}, \code{"PE"}, \code{"QC"} or
#' \code{"SK"}.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return The same sf data.frame fed, with the `name` column populated
#' with the reverse geocoded addresses.
#' @export
rev_geocode_sf <- function(master_polygon, sf_df, province_code, crs) {
  if (!province_code %in% cc.buildr::addresses_db_links$province_code) {
    stop(paste0(
      "`province_code` must be an available province_code in ",
      "`cc.buildr::addresses_db_links`."
    ))
  }

  if (!"ID" %in% names(sf_df)) {
    stop(paste0(
      "`sf_df` must have an ID column."
    ))
  }

  # Download and load the addresses -----------------------------------------

  url <- addresses_db_links$link[
    addresses_db_links$province_code == province_code
  ]
  tmp <- tempfile(pattern = "addresses_db", fileext = ".zip")
  utils::download.file(url, destfile = tmp)
  all_files <- utils::unzip(tmp, list = TRUE)
  csv_to_extract <- all_files$Name[all_files$Length == max(all_files$Length)]
  connection_to_csv <- unz(tmp, csv_to_extract)
  addresses_raw <- utils::read.csv(connection_to_csv)
  # close(connection_to_csv)


  # Addresses as sf, and spatially filtered ---------------------------------

  addresses <- sf::st_as_sf(addresses_raw,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  addresses <- sf::st_transform(addresses, crs = crs)

  master_polygon <- sf::st_transform(master_polygon, crs = crs)
  addresses <- sf::st_filter(addresses, master_polygon)


  # Get sfs centroid --------------------------------------------------------

  sf_df_crs <- sf::st_transform(sf_df, crs)
  sf_df_centroids <- suppressWarnings(sf::st_centroid(sf_df_crs))


  # Get closest address for each sf -----------------------------------------

  nearest <- sf::st_nearest_feature(sf_df_centroids, addresses)
  sf_df_centroids$name <-
    # The toTitleCase from tools is pretty slow...
    paste(tools::toTitleCase(tolower(addresses$full_addr[nearest])),
      addresses$csdname[nearest],
      sep = ", "
    )


  # Detect values used too many times ---------------------------------------
  # This indicated the CSD did not share addresses database with The Open Database
  # of Addresses

  same_addresses <- table(sf_df_centroids$name)
  same_addresses <- same_addresses[same_addresses > 5]
  same_addresses <- names(same_addresses)
  sf_df_centroids[
    sf_df_centroids$name %in% same_addresses,
  ]$name <- NA_character_


  # Get addresses from provincial government databases ----------------------

  if (exists(paste0("rev_geocode_", province_code))) {
    for (i in seq_len(nrow(sf_df_centroids))) {
      if (!is.na(sf_df_centroids$name[i])) next

      # Inform progress
      message("\r",
        paste0(
          "Reverse geocoding from province's API - ",
          i, "/", nrow(sf_df_centroids), " - ",
          round(i / nrow(sf_df_centroids), digits = 5) * 100, "%"
        ),
        appendLF = FALSE
      )

      # Save the result
      sf_df_centroids$name[i] <-
        do.call(
          paste0("rev_geocode_", province_code),
          list(sf_df_centroids$geometry[i])
        )
    }
  }


  # Get rest of missing address through OSM ---------------------------------

  for (i in seq_len(nrow(sf_df_centroids))) {
    if (!is.na(sf_df_centroids$name[i])) next

    # Inform progress
    message("\r",
      paste0(
        "Reverse geocoding from OSM - ",
        i, "/", nrow(sf_df_centroids), " - ",
        round(i / nrow(sf_df_centroids), digits = 5) * 100, "%"
      ),
      appendLF = FALSE
    )

    # Save the result
    sf_df_centroids$name[i] <-
      rev_geocode_OSM(sf_df_centroids$geometry[i])
  }


  # Bind to raw sf df -------------------------------------------------------

  out <-
    merge(
      sf_df[, names(sf_df) != "name"],
      sf::st_drop_geometry(sf_df_centroids[, c("ID", "name")], by = "ID")
    )
  out_reordered <-
    out[, c("ID", "name", names(out)[!names(out) %in% c("ID", "name")])]


  # Return ------------------------------------------------------------------

  return(out_reordered)
}
