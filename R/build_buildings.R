#' Build buildings dataframe
#'
#' `build_buildings()` uses a DA table to download building data from
#' Open Street Map and Microsoft's Canadian Building Footprints.
#'
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame from which boundaries
#' are used to filter the buildings, and add other DA information.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param download_MS_buildings <`logical`> Should the Microsoft's Canadian
#' Building Footprints dataset be used to add the missing buildings to the OSM data?
#' @param MS_province <`character`> The Province for which Microsoft's buildings
#' should be download. One of: \code{"Alberta"}, \code{"BritishColumbia"},
#' \code{"Manitoba"}, \code{"NewBrunswick"}, \code{"NewfoundlandAndLabrador"},
#' \code{"NorthwestTerritories"}, \code{"NovaScotia"}, \code{"Nunavut"},
#' \code{"Ontario"}, \code{"PrinceEdwardIsland"}, \code{"Quebec"},
#' \code{"Saskatchewan"}, \code{"YukonTerritory"}.
#'
#' @return Returns a data.frame of all buildings in the boundaries of the
#' \code{"DA_table"}, with \code{"ID"}, \code{"name_2"}, \code{"DAUID"},
#' \code{"CTUID"}, \code{"CSDUID"}, \code{"osm_ID"} and, \code{"geometry"} columns.
#' @export
build_buildings <- function(DA_table, crs, download_MS_buildings = TRUE,
                            MS_province) {
  possible_province <-
    c(
      "Alberta", "BritishColumbia", "Manitoba", "NewBrunswick",
      "NewfoundlandAndLabrador", "NorthwestTerritories", "NovaScotia",
      "Nunavut", "Ontario", "PrinceEdwardIsland", "Quebec", "Saskatchewan",
      "YukonTerritory"
    )

  if (download_MS_buildings && {
    missing(MS_province) || !MS_province %in% possible_province
  }) {
    stop("Please provide a valid `MS_province`. See function documentation.")
  }

  # Get DA_table bbox
  DA_table_bbox <- sf::st_bbox(DA_table)

  # Transform DA_table to desired projection
  DA_table <- sf::st_transform(DA_table, crs)

  # OSM buildings -----------------------------------------------------------
  # Retrieve ALL OSM building features within bounding box
  building_osm <-
    DA_table_bbox |>
    osmdata::opq(timeout = 200) |>
    osmdata::add_osm_feature(key = "building") |>
    osmdata::osmdata_sf()

  # Make a uniform dataframe off the building polygons and multipolygons
  valid_buildings <-
    lapply(
      list(building_osm$osm_polygons, building_osm$osm_multipolygons),
      \(x) {
        suppressWarnings({
          x <- sf::st_as_sf(x)
          x <- x[, c("osm_id", "geometry")]
          names(x) <- c("osm_ID", "geometry")
          x <- sf::st_cast(x, "MULTIPOLYGON")
          sf::st_make_valid(x)
        })
      }
    )
  building <- do.call(rbind, valid_buildings)
  building <- sf::st_transform(building, crs)
  building <- building[!sf::st_is_empty(building$geometry), ]
  if (length(unique(building$osm_ID)) != nrow(building)) {
    building <- unique(building)
  }
  building <- sf::st_cast(building, "MULTIPOLYGON")
  # Filter centroid in our geometry
  building_centroid <- suppressWarnings(sf::st_centroid(building))
  building_in_DA <- sf::st_filter(building_centroid, DA_table)$osm_ID
  building <- building[building$osm_ID %in% building_in_DA, ]
  # Add ID
  building$ID <- paste0("building_", seq_along(building$osm_ID))
  building <- building[, c("ID", "osm_ID", "geometry")]

  # Get centroid for self-intersection, and find self-intersections
  building_centroid <- suppressWarnings(sf::st_centroid(building))
  self_intersects <- sf::st_intersects(building_centroid, building)
  to_merge <- self_intersects[lengths(self_intersects) > 1]
  # Reducer function, and get reduced lists of intersections
  reduce <- function(x) {
    Reduce(function(a, b) {
      merge_index <- lapply(a, intersect, b)
      if (sum(lengths(merge_index)) > 0) {
        merge_index <- which(lengths(merge_index) > 0)
        merged <- a[merge_index]
        merged <- unlist(merged)
        merged <- union(merged, b)
        merged <- list(sort(merged))
        not_merged <- a[-merge_index]
        out <- c(merged, not_merged)
      } else {
        out <- c(a, list(b))
      }
    }, x, init = list())
  }
  merged <- reduce(to_merge)
  # Buildings that will not be merged
  building_remaining <- building[!building$ID %in% unlist(merged), ]
  # Merge buildings that have self-intersection
  building_merged <- lapply(merged, \(x) {
    z <- building[building$ID %in% paste0("building_", x), ]
    geo <- sf::st_union(z)
    z <- z[1, ]
    z$geometry <- geo
    z
  })
  building_merged <- do.call(rbind, building_merged)
  # Bind everything together
  building_merged <- sf::st_cast(building_merged, "MULTIPOLYGON")
  building_merged <- sf::st_make_valid(building_merged)
  building_merged <- building_merged[!sf::st_is_empty(building_merged$geometry), ]
  building <- rbind(building_remaining, building_merged)
  building <- sf::st_set_agr(building, "constant")


  # Get Microsoft buildings -------------------------------------------------
  # Source <`https://github.com/Microsoft/CanadianBuildingFootprints`>
  if (download_MS_buildings) {
    if (!MS_province %in% possible_province) {
      stop(paste0("`MS_province` must be one of ", paste0(possible_province,
                                                          collapse = ", "
      )))
    }

    # Download building json from the source
    province_url <-
      paste0(
        "https://usbuildingdata.blob.core.windows.net/canadian-buildings-v2/",
        MS_province, ".zip"
      )
    tmp <- tempfile(pattern = "ms_buildings", fileext = ".zip")
    utils::download.file(province_url, destfile = tmp)
    # Manipulation to get the data from the zip file
    geojson_tmp <- tempfile(pattern = "ms_buildings", fileext = ".geojson")
    connection_to_geojson <- unz(tmp, paste0(MS_province, ".geojson"))
    writeLines(readLines(connection_to_geojson), geojson_tmp)
    ms_building <- geojsonsf::geojson_sf(geojson_tmp)
    close(connection_to_geojson)

    # Only keep polygons which intersect with DAs and don't intersect with buildings
    ms_building <- sf::st_transform(ms_building, crs)
    ms_building <- sf::st_filter(ms_building, DA_table)
    ms_building <- ms_building[
      lengths(sf::st_intersects(ms_building$geometry, building)) == 0,
    ]

    # Drop very small polygons and add temporary ID
    ms_building_area <- susbuildr::get_area(ms_building$geometry)
    ms_building <- ms_building[ms_building_area > 10, ]
    ms_building <- sf::st_cast(ms_building, "MULTIPOLYGON")
    ms_building <- sf::st_make_valid(ms_building)
    ms_building <- ms_building[!sf::st_is_empty(ms_building$geometry), ]
    ms_building$ID <- as.character(seq_along(ms_building$geometry))

    # Bind OSM buildings and MS buildings
    ms_building$osm_ID <- NA
    building <- rbind(building, ms_building)
    building_area <- susbuildr::get_area(building$geometry)
    building <- building[building_area > 10, ]
    building$ID <- paste0("building_", seq_along(building$geometry))
    building <- sf::st_set_agr(building, "constant")
    building <- sf::st_make_valid(building)
  }


  # Add DA information to buildings -----------------------------------------

  # Get centroids
  building_centroid <- sf::st_centroid(building)
  # Get DA information for those centroids
  das <- DA_table[, c("ID", "name_2", "CTUID", "CSDUID", "geometry")]
  names(das)[1] <- "DAUID"

  # Join DA information to buildings
  building_DA <- sf::st_drop_geometry(sf::st_join(building_centroid, das))
  building <- merge(building[, c("ID", "geometry")], building_DA, by = "ID")


  # Consolidate and clean output --------------------------------------------

  building$name <- NA_character_
  building <- building[, c(
    "ID", "name", "name_2", "CSDUID", "CTUID", "DAUID",
    "osm_ID", "geometry"
  )]
  building <- sf::st_make_valid(building)
  building <- building[!sf::st_is_empty(building$geometry), ]
  building <- sf::st_transform(building, 4326)
  building <- sf::st_set_agr(building, "constant")

  return(building)
}
