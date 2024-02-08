#' BC Zoning Page
#'
#' This function processes zoning data and updates the map tileset. It filters and
#' transforms zoning data, calculates various zoning metrics, and uploads the
#' updated data to Mapbox for visualization. The function also updates the modules
#' table with zoning information.
#'
#' @param scales_variables_modules <`list`> A list containing scales, variables,
#' and modules data.
#' @param base_polygons <`sf object`> An sf object containing base polygons
#' for spatial filtering.
#' @param username <`character`> Mapbox username.
#' @param access_token <`character`> Mapbox access token.
#' @param crs <`numeric`> Coordinate Reference System code for spatial transformations.
#' @param overwrite <`logical`> Flag to indicate if existing files should be overwritten.
#' @param overwrite_mb <`logical`> Flag to indicate if existing Mapbox data
#' should be overwritten.
#' @param tileset_prefix <`character`> Prefix for naming the Mapbox tileset.
#' @param region_DA_IDs <`vector`> Vector of DA (Dissemination Area) IDs
#' for extracting frequent bus stops.
#' @param CMA_name_in_coding_xlsx <`character`> In the coding document, what is
#' the `CMA` corresponding to this region?
#'
#' @return <`list`> A list containing updated scales, variables, and modules.
#' @export
zoning_page <- function(scales_variables_modules, base_polygons, username,
                        access_token, crs, overwrite, overwrite_mb,
                        tileset_prefix, region_DA_IDs, CMA_name_in_coding_xlsx) {

  # Filter zoning -----------------------------------------------------------

  if (!file.exists("dev/data/built/zoning.qsm") | overwrite) {

    z <- cc.data::bucket_read_object_zip_shp("LG_ZONING.shp.zip",
                                             "curbcut.bc.zoning")
    z <- sf::st_transform(z, 4326)
    z <- sf::st_make_valid(z)
    z <- z[sf::st_is_valid(z), ]

    # Calculate intersection of each polygon in z with the master_polygon
    z <- sf::st_filter(z, base_polygons$master_polygon)
    z <- spatial_filtering(df = z, crs = crs,
                           master_polygon = base_polygons$master_polygon,
                           ID = "OBJECTID",
                           area_threshold = 0.95)


    z <- tibble::as_tibble(z)
    z <- sf::st_as_sf(z)

    z <- dplyr::group_by(z, PROVIDER, ZONE_CODE, ICI_ZONE) |>
      dplyr::summarize()
    z <- sf::st_cast(z, "MULTIPOLYGON")
    z <- sf::st_transform(z, crs)
    z <- z[order(z$ZONE_CODE), ]

    # Get cadastre for the area
    cadastre <- cc.data::bucket_read_object_zip_shp("BCA.shp.zip",
                                                    "curbcut.bc.zoning")
    cadastre <- tibble::as_tibble(cadastre)
    cadastre <- sf::st_as_sf(cadastre)
    cadastre <- sf::st_transform(cadastre, crs)
    cadastre <- sf::st_filter(cadastre, z)
    cadastre <- sf::st_cast(cadastre, "POLYGON")

    # Clean cadastre
    un <- unique(cadastre["geometry"])
    un <- un[cc.buildr::get_area(un) > 100, ]
    buffered <- sf::st_buffer(un, -2)
    ints <- sf::st_intersects(buffered)
    not_mixed <- un[lengths(ints) == 1, ]
    mixed <- un[lengths(ints) > 1, ]

    graphs <- matchr:::reduce_int(sf::st_intersects(mixed))

    new_mixed <- lapply(graphs, \(x) {
      # Initialize the error flag and precision
      error <- TRUE
      precision <- 0

      while (error) {
        # Try to perform operations and catch any errors
        tryCatch({
          # Update the geometry with increased precision and make it valid
          from <- sf::st_make_valid(mixed[x, ]) |>
            sf::st_set_precision(precision) |>
            sf::st_make_valid()

          # Attempt to compute the intersection
          to <- sf::st_intersection(from)["geometry"]

          # If successful, set error flag to FALSE
          error <- FALSE
        }, error = function(e) {
          # In case of an error, increment precision and leave error flag TRUE
          precision <<- precision + 1
        })
      }
      to <- to[sf::st_is(to, "POLYGON") | sf::st_is(to, "MULTIPOLYGON"), ]
      sf::st_cast(to, "POLYGON")
    })
    new_mixed <- Reduce(rbind, new_mixed)
    cadastre <- rbind(not_mixed["geometry"], new_mixed)

    # Add a cadastre ID
    cadastre$ID <- paste0("lot_", seq_along(cadastre$geometry))

    # Add zoning to every lot
    ints <- sf::st_join(sf::st_centroid(cadastre), z)

    # If one lot falls within more than on zone, remove the duplicate
    keep_rows <- !duplicated(ints$ID)
    ints <- ints[keep_rows, ]

    # Merge good geometry
    zoning_lots <- cc.buildr::merge(sf::st_drop_geometry(ints), cadastre)

    # Rearrange categories
    zoning_lots$ICI_ZONE[is.na(zoning_lots$ICI_ZONE)] <- "OTHER"
    zoning_lots$ICI_ZONE[zoning_lots$ICI_ZONE %in%
                           c("NOT SPECIFIED", "OTHER", "UNKNOWN", "TRANSPORTATION")] <- "OTHER"

    zoning_lots <- sf::st_make_valid(zoning_lots)

    # Reverse geolocate

    centroids <- sf::st_centroid(zoning_lots)
    centroids <- sf::st_transform(centroids, 4326)
    zoning_lots$address <- future.apply::future_sapply(centroids$geometry,
                                                       cc.data::rev_geocode_localhost,
                                                       future.seed = NULL)

    # Is the lot in an heritage conservation area?
    history_areas <- cc.data::bucket_read_object_zip_shp("history_heritage_areas.zip",
                                                         "curbcut.bc.zoning")
    hist <- sf::st_intersects(sf::st_centroid(zoning_lots),
                              sf::st_transform(history_areas, crs))
    zoning_lots$in_hist <- as.logical(lengths(hist))

    # Is the lot in an Urban Containment Boundaries ?
    UCBs <- cc.data::BC_UCB()
    in_ucb <- sf::st_intersects(sf::st_centroid(zoning_lots), UCBs)
    zoning_lots$in_ucb <- as.logical(lengths(in_ucb))


    # Is the lot serviced by Local Government water and sewer systems?
    zoning_lots$in_water_sewer <- TRUE

    # Is the lot in a Transit-Oriented Area?
    toas <- cc.data::db_read_data("BC_TOA") |> sf::st_transform(crs)
    in_toa <- sf::st_intersects(sf::st_centroid(zoning_lots), toas)
    zoning_lots$in_toa <- as.logical(lengths(in_toa))


    ## Does the lot meet bus stop proximity definition?
    stops <- cc.data::db_read_data("BC_frequent_bus_stops", column_to_select = "DA_ID",
                                   IDs = census_scales$DA$ID)
    stops <- sf::st_transform(stops, crs)
    stops <- sf::st_buffer(stops, 400)
    in_bus <- sf::st_intersects(sf::st_centroid(zoning_lots), stops)
    zoning_lots$in_bus <- as.logical(lengths(in_bus))

    # Population count of the municipality of the zone
    CSD <- scales_variables_modules$scales$CSD
    if (is.null(CSD)) {
      csds <- scales_variables_modules$scales$DA$CSD_ID |> unique()
      CSD <- cancensus::get_census("CA21", regions = list(CSD = csds), level = "CSD",
                                   geo_format = "sf")
      CSD <- CSD[c("GeoUID", "Households", "Population")]
      names(CSD) <- c("ID", "households", "population", "geometry")
    }
    zoning_lots <- sf::st_join(zoning_lots, sf::st_transform(CSD["population"], crs))
    names(zoning_lots)[names(zoning_lots) == "population"] <- "pop"
    zoning_lots <- zoning_lots[!is.na(zoning_lots$pop), ]


    # Add the area
    zoning_lots$area <- cc.buildr::get_area(zoning_lots)

    # For Prince George, 'OTHER' ICI_ZONE must be fixed
    if (CMA_name_in_coding_xlsx == "Prince George") {
      zoning_lots$ICI_ZONE <- ifelse(grepl("^A", zoning_lots$ZONE_CODE),
                                     "AGRICULTURAL / RURAL", zoning_lots$ICI_ZONE)
      zoning_lots$ICI_ZONE <- ifelse(grepl("^R|^AR", zoning_lots$ZONE_CODE),
                                     "RESIDENTIAL", zoning_lots$ICI_ZONE)
      zoning_lots$ICI_ZONE <- ifelse(grepl("^C", zoning_lots$ZONE_CODE),
                                     "COMMERCIAL", zoning_lots$ICI_ZONE)
      zoning_lots$ICI_ZONE <- ifelse(grepl("^M", zoning_lots$ZONE_CODE),
                                     "INDUSTRIAL", zoning_lots$ICI_ZONE)
    }
    if (CMA_name_in_coding_xlsx == "Kelowna CMA") {
      zoning_lots$ICI_ZONE[zoning_lots$PROVIDER == "CENTRAL OKANAGAN" &
                             zoning_lots$ICI_ZONE == "RURAL"] <- "AGRICULTURAL / RURAL"
      zoning_lots$ICI_ZONE[zoning_lots$PROVIDER == "CENTRAL OKANAGAN" &
                             zoning_lots$ZONE_CODE %in% c("RU1", "RU2", "RU3",
                                                          "RU4", "RU5", "RU6")] <- "RESIDENTIAL"
    }

    qs::qsave(zoning_lots, "dev/data/built/zoning_lots.qs")
    zoning_lots <- qs::qread("dev/data/built/zoning_lots.qs")

    # # Bill 44 -----------------------------------------------------------------

    # Get the coding information (single or duplex)
    coding <- cc.data::bucket_read_object("restricted_zones_coding.xlsx",
                                          objectext = "xlsx",
                                          bucket = "curbcut.bc.zoning",
                                          method = readxl::read_xlsx)
    coding <- coding[coding$CMA == CMA_name_in_coding_xlsx, ]
    coding <- coding[!is.na(coding$CMA), ]

    coding$single <- !is.na(coding$single) & grepl("x|X", coding$single)
    coding$duplex <- !is.na(coding$single) & grepl("x|X", coding$duplex)

    # Check for coding errors (if both single and duplex are TRUE)
    coding$single <- ifelse(coding$single & coding$duplex, FALSE, coding$single)

    coding$secondary_suite <- !is.na(coding$secondary_suite) & grepl("x|X", coding$secondary_suite)
    coding <- coding[c("PROVIDER", "ZONE_CODE", "single", "duplex", "secondary_suite")]

    # Bind the coding information to residential zones
    # zoning_lots_residential <- zoning_lots[zoning_lots$ICI_ZONE == "RESIDENTIAL", ]
    zoning_lots_residential <-
      cc.buildr::merge(coding, zoning_lots[!names(zoning_lots) %in% "ICI_ZONE"],
                       by = c("PROVIDER", "ZONE_CODE"), all.x = TRUE)
    zoning_lots_residential <- zoning_lots_residential[!is.na(zoning_lots_residential$ID), ]


    # THIS FUNCTION RETURNS THE ADDITIONAL NUMBER OF UNITS
    new_permitted <- function(zone_row) {

      # If the zone is NOT restricted
      if (is.na(zone_row$single)) return(0)
      if (!zone_row$single & !zone_row$duplex) return(0)

      # How many units will it be possible to build?
      new_nb_units <- (\(zone_row) {
        # If the zone is not in an UCB AND the population is less than 5k
        if (!zone_row$in_ucb) {
          if (zone_row$pop < 5000) return(2)
        }

        # If it's not served by Local Governement water and sewer
        if (!zone_row$in_water_sewer) return(2)

        # If it's protected by Heritage Conservation Act
        if (zone_row$in_hist) return(2)

        # If it's in a Transit-Oriented Area (NO CHANGE. They will be subject
        # to separate legislation about TOAs.)
        if (zone_row$in_toa) return(if (zone_row$single) 1 else if (zone_row$duplex) 2)

        # If the lot is larger than 4050m^2
        if (zone_row$area > 4050) return(2)

        # If the lot is less than 280m^2
        if (zone_row$area < 280) return(3)

        if (zone_row$in_bus) return(6)

        if (!zone_row$in_bus) return(4)

        stop("Unable to classify this row")
      })(zone_row)

      # What is the old amount of units?
      old_nb_units <- if (zone_row$single) 1 else if (zone_row$duplex) 2

      # Return the additional number of units. Use floor as if the parcel is in a
      # TAO, then there is no change.
      return(new_nb_units - old_nb_units)

    }

    zoning_lots_residential$additional_units <- apply(zoning_lots_residential, 1, new_permitted)


    # Text and graph ----------------------------------------------------------

    zoning_lots$area <- cc.buildr::get_area(zoning_lots)
    zones_t <- unique(zoning_lots$ICI_ZONE)
    zones <- lapply(zones_t, \(type) {
      size_km2 <- sum(zoning_lots$area[zoning_lots$ICI_ZONE == type])
      c(prettyNum(round(size_km2  / 1e6, digits = 2), big.mark = ","),
        scales::percent(size_km2 / sum(zoning_lots$area), accuracy = 0.1))
    })
    zones <- tibble::tibble(zoning = stringr::str_to_sentence(zones_t),
                            area_km2 = sapply(zones, `[[`, 1),
                            area_pct = sapply(zones, `[[`, 2))


    # Residential -------------------------------------------------------------

    # Categories
    zoning_lots_residential$res_category_before <- apply(zoning_lots_residential, 1, \(x) {
      if (!x$single & !x$duplex) return("Multiresidential")
      if (x$single) {
        if (x$secondary_suite) return("Single + Secondary suite/ADU")
        return("Single")
      }
      if (x$duplex) return("Duplex")
      return("Multiresidential")
    })

    # How many units will it be possible to build?
    zoning_lots_residential$res_category_after <- apply(zoning_lots_residential, 1, \(zone_row) {

      if (!zone_row$single & !zone_row$duplex) return("Multiresidential")

      # If the zone is not in an UCB AND the population is less than 5k
      if (!zone_row$in_ucb) {
        if (zone_row$pop < 5000) return("Single + Secondary suite/ADU")
      }

      # If it's not served by Local Governement water and sewer
      if (!zone_row$in_water_sewer) return("Single + Secondary suite/ADU")

      # If it's protected by Heritage Conservation Act
      if (zone_row$in_hist) return("Single + Secondary suite/ADU")

      # If it's in a Transit-Oriented Area
      if (zone_row$in_toa) return("Alternate process")

      # If the lot is larger than 4050m^2
      if (zone_row$area > 4050) return("Single + Secondary suite/ADU")

      # If the lot is less than 280m^2
      if (zone_row$area < 280) return("Multiresidential (3 units)")

      if (zone_row$in_bus) return("Multiresidential (6 units)")

      if (!zone_row$in_bus) return("Multiresidential (4 units)")

      stop("Unable to classify this row")
    })

    zones_t <- c("Single", "Single + Secondary suite/ADU", "Duplex", "Multiresidential")
    zones_res <- lapply(zones_t, \(type) {
      size_km2 <- sum(zoning_lots_residential$area[zoning_lots_residential$res_category_before == type])
      c(prettyNum(round(size_km2  / 1e6, digits = 1), big.mark = ","),
        scales::percent(size_km2 / sum(zoning_lots_residential$area), accuracy = 0.1))
    })
    zones_residential <- tibble::tibble(before = TRUE,
                                        zoning = zones_t,
                                        area_km2 = sapply(zones_res, `[[`, 1),
                                        area_pct = sapply(zones_res, `[[`, 2))

    zones_t <- c("Single + Secondary suite/ADU", "Multiresidential (3 units)",
                 "Multiresidential (4 units)", "Multiresidential (6 units)",
                 "Multiresidential")
    zones_res <- lapply(zones_t, \(type) {
      size_km2 <- sum(zoning_lots_residential$area[zoning_lots_residential$res_category_after == type])
      c(prettyNum(round(size_km2  / 1e6, digits = 1), big.mark = ","),
        scales::percent(size_km2 / sum(zoning_lots_residential$area), accuracy = 0.1))
    })
    zones_residential <- rbind(zones_residential,
                               tibble::tibble(before = FALSE,
                                              zoning = zones_t,
                                              area_km2 = sapply(zones_res, `[[`, 1),
                                              area_pct = sapply(zones_res, `[[`, 2))
    )


    zones_t <- 0:5
    zones_compare <- lapply(zones_t, \(type) {
      size_km2 <- sum(zoning_lots_residential$area[zoning_lots_residential$additional_units == type])
      c(prettyNum(round(size_km2  / 1e6, digits = 1), big.mark = ","),
        scales::percent(size_km2 / sum(zoning_lots_residential$area), accuracy = 0.1))
    })
    zones_compare <- tibble::tibble(zoning = zones_t,
                                    area_km2 = sapply(zones_compare, `[[`, 1),
                                    area_pct = sapply(zones_compare, `[[`, 2))
    zones_compare <- zones_compare[order(zones_compare$zoning), ]
    zones_compare$zoning <- c("None", "+1 unit", paste0("+", 2:5, " units"))


    # Save zoning tibbles -----------------------------------------------------

    qs::qsavem(zoning_lots, zones, zones_residential, zoning_lots_residential,
               zones_compare, file = "dev/data/built/zoning.qsm")

  }


  # Upload to mapbox --------------------------------------------------------

  current_tilesets <- cc.buildr::tileset_list_tilesets(username, access_token)
  tile_id <- sprintf("%s_zoning", tileset_prefix)


  if (!tile_id %in% current_tilesets$id | overwrite_mb) {
    qs::qload("dev/data/built/zoning.qsm")

    # Merge residential information
    zoning_lots_residential$additional_units <- as.character(zoning_lots_residential$additional_units)
    res <- sf::st_drop_geometry(zoning_lots_residential)
    zoning_lots <- merge(zoning_lots,
                         res[c("ID", "res_category_before", "res_category_after",
                               "additional_units")],
                         by = "ID", all.x = TRUE)

    # Reset
    cc.buildr::tileset_delete_tileset_source(
      tile_id, username = username, access_token = access_token
    )
    cc.buildr::tileset_delete_tileset(
      tile_id, username = username, access_token = access_token
    )
    tryCatch(cc.buildr::tileset_upload_tile_source(
      df = zoning_lots[c("ID", "ZONE_CODE", "ICI_ZONE", "res_category_before", "res_category_after",
                         "additional_units")],
      id = tile_id,
      username = username,
      access_token = access_token
    ), error = function(e) {
      cc.buildr::tileset_upload_tile_source_large(
        df = zoning_lots[c("ID", "ZONE_CODE", "ICI_ZONE", "res_category_before", "res_category_after",
                           "additional_units")],
        id = tile_id,
        username = username,
        access_token = access_token
      )
    })
    # Create the recipe
    recipe <- cc.buildr::tileset_create_recipe(
      layer_names = tile_id,
      source = paste0("mapbox://tileset-source/", username, "/", tile_id),
      minzoom = 0,
      maxzoom = 16,
      layer_size = 2500,
      recipe_name = tile_id
    )
    # Publish tileset
    cc.buildr::tileset_create_tileset(
      tileset = tile_id,
      recipe = recipe,
      username = username,
      access_token = access_token
    )
    cc.buildr::tileset_publish_tileset(
      tileset = tile_id,
      username = username,
      access_token = access_token
    )
  }

  # Make sure no geometry feature follows
  qs::qload("dev/data/built/zoning.qsm")
  zoning_lots <- sf::st_drop_geometry(zoning_lots)
  zoning_lots_residential <- sf::st_drop_geometry(zoning_lots_residential)
  qs::qsavem(zoning_lots, zones, zones_residential, zoning_lots_residential,
             zones_compare, file = "data/zoning.qsm")

  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "zoning",
      theme = "Land use",
      nav_title = "Current and projected zoning",
      title_text_title = "Current and projected zoning",
      title_text_main = paste0(
        "<p>Zoning is a set of laws that separates land into “zones” where ea",
        "ch zone has regulations that dictate what kinds and how many buildings",
        " and dwellings are allowed. Although zoning can be relatively invisibl",
        "e, it shapes the city in important ways. Zoning plays a role in spatia",
        "lly structuring the city and as a result is often entangled in spatial",
        " inequalities like housing crises and environmental racism. Because of",
        " this, addressing land-use regulations can help in the pursuit of spat",
        "ial equity and the common good."
      ),
      title_text_extra = paste0(
        "<p>Upzoning, or inclusionary zoning of low-density residential neighbo",
        "urhoods has recently been put forward as a strategy to tackle housing ",
        "crises in North America. This is done by regulating development densit",
        "y rights. The Small-Scale, Multi-Unit Housing (SSMUH) legislation, or ",
        "Bill 44, aims to increase housing supply, create more diverse housing ",
        "choices, and over time, contribute to more affordable housing across B",
        "C. To visualize this new legislation, select “Residential zones” on th",
        "e left-hand panel, and slide the slider to the right to toggle “Bill 4",
        "4”. The visualization of the zoning changes on this page is Curbcut’s ",
        "interpretation and is not intended as a legal interpretation."
      ),
      metadata = FALSE,
      dataset_info = ""
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = scales_variables_modules$variables,
    modules = modules
  ))

}
