#' Download OSM and GTFS data, and build the network
#'
#' @param master_polygon <`sfc_MULTIPOLYGON`>. Unioned multipolygon of all the
#' geometries for which census data must be gathered.
#' @param routing_folder <`character`> A folder where the routing files will be
#' downloaded.
#'
#' @return Download GTFS and OSM data in the `routing_folder` and the network.dat file
#' is created (Combine data inputs (gtfs, osm) in a directory to build a
#' multimodal transport network used for routing in R5) using `r5r`.
#' @export
tt_create_routing <- function(master_polygon, routing_folder) {


  # Check -------------------------------------------------------------------

  if (!requireNamespace("r5r", quietly = TRUE)) {
    stop(
      "Package \"r5r\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("osmextract", quietly = TRUE)) {
    stop(
      "Package \"osmextract\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("osmextract", quietly = TRUE)) {
    stop(
      "Package \"aws.s3\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (is.null(getOption("java.parameters")))
    stop("Set a maximum memory to Java, as much as you can spare with: ",
         "`options(java.parameters = '-Xmx16G')` and change 16 for as many ",
         "gb you can use of your RAM. If not enough, the funciton will break ",
         "with a Java 'heap space' error.")

  if (!file.exists(routing_folder)) dir.create(routing_folder)
  if (!grepl("/$", routing_folder)) routing_folder <- paste0(routing_folder, "/")


  # Download all GTFS zips --------------------------------------------------

  # Download CSV of GTFS links from here https://database.mobilitydata.org/
  temp <- tempfile()
  utils::download.file("https://bit.ly/catalogs-csv", temp, quiet = TRUE)
  zip_links <- tibble::as_tibble(read.csv(temp))
  # Download all zips
  zips <- zip_links[
    zip_links$location.country_code == "CA" &
      zip_links$data_type == "gtfs" &
      !zip_links$status %in% c("inactive", "deprecated"), ]

  # Get polygons
  zips <- sapply(seq_len(nrow(zips)), \(x) {
    if (is.na(zips$location.bounding_box.maximum_longitude[x]))
      return(sf::st_polygon())

    lon <- c(zips$location.bounding_box.minimum_longitude[x],
             zips$location.bounding_box.maximum_longitude[x])
    lat <- c(zips$location.bounding_box.minimum_latitude[x],
             zips$location.bounding_box.maximum_latitude[x])

    dat <- data.frame(lon, lat)

    geo <- sf::st_as_sf(x = dat, coords = c("lon", "lat"), crs = 4326) |>
      sf::st_bbox() |>
      sf::st_as_sfc()

    out <- zips[x,]
    out$geometry <- geo
    out
  }, simplify = FALSE)

  zips <- data.table::rbindlist(zips)
  zips <- tibble::as_tibble(zips)
  zips <- sf::st_as_sf(zips)

  zips <- zips[sf::st_intersects(zips, master_polygon, sparse = FALSE), ]

  future.apply::future_sapply(seq_len(nrow(zips)), \(x) {
    prov <- tolower(zips$provider[x])
    prov <- gsub("[^a-z.]", "", prov)
    utils::download.file(zips$urls.latest[x],
                         paste0(routing_folder, prov, ".zip"), mode = "wb",
                         quiet = TRUE)
  }, USE.NAMES = FALSE, future.seed = NULL) |> invisible()

  # # Drop all TAXIS
  # files <-
  #   list.files(routing_folder, full.names = TRUE) |>
  #   stringr::str_subset(".zip$")
  #
  # future.apply::future_lapply(files, \(x) {
  #
  #   folder <- gsub(".zip$", "", x)
  #   dir.create(folder)
  #
  #   unzip(zipfile = x, exdir = folder)
  #
  #   routes <- tibble::as_tibble(read.csv(paste0(folder, "/routes.txt")))
  #   take_out_routes_id <- routes$route_id[routes$route_type > 1500]
  #
  #   trips <- tibble::as_tibble(read.csv(paste0(folder, "/trips.txt")))
  #   take_out_trips_id <- trips$trip_id[trips$route_id %in% take_out_routes_id]
  #
  #   all_files <- list.files(folder, full.names = TRUE)
  #
  #   lapply(all_files, \(y) {
  #     tryCatch({
  #       file <- read.csv(y)
  #       if ("route_id" %in% names(file)) {
  #         file <- file[!file$route_id %in% take_out_routes_id, ]
  #         utils::write.csv(x = file, file = y, quote = FALSE, row.names = FALSE)
  #       }
  #       if ("trip_id" %in% names(file)) {
  #         file <- file[!file$trip_id %in% take_out_trips_id, ]
  #         utils::write.csv(x = file, file = y, quote = FALSE, row.names = FALSE)
  #       }
  #       return()},
  #       error = function(e) {})
  #   })
  #
  #   unlink(x)
  #   invisible(utils::zip(zipfile = x, files = all_files,
  #                        extras = "-j -q"))
  #   unlink(folder, recursive = TRUE)
  #
  # }, future.seed = NULL) |> invisible()


  # Download the .pbf files, drop turn restrictions and merge -----------------

  # Cut the OSM file using the master polygon We need the osmconvert.exe`
  # executable for it. https://wiki.openstreetmap.org/wiki/Osmconvert
  osmconvert <- tempfile(fileext = ".exe")
  invisible(aws.s3::save_object(
    region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
    key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
    secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
    object = "osmconvert.exe",
    bucket = "curbcut.utils",
    file = osmconvert))

  # Which OSM pbf to download
  prov <- provinces_pbf[
      sf::st_intersects(cc.buildr::provinces_pbf, master_polygon,
                        sparse = FALSE), ]

  prov_name <- tolower(prov$name)
  prov_name <- gsub("[^a-z.]", "", prov_name)

  # Download the pbf
  dirtemp <- tempdir()
  osm_link <- osmextract::oe_download(
    file_url = prov$link,
    download_directory = dirtemp,
    quiet = TRUE,
    max_file_size = Inf)

  mp_buffer <- sf::st_buffer(master_polygon, 50000)
  invisible(shell(paste0(
    osmconvert, " ",
    osm_link,
    " -b=",
    paste0(sf::st_bbox(mp_buffer), collapse = ","),
    "--complete-ways",# --complete-multipolygons",# --complete-boundaries",
    " -o=", paste0(getwd(), "/", routing_folder, "clipped.osm.pbf")),
    intern = TRUE))


  # Build network -----------------------------------------------------------

  r5r::setup_r5(data_path = routing_folder, verbose = FALSE)


  # Return the routing folder to then send it in a bucket -------------------

  return(routing_folder)

}

#' Get dissemination block centroids
#'
#' @param master_polygon <`sfc_MULTIPOLYGON`>. Unioned multipolygon of all the
#' geometries for which census data must be gathered.
#' @param region <`named list`> A named list of census regions to retrieve.
#' Names must be valid census aggregation levels. Preferably a whole province
#' to make sure all geometries present in the <`master_polygon`> is added.
#' e.g. \code{list(PR = 24)} for Montreal. Preferably use the output of
#' cc.buildr::create_master_polygon()$province_cancensus_code.
#' @param street <`sf data.frame`> All the streets segments in the study region.
#' @param census_dataset <`character`> The dataset to query for available
#' regions, e.g. \code{"CA16"}.
#' @param routing_folder <`character`> A folder where the routing files were
#' downloaded.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return Returns a point sf data.frame where every point attached to
#' dissemination blocks are on the street network the closest to the centroid.
#' @export
tt_get_db <- function(master_polygon, region, street,
                      census_dataset = cc.buildr::current_census,
                      routing_folder, crs) {


  # Get the street from the OSM retrieval -----------------------------------

  if (!grepl("/$", routing_folder)) routing_folder <- paste0(routing_folder, "/")
  osm_file <- list.files(routing_folder, full.names = TRUE)
  osm_file <- osm_file[grepl("\\.osm\\.pbf$", osm_file)]
  osm_streets <- osmextract::oe_read(osm_file, layer = "lines", quiet = TRUE)

  DB_retrieval_polygon <-
    sf::st_bbox(osm_streets) |>
    sf::st_as_sfc()


  # Get dissemination blocks ------------------------------------------------

  DB <- get_census_cc(
    master_polygon = DB_retrieval_polygon,
    census_dataset = census_dataset,
    regions = region,
    level = "DB",
    crs = crs
  )


  # Get a DB point on the street network ------------------------------------

  street <- sf::st_transform(street, crs)
  street_DB <- split(street, street$DB_ID)

  pb <- progressr::progressor(steps = length(street_DB))
  DB_street_centroid <-
    future.apply::future_lapply(street_DB, \(strts) {

      bbox_centroid <-
        sf::st_bbox(strts) |>
        sf::st_as_sfc() |>
        sf::st_centroid()

      strts_centroids <- suppressWarnings(sf::st_centroid(strts))

      dist <- sf::st_distance(strts_centroids, bbox_centroid)
      geom <- strts_centroids$geometry[which(dist == min(dist))]

      pb()

      tibble::tibble(DB_ID = strts$DB_ID[1],
                     geometry = geom)

    }, future.seed = NULL)

  DB_street_centroid <- data.table::rbindlist(DB_street_centroid)
  DB_street_centroid <- sf::st_as_sf(tibble::as_tibble(DB_street_centroid))


  # Fill in missing DBs -----------------------------------------------------

  missing_DB <- DB[!DB$ID %in% DB_street_centroid$DB_ID]

  osm_streets <- osm_streets[, c("osm_id", "geometry")]
  osm_streets <- sf::st_transform(osm_streets, crs)

  osm_streets <- suppressWarnings(sf::st_intersection(osm_streets, missing_DB))
  osm_streets <- osm_streets[, c("ID", "geometry")]
  names(osm_streets)[1] <- "DB_ID"

  osm_streets <- split(osm_streets, osm_streets$DB_ID)

  pb <- progressr::progressor(steps = length(osm_streets))
  osm_streets_centroid <-
    future.apply::future_lapply(osm_streets, \(strts) {

      bbox_centroid <-
        sf::st_bbox(strts) |>
        sf::st_as_sfc() |>
        sf::st_centroid()

      strts_centroids <- suppressWarnings(sf::st_centroid(strts))

      dist <- sf::st_distance(strts_centroids, bbox_centroid)
      geom <- strts_centroids$geometry[which(dist == min(dist))]

      pb()

      tibble::tibble(DB_ID = strts$DB_ID[1],
                     geometry = geom)

    }, future.seed = NULL)

  osm_streets_centroid <- data.table::rbindlist(osm_streets_centroid)
  osm_streets_centroid <- sf::st_as_sf(tibble::as_tibble(osm_streets_centroid))


  # Return ------------------------------------------------------------------

  return(rbind(DB_street_centroid, osm_streets_centroid))

}
