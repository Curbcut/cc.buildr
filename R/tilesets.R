#' List all tile sources in a Mapbox account
#'
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return A data.frame listing all the tile sources in the account.
#' @export
tileset_list_tile_sources <- function(username, access_token) {
  res <- httr::GET(
    paste0(
      "https://api.mapbox.com/tilesets/v1/sources/",
      username
    ),
    query = list(access_token = access_token, limit = 500)
  )
  resDF <- jsonlite::fromJSON(httr::content(res, as = "text"))
  while (isTRUE(grepl("next", res$headers$link))) {
    res <- httr::GET(stringr::str_extract(res$headers$link, "(?<=\\<).*(?=>)"),
      query = list(access_token = access_token, limit = 500)
    )
    resDF <- rbind(resDF, jsonlite::fromJSON(httr::content(res, as = "text")))
  }

  resDF <- tibble::as_tibble(resDF)
  resDF$id <- gsub(
    paste0("mapbox://tileset-source/", username, "/"), "",
    resDF$id
  )
  resDF$size <- resDF$size / 1024^2

  resDF
}


#' Upload a tilesource to a Mapbox account
#'
#' @param df <`sf data.frame`> The `sf` data.frame that must be uploaded to
#' mapbox. CRS must be 4326.
#' @param id <`character`> The ID of the new tile source. For a scale, usually
#' follows a prefix (mtl), the region (CMA), and the scale (CSD), e.g `mtl_CMA_CSD`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds.
#' @export
tileset_upload_tile_source <- function(df, id, username, access_token) {
  current_crs <- sf::st_crs(df)$input
  if (current_crs != "EPSG:4326") {
    df <- sf::st_transform(df, 4326)
    warning(paste0("crs of input data was not 4326 as it need to be to upload ",
                   "to Mapbox. CRS has been updated."))
  }

  # Initialize tempfile
  tmp1 <- tempfile(fileext = ".json")
  tmp2 <- tempfile(fileext = ".geojson")

  # Write Geojson to tempfile
  out <- utils::capture.output(utils::capture.output(
    geojsonio::geojson_write(df, file = tmp2),
    type = "message"
  ))

  suppressWarnings(readtext::readtext(tmp2)) |>
    paste0(collapse = " ") |>
    geojson::featurecollection() |>
    geojson::ndgeo_write(tmp1)

  # Construct system call
  out <- paste0(
    'curl -X POST "https://api.mapbox.com/tilesets/v1/sources/',
    username, "/", id, "?access_token=", access_token,
    '" -F file=@', tmp1,
    ' --header "Content-Type: multipart/form-data"'
  )

  system(out)
}

#' Upload a large tilesource to a Mapbox account
#'
#' Uploads a large tile source to Mapbox Tiling Service (MTS) in 10 iterations.
#'
#' @param df <`sf data.frame`> The `sf` data.frame that must be uploaded to
#' mapbox. CRS must be 4326.
#' @param id <`character`> The ID of the new tile source. For a scale, usually
#' follows a prefix (mtl), the region (CMA), and the scale (CSD), e.g `mtl_CMA_building`.
#' @param total_batches <`numeric`> Number of batches to divide the data frame into
#' for the upload process. Defaults to 100. As the dataset is large, the more batches
#' will be needed.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing when succesfull.
#' @export
tileset_upload_tile_source_large <-
  function(df, id, total_batches = 100, username, access_token) {

    current_crs <- sf::st_crs(df)$input
    if (current_crs != "EPSG:4326") {
      df <- sf::st_transform(df, 4326)
      warning(paste0("crs of input data was not 4326 as it need to be to upload ",
                     "to Mapbox. CRS has been updated."))
    }

    iter_size <- ceiling(nrow(df) / total_batches)

    to_process_list <-
      lapply(1:total_batches, \(x) {
        df[(((x - 1) * iter_size + 1):min(x * iter_size, nrow(df))), ] |>
          geojsonsf::sf_geojson() |>
          paste0(collapse = " ") |>
          geojson::featurecollection()
      })

    batch_size <- total_batches / 10
    num_groups <- ceiling(length(to_process_list) / batch_size)

    for (group in 1:num_groups) {
      # Define the range for each group
      start <- (group - 1) * batch_size + 1
      end <- min(group * batch_size, length(to_process_list))

      # Temporary files for each of the batches in the group
      tmp_list <- lapply(start:end, \(x) tempfile(fileext = ".json"))

      # Write to temporary files
      mapply(geojson::ndgeo_write, to_process_list[start:end], tmp_list)

      # Concatenate geoJSONs
      tmp <- tempfile(fileext = ".json")
      if (Sys.info()[["sysname"]] == "Windows") {
        out <- paste0("type ", paste(tmp_list, collapse = " "), " > ", tmp)
        shell(out)
      } else {
        out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
        system(out)
      }

      # Upload to MTS
      out <- paste0(
        'curl --retry 5 --retry-delay 5 -X POST "https://api.mapbox.com/tilesets/v1/sources/',
        username, "/", id, "?access_token=", access_token,
        '" -F file=@', tmp,
        ' --header "Content-Type: multipart/form-data"'
      )
      system(out)
    }
  }


#' Delete tileset source
#'
#' @param id <`character`> ID of the tileset source that needs to be deleted.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return A success message if succeeds.
#' @export
tileset_delete_tileset_source <- function(id, username, access_token) {
  out <- httr::DELETE(
    paste0(
      "https://api.mapbox.com/tilesets/v1/sources/",
      username, "/", id
    ),
    query = list(access_token = access_token)
  )

  if (rlang::is_empty(httr::content(out))) {
    return("Success")
  }
  return(httr::content(out))
}


#' List all tilesets in a Mapbox account
#'
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return A data.frame listing all the tilesets.
#' @export
tileset_list_tilesets <- function(username, access_token) {
  res <- httr::GET(
    paste0(
      "https://api.mapbox.com/tilesets/v1/",
      username
    ),
    query = list(access_token = access_token, limit = 500)
  )
  resDF <- jsonlite::fromJSON(httr::content(res, as = "text"))
  resDF <- resDF[, c("id", "filesize", "status")]

  while (isTRUE(grepl("next", res$headers$link))) {
    res <- httr::GET(stringr::str_extract(res$headers$link, "(?<=\\<).*(?=>)"),
      query = list(access_token = access_token, limit = 500)
    )
    res <- jsonlite::fromJSON(httr::content(res, as = "text"))
    res <- res[, c("id", "filesize", "status")]
    resDF <- rbind(resDF, res)
  }

  resDF <- tibble::as_tibble(resDF)
  resDF$id <- gsub(paste0(username, "\\."), "", resDF$id)
  resDF$size <- resDF$filesize / 1024^2

  resDF
}


#' Create a tileset
#'
#' @param tileset <`character`> Name of the tileset that will be created
#' @param recipe <`character`> Tileset recipe previously created with
#' \code{\link[cc.buildr]{tileset_create_recipe}}
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return A success message if succeeds.
#' @export
tileset_create_tileset <- function(tileset, recipe, username, access_token) {
  # More complex httr::RETRY
  out <- httr::POST(
    url = paste0(
      "https://api.mapbox.com/tilesets/v1/",
      username, ".", tileset
    ),
    query = list(access_token = access_token),
    body = recipe,
    httr::content_type("application/json")
  )

  if (rlang::is_empty(httr::content(out))) {
    return("Success")
  }
  return(httr::content(out))
}


#' Delete a tileset
#'
#' @param id <`character`> ID of the tileset that needs to be deleted.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds.
#' @export
tileset_delete_tileset <- function(id, username, access_token) {
  out <- httr::DELETE(paste0(
    "https://api.mapbox.com/tilesets/v1/", username,
    ".", id
  ), query = list(access_token = access_token))

  return(httr::content(out))
}


#' Publish a previously created tileset
#'
#' @param tileset <`character`> Name of the previously created tileset (with
#' \code{\link[cc.buildr]{tileset_create_tileset}}) which will be published.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return A success message if succeeds.
#' @export
tileset_publish_tileset <- function(tileset, username, access_token) {
  out <- httr::RETRY("POST",
    url = paste0(
      "https://api.mapbox.com/tilesets/v1/",
      username, ".", tileset,
      "/publish"
    ),
    query = list(access_token = access_token),
    times = 4,
    pause_min = 35
  )


  if (rlang::is_empty(httr::content(out))) {
    return("Success")
  }
  return(httr::content(out))
}


#' Create a recipe (character)
#'
#' Create a recipe from scratch. Almost every argument takes a named vector as
#' for example the auto-zoom must present multiple features at different zoom levels.
#' So the `minzoom` must be a named vector, the name corresponding to one entry
#' in the `layer_names`. For more information on every argument, visit
#' https://docs.mapbox.com/mapbox-tiling-service/reference/
#'
#' @param layer_names <`named character vector`> All the layers that should be
#' part of the tileset. All the other arguments are named accordignly to what
#' is present in `layer_names`.
#' @param source <`named character vector`> The tileset source of every layer.
#' @param minzoom <`named numeric vector`> The minimum zoom levels at which
#' the features in the tileset should be visible.
#' @param maxzoom <`named numeric vector`> The maximum zoom levels at which
#' the features in the tileset should be visible.
#' @param layer_size <`named numeric vector`> Sets the maximum allowed size
#' (in KiB) of the tile layer
#' @param simp_zoom <`named numeric vector`> At which zoom should the simplification
#' starts taking place.
#' @param simp_value <`named numeric vector`> The simplification value. The normal
#' default simplification value is 4. The `simp_value` will take place for zoom
#' levels higher than the `simp_zoom`.
#' @param fallback_simp_zoom <`named numeric vector`> The simplification value for
#' every zoom level below the `simp_zoom`
#' @param bbox <`numeric vector`> Bounding box of the source, used for testing.  It
#' must contain an array of four numbers in this order: the minimum longitude,
#' minimum latitude, maximum longitude, and maximum latitude.
#' @param recipe_name <`character`> The ID of the new tile source. For a scale, usually
#' follows a prefix (mtl), the region (CMA), and the scale (CSD), e.g `mtl_CMA_CSD`.
#'
#' @return A JSON document containing configuration options that tell Mapbox Tiling
#' Service (MTS) how to turn tileset source data into vector tiles.
#' @export
tileset_create_recipe <- function(layer_names, source, minzoom, maxzoom,
                                  layer_size = 2500, simp_zoom = NULL, simp_value = NULL,
                                  fallback_simp_zoom = 4, bbox = NULL, recipe_name) {
  out <- list()
  out$recipe$version <- 1
  out$name <- recipe_name
  layers <- list()

  out$recipe$layers <-
    if (length(layer_names) > 1) {
      z <- lapply(layer_names, function(layer) {
        layers[[layer]]$source <- source[[layer]]
        layers[[layer]]$minzoom <- minzoom[[layer]]
        layers[[layer]]$maxzoom <- maxzoom[[layer]]
        if (!is.null(layer_size[[layer]]) && !is.na(layer_size[[layer]])) {
          layers[[layer]]$tiles$layer_size <- layer_size[[layer]]
        }
        if (!is.null(simp_zoom[[layer]]) &&
          !is.na(simp_zoom[[layer]])) {
          layers[[layer]]$features$simplification[[1]] <- "case"
          layers[[layer]]$features$simplification[[2]] <-
            list("==", "zoom", simp_zoom[[layer]])
          layers[[layer]]$features$simplification[[3]] <-
            if (!is.null(simp_value[[layer]]) &&
              !is.na(simp_value[[layer]])) {
              simp_value[[layer]]
            } else {
              1
            }
          layers[[layer]]$features$simplification[[4]] <-
            fallback_simp_zoom[[layer]]
        }
        if (!is.null(bbox)) layers[[layer]]$tiles$bbox <- bbox
        layers
      })
      Reduce(c, z)
    } else {
      layers[[layer_names]]$source <- source
      layers[[layer_names]]$minzoom <- minzoom
      layers[[layer_names]]$maxzoom <- maxzoom
      if (!is.null(layer_size) && !is.na(layer_size)) {
        layers[[layer_names]]$tiles$layer_size <- layer_size
      }
      if (!is.null(simp_zoom) && !is.na(simp_zoom)) {
        layers[[layer_names]]$features$simplification[[1]] <- "case"
        layers[[layer_names]]$features$simplification[[2]] <-
          list("==", "zoom", simp_zoom)
        layers[[layer_names]]$features$simplification[[3]] <-
          if (!is.null(simp_value) && !is.na(simp_value)) simp_value else 1
        layers[[layer_names]]$features$simplification[[4]] <-
          fallback_simp_zoom
      }
      if (!is.null(bbox)) layers[[layer_names]]$tiles$bbox <- bbox
      layers
    }

  out <- jsonlite::toJSON(out, pretty = TRUE, auto_unbox = TRUE)

  out <-
    gsub(
      paste0(
        '\"simplification\": \\[\n            ',
        '\"case\",\n            \\[\n         ',
        '     \"==\",\n              \"zoom\",'
      ),
      paste0(
        '\"simplification\": \\[\n            \"case\",\n  ',
        '          \\[\n              \"==\",\n            ',
        '  [ \"zoom\" ],'
      ), paste0(out)
    ) |>
    jsonlite::prettify()

  out
}

#' Do it all tileset function
#'
#' Constructs the combinations with the default auto-zoom, creates the tile
#' sources, the recipes, the creation and publishing of tilesets. For the auto-zoom
#' that will get attached for each region, only the first scale along with `CT`,
#' `DA` and `building` will be part of the default region auto-zoom.
#'
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#' @param map_zoom_levels <`named list`> The previously created zoom levels
#' using \code{\link[cc.buildr]{map_zoom_levels_create_all}} and
#' \code{\link[cc.buildr]{map_zoom_levels_create_custom}}.
#' @param tweak_max_zoom <`named list`> What would be the maximum zoom of the
#' additional scales? Named list, where the name is the scale name and the value
#' is the maximum zoom of the scale.
#' @param inst_prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#' @param no_reset <`character vector`> Which scale should not be re-uploaded?
#' If boundaries change and new tilesets are necessary, potentially buildings would
#' not be re-uploaded as it's costly and they don't change. Defaults to NULL for
#' resetting all scales.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
tileset_upload_all <- function(map_zoom_levels, tweak_max_zoom = NULL,
                               inst_prefix, username, access_token, no_reset = NULL) {

  # Remove grids (they have their own tileset upload function)
  map_zoom_levels <- map_zoom_levels[!grepl("^mzl_grd", names(map_zoom_levels))]

  # Grab all scales
  all_scales <- sapply(map_zoom_levels, names)
  all_scales <- unique(unlist(all_scales, use.names = FALSE))

  all_scales <- sapply(all_scales, \(x) {
    file <- sprintf("data/geometry_export/%s.qs", x)
    if (x %in% db_scales) {
      conn <- db_connect_prod()
      out <- db_try_disconnect(conn = conn,
                               fun = DBI::dbGetQuery(conn,
                                                     sprintf("SELECT * FROM %s.%s",
                                                             inst_prefix, x)))
      db_disconnect_prod(conn)
      return(out)
    }

    if (!file.exists(file)) {
      stop(sprintf("scale geometry file `%s` does not exist, and scale not in DB.", file))
    }
    qs::qread(file)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Remove from all scales the scales we shouldn't reset the tileset for
  if (!is.null(no_reset)) {
    all_scales <- all_scales[!names(all_scales) %in% no_reset]
  }

  # # Switch the geometry for digital
  # all_scales <- lapply(all_scales, \(scale_df) {
  #   # If there is no digital geometry, return raw
  #   if (!"geometry_digital" %in% names(scale_df)) return(scale_df)
  #
  #   # Switch the geometry to the digital one
  #   scale_df <- sf::st_drop_geometry(scale_df)
  #   names(scale_df)[names(scale_df) == "geometry_digital"] <- "geometry"
  #   sf::st_as_sf(scale_df)
  # })

  # Reset
  mapply(\(scale_name, scale_df) {
    name <- paste(inst_prefix, scale_name, sep = "_")

    tileset_delete_tileset_source(
      id = name,
      username = username,
      access_token = access_token
    )

    tileset_delete_tileset(
      id = name,
      username = username,
      access_token = access_token
    )
  }, names(all_scales), all_scales, SIMPLIFY = FALSE)

  # DO THE SAME FOR AUTOZOOMS
  lapply(names(map_zoom_levels), \(x) {
    x <- gsub("mzl_", "", x)
    x <- paste(inst_prefix, x, sep = "_")

    tileset_delete_tileset_source(
      id = x,
      username = username,
      access_token = access_token
    )

    tileset_delete_tileset(
      id = x,
      username = username,
      access_token = access_token
    )
  })

  # Tileset sources
  mapply(function(scale_name, scale_df) {
    scale_name <- paste(inst_prefix, scale_name, sep = "_")

    # We can detect too large data to pass by the normal flow by the fact
    # they don't have population and households interpolated
    if (grepl("_building$", scale_name)) {
      df <- scale_df[, grepl("ID$", names(scale_df))]
      df <- df[, c("ID", "DA_ID")]
      names(df) <- c("ID", "ID_color", "geometry")
      df <- sf::st_transform(df, 4326)

      tileset_upload_tile_source_large(
        id = scale_name,
        df = df,
        username = username,
        access_token = access_token
      )
    } else {
      df <- scale_df[, grepl("ID$", names(scale_df))]
      df$ID_color <- df$ID
      df <- sf::st_transform(df, 4326)
      df <- sf::st_set_agr(df, "constant")

      tileset_upload_tile_source(df,
                                 id = scale_name,
                                 username = username,
                                 access_token = access_token
      )
    }
  }, names(all_scales), all_scales, SIMPLIFY = FALSE)

  # Create recipe, create tileset and publish
  maxzooms <-
    tibble::tibble(
      scale = c("first_level", "CT", "DA", "DB", "building"),
      maxzoom = c(11, 12, 13, 14, 16)
    )
  addition <- if (!is.null(tweak_max_zoom)) {
    tibble::tibble(
      scale = names(tweak_max_zoom),
      maxzoom = unlist(tweak_max_zoom)
    )
  } else {
    tibble::tibble()
  }

  maxzooms <- rbind(addition, maxzooms)
  # If there are duplicated, only keep the ones coming first in the 'tweak_max_zoom'
  maxzooms <- maxzooms[!duplicated(maxzooms$scale), ]

  all_recipes <-
    mapply(\(scale_name, scale_df) {

      source_names <- paste(inst_prefix, scale_name, sep = "_")
      sources <- paste0("mapbox://tileset-source/", username, "/", source_names)
      names(sources) <- source_names
      minzooms <- 3
      names(minzooms) <- source_names

      default_maxzoom <- maxzooms$maxzoom[maxzooms$scale == scale_name]
      new_maxzoom <- max(default_maxzoom, 14)
      maxzooms_ <- new_maxzoom
      names(maxzooms_) <- source_names
      layer_sizes <- 2500
      names(layer_sizes) <- source_names

      recipe <- tileset_create_recipe(
        layer_names = source_names,
        source = sources,
        minzoom = minzooms,
        maxzoom = maxzooms_,
        recipe_name = source_names,
        layer_size = layer_sizes
      )

      tileset_create_tileset(source_names,
                             recipe = recipe,
                             username = username,
                             access_token = access_token
      )

      tileset_publish_tileset(source_names,
                              username = username,
                              access_token = access_token
      )
    }, names(all_scales), all_scales, SIMPLIFY = FALSE)

  # Function to calculate on autozoom when the scale starts and when it ends
  calculate_zoom_levels <- function(zoom_levels) {
    # Initialize the output tibble
    result <- tibble::tibble(
      scale = character(), min_zoom = integer(),
      max_zoom = integer()
    )

    # Loop through the named numeric vector
    for (i in seq_along(zoom_levels)) {
      scale_name <- names(zoom_levels)[i]
      zoom_value <- zoom_levels[i]

      # Calculate min zoom
      min_zoom <- ifelse(i == 1, 0, result$max_zoom[i - 1] + 1)

      # Calculate max zoom
      max_zoom <-
        if (length(zoom_levels) == 1) {
          10
        } else {
          ifelse(i == length(zoom_levels), zoom_value, zoom_levels[i + 1] - 1)
        }


      # Add the min and max zoom to the result tibble
      result <-
        tibble::add_row(result,
                        scale = scale_name, min_zoom = min_zoom,
                        max_zoom = max_zoom
        )
    }

    return(result)
  }

  auto_zoom_recipes <-
    mapply(\(mzl_name, zoom_levels) {

      az_name <- gsub("^mzl", inst_prefix, mzl_name)
      scale_names <- paste(inst_prefix, names(zoom_levels), sep = "_")


      sources <- stats::setNames(paste0(
        "mapbox://tileset-source/", username, "/",
        scale_names
      ), scale_names)

      zooms <- calculate_zoom_levels(zoom_levels)
      minzooms <- zooms$min_zoom
      maxzooms <- zooms$max_zoom
      maxzooms[length(maxzooms)] <- 16
      names(minzooms) <- scale_names
      names(maxzooms) <- scale_names

      layer_sizes <-
        stats::setNames(rep(NA, length(scale_names)), scale_names)

      recipe <-
        tileset_create_recipe(
          layer_names = scale_names,
          source = sources,
          minzoom = minzooms,
          maxzoom = maxzooms,
          layer_size = layer_sizes,
          recipe_name = az_name
        )

      # Reset
      tileset_delete_tileset_source(
        id = az_name,
        username = username,
        access_token = access_token
      )
      tileset_delete_tileset(
        id = az_name,
        username = username,
        access_token = access_token
      )
      # Give some time so the deletion is completed
      Sys.sleep(5)

      # New tileset
      tileset_create_tileset(az_name,
                             recipe = recipe,
                             username = username,
                             access_token = access_token
      )
      tileset_publish_tileset(az_name,
                              username = username,
                              access_token = access_token
      )
    }, names(map_zoom_levels), map_zoom_levels, SIMPLIFY = FALSE)


  return(invisible(NULL))
}

#' Create CSD labels tileset
#'
#' @param scales <`list`> Lists of spatial features dataframes with regions and
#' scales, Usually `scales_variables_modules$scales`. Their labels will be the
#' `name` column of the first scale for each region, and the `population` column
#' will be used to weight which label to display first.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param inst_prefix <`character`> inst_prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
tileset_labels <- function(scales, crs, inst_prefix, username, access_token) {
  mapply(\(region_name, scale) {
    scale <- scale[[1]]

    ## Error catch
    if (all((!c("name", "population", "geometry") %in% names(scale)))) {
      stop("One of `name`, `population` or `geometry` column is missing.")
    }

    name <- paste(inst_prefix, region_name, "label", sep = "_")

    # Calculate centroid using a projection
    scale <- sf::st_transform(scale, crs)

    scale <- scale[c("name", "population", "geometry")]
    scale$name <- stringi::stri_trans_general(scale$name, id = "Latin-ASCII")
    scale <- sf::st_set_agr(scale, "constant")
    scale <- sf::st_centroid(scale)

    scale <- sf::st_transform(scale, 4326)
    tileset_upload_tile_source(
      df = scale, id = name,
      username = username,
      access_token = access_token
    )

    recipe_label <- paste0('
{
  "recipe": {
    "version": 1,
    "layers": {
      "label": {
        "source": "mapbox://tileset-source/', username, "/", name, '",
        "minzoom": 8,
        "maxzoom": 14,
        "tiles": {
          "limit": [
            [ "highest_where_in_distance", true, 4, "population" ]
          ]
        }
      }
    }
  },
  "name": "', name, '"
}
')

    # Create and publish tileset

    tileset_create_tileset(name, recipe_label,
      username = username,
      access_token = access_token
    )
    tileset_publish_tileset(name,
      username = username,
      access_token = access_token
    )
  }, names(scales), scales)
}

#' Create and publish street and park tilesets
#'
#' @param master_polygon <`sf data.frame`> Unioned multipolygon of all the
#' geometries for which data has been gathered.
#' @param street <`sf data.frame`> All the streets in the zone under study.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param inst_prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
tileset_streets <- function(master_polygon, street, crs, inst_prefix, username,
                            access_token) {
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    stop(
      "Package \"osmdata\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Subset the street in groups
  street_1 <- street[street$rank %in% c(1, 2, 3), "ID"]
  street_2 <- street[street$rank == 4, "ID"]
  street_3 <- street[street$rank == 5, "ID"]

  # Back to 4326
  street_1 <- sf::st_transform(street_1, 4326)
  street_2 <- sf::st_transform(street_2, 4326)
  street_3 <- sf::st_transform(street_3, 4326)

  # Delete tilesets
  ids <- paste0(inst_prefix, "_street_", c(1:3))
  lapply(ids, tileset_delete_tileset,
    username = username,
    access_token = access_token
  )
  lapply(ids, tileset_delete_tileset_source,
    username = username,
    access_token = access_token
  )

  # Upload tile_source
  tileset_upload_tile_source(
    df = street_1,
    id = paste0(inst_prefix, "_street_1"),
    username = username,
    access_token = access_token
  )
  tileset_upload_tile_source(
    df = street_2,
    id = paste0(inst_prefix, "_street_2"),
    username = username,
    access_token = access_token
  )
  tileset_upload_tile_source_large(
    df = street_3,
    id = paste0(inst_prefix, "_street_3"),
    username = username,
    access_token = access_token
  )

  # Upload an empty point
  north_pole_coords <- c(90, 0)
  north_pole <- sf::st_point(north_pole_coords, dim = "XY")
  north_pole_sf <- sf::st_sf(
    data = tibble::tibble(name = "North Pole"),
    geometry = sf::st_sfc(north_pole), crs = 4326
  )

  tileset_upload_tile_source(
    df = north_pole_sf,
    id = paste0(inst_prefix, "_np"),
    username = username,
    access_token = access_token
  )


  # Load and process park data
  bb <- sf::st_bbox(sf::st_transform(street, 4326))

  park <-
    osmdata::opq(bb, timeout = 200) |>
    osmdata::add_osm_feature(key = "leisure") |>
    osmdata::osmdata_sf()
  park <- sf::st_cast(park$osm_polygons, "POLYGON")

  park <- sf::st_transform(park, crs)
  master_polygon <- sf::st_transform(master_polygon, crs)
  park <- sf::st_filter(park, master_polygon)

  park <- sf::st_transform(park, 4326)
  park <- sf::st_set_agr(park, "constant")
  park <- park[park$leisure != "nature_reserve" & !is.na(park$leisure), "name"]

  tileset_delete_tileset_source(
    id = paste0(inst_prefix, "_park"),
    username = username,
    access_token = access_token
  )
  tileset_upload_tile_source(
    df = park,
    id = paste0(inst_prefix, "_park"),
    username = username,
    access_token = access_token
  )


  # Create recipes
  # Street 1
  source_names <- c("_street_1", "_np")
  sources <- paste0("mapbox://tileset-source/", username, "/", inst_prefix, source_names)
  names(sources) <- source_names
  minzooms <- c(9, 14)
  names(minzooms) <- source_names
  maxzooms <- c(13, 15)
  names(maxzooms) <- source_names
  recipe_street_1 <- tileset_create_recipe(
    layer_names = source_names,
    source = sources,
    minzoom = minzooms,
    maxzoom = maxzooms,
    recipe_name = paste0(inst_prefix, "_street_1")
  )

  # Street 2
  source_names <- c("_street_2", "_np")
  sources <- paste0("mapbox://tileset-source/", username, "/", inst_prefix, source_names)
  names(sources) <- source_names
  minzooms <- c(11, 14)
  names(minzooms) <- source_names
  maxzooms <- c(13, 15)
  names(maxzooms) <- source_names
  recipe_street_2 <- tileset_create_recipe(
    layer_names = source_names,
    source = sources,
    minzoom = minzooms,
    maxzoom = maxzooms,
    recipe_name = paste0(inst_prefix, "_street_2")
  )

  # Street 3
  source_names <- c("_street_3", "_np")
  sources <- paste0("mapbox://tileset-source/", username, "/", inst_prefix, source_names)
  names(sources) <- source_names
  minzooms <- c(13, 14)
  names(minzooms) <- source_names
  maxzooms <- c(13, 15)
  names(maxzooms) <- source_names
  recipe_street_3 <- tileset_create_recipe(
    layer_names = source_names,
    source = sources,
    minzoom = minzooms,
    maxzoom = maxzooms,
    recipe_name = paste0(inst_prefix, "_street_3")
  )


  # Publish tileset
  tileset_create_tileset(
    tileset = paste0(inst_prefix, "_street_1"),
    recipe = recipe_street_1,
    username = username,
    access_token = access_token
  )
  tileset_publish_tileset(
    tileset = paste0(inst_prefix, "_street_1"),
    username = username,
    access_token = access_token
  )

  tileset_create_tileset(
    tileset = paste0(inst_prefix, "_street_2"),
    recipe = recipe_street_2,
    username = username,
    access_token = access_token
  )
  tileset_publish_tileset(
    tileset = paste0(inst_prefix, "_street_2"),
    username = username,
    access_token = access_token
  )

  tileset_create_tileset(
    tileset = paste0(inst_prefix, "_street_3"),
    recipe = recipe_street_3,
    username = username,
    access_token = access_token
  )
  tileset_publish_tileset(
    tileset = paste0(inst_prefix, "_street_3"),
    username = username,
    access_token = access_token
  )
}

#' Create a tileset from a stories dataframe
#'
#' @param stories <`dataframe`>A dataframe with columns "ID", "name_id", "lon",
#' "lat".
#' @param inst_prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
stories_create_tileset <- function(stories, inst_prefix, username, access_token) {
  # Delete tileset and source
  tileset_delete_tileset(
    id = paste0(inst_prefix, "_stories"),
    username = username,
    access_token = access_token
  )
  tileset_delete_tileset_source(
    id = paste0(inst_prefix, "_stories"),
    username = username,
    access_token = access_token
  )

  # Upload source
  stories <- sf::st_as_sf(stories, coords = c("lon", "lat"), crs = 4326)
  stories[c("ID", "name_id", "short_title", "preview_en", "preview_fr", "geometry")]
  names(stories)[2] <- "name"
  stories$ID <- as.character(stories$ID)

  tileset_upload_tile_source(
    df = stories,
    id = paste0(inst_prefix, "_stories"),
    username = username,
    access_token = access_token
  )

  # Create recipe
  stories_recipe <- tileset_create_recipe(
    layer_names = paste0(inst_prefix, "_stories"),
    source = paste0("mapbox://tileset-source/", username, "/", inst_prefix, "_stories"),
    minzoom = 3,
    maxzoom = 13,
    recipe_name = paste0(inst_prefix, "_stories")
  )

  # Create and publish
  tileset_create_tileset(
    tileset = paste0(inst_prefix, "_stories"),
    recipe = stories_recipe,
    username = username,
    access_token = access_token
  )
  tileset_publish_tileset(
    tileset = paste0(inst_prefix, "_stories"),
    username = username,
    access_token = access_token
  )
}

#' Upload NDVI Tilesets
#'
#' This function uploads NDVI tilesets for different grid scales and regions.
#' It handles the deletion of old tilesets, creation of new ones, and their publishing.
#' The function is designed to work with specific data structures and assumes
#' the presence of NDVI year-specific data.
#'
#' @param grids_dir <`character`> Directory containing grid files. Default is "dev/data/built/".
#' @param map_zoom_levels <`list`> List of zoom levels for different grid scales.
#' @param max_zoom <`list`> Maximum zoom levels for different grid scales.
#' Default values for scales grd600, grd300, grd120, grd60, and grd30.
#' @param regions <`data.frame` or `sf` object> Spatial data for different regions.
#' @param inst_prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#' @param ndvi_delta_breaks <`numeric vector`> Break points for categorizing NDVI deltas.
#' Default is c(-0.5, -0.1, -0.02, 0.02, 0.1, 0.5).
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return Invisibly returns NULL. The function's primary purpose is the side-effects
#' of uploading, creating, and publishing tilesets.
#' @export
tileset_upload_ndvi <- function(grids_dir = "dev/data/built/",
                                map_zoom_levels,
                                max_zoom = list(grd600 = 11, grd300 = 13, grd120 = 14,
                                                grd60 = 15, grd30 = 16),
                                regions,
                                inst_prefix,
                                username,
                                access_token,
                                ndvi_delta_breaks = c(-0.5, -0.1, -0.02, 0.02, 0.1, 0.5),
                                crs) {

  grids_size <- c(30, 60, 120, 300, 600)
  all_scales <- sapply(grids_size, \(x) {

    data_file <- sprintf("data/grd%s/ndvi.qs", x)
    if (data_file %in% list.files(sprintf("data/grd%s", x), full.names = TRUE)) {
      file <- sprintf("%sgrd%s.qs", grids_dir, x)
      geo <- qs::qread(file)
      data_file <- qs::qread(sprintf("data/grd%s/ndvi.qs", x))
      return(merge(geo, data_file, by = "ID") |> sf::st_transform(crs))
    }

    geo_chr <- sprintf("grd%s", x)
    conn <- db_connect_prod()
    geo <- db_try_disconnect(conn = conn,
                             fun = DBI::dbGetQuery(conn,
                                                   sprintf("SELECT * FROM %s.%s",
                                                           inst_prefix, geo_chr)))
    db_disconnect_prod(conn)
    geo$geometry <- sf::st_as_sfc(geo$geometry, EWKB = TRUE, crs = 4326)
    geo <- tibble::as_tibble(geo)
    geo <- sf::st_as_sf(geo)

    conn <- db_connect_prod()
    dat <- db_try_disconnect(conn = conn,
                             fun = DBI::dbGetQuery(conn,
                                                   sprintf("SELECT * FROM %s.%s_ndvi",
                                                           inst_prefix, geo_chr)))

    merge(geo, dat, by = "ID") |> sf::st_transform(crs)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Switch the geometry for digital
  all_scales <- lapply(all_scales, \(scale_df) {
    # If there is no digital geometry, return raw
    if (!"geometry_digital" %in% names(scale_df)) return(scale_df)

    # Switch the geometry to the digital one
    scale_df <- sf::st_drop_geometry(scale_df)
    names(scale_df)[names(scale_df) == "geometry_digital"] <- "geometry"
    sf::st_as_sf(scale_df)
  })
  names(all_scales) <- sprintf("grd%s", grids_size)

  # Region at the current crs
  regions <- lapply(regions, sf::st_transform, crs)

  # Reset
  mapply(function(region_name, reg_sf) {
    mapply(\(scale_name, scale_df) {
      name <- paste(inst_prefix, scale_name, region_name, sep = "_")

      tileset_delete_tileset_source(
        id = name,
        username = username,
        access_token = access_token
      )

      tileset_delete_tileset(
        id = name,
        username = username,
        access_token = access_token
      )
    }, names(all_scales), all_scales, SIMPLIFY = FALSE)
  }, names(regions), regions)

  # Tileset sources
  mapply(function(region_name, reg_sf) {
    mapply(function(scale_name, scale_df) {
      scale_n <- paste(inst_prefix, scale_name, region_name, sep = "_")
      df <- scale_df
      df <- sf::st_filter(df, reg_sf)

      vars_col <- grep("ndvi_\\d{4}$", names(df), value = TRUE)

      # Subset
      df <- df[, c("ID", vars_col)]
      if (scale_name == "grd300") df$ID_color <- df$ID

      cols <- names(df)
      cols <- cols[grepl("^ndvi_", cols)]
      cols <- gsub("ndvi_", "", cols)

      # Sort years in descending order (since you want lowest to highest in pairs)
      cols_sorted <- sort(cols, decreasing = TRUE)
      combinations <- combn(cols_sorted, 2)
      pairs <- data.frame(year1 = combinations[2,], year2 = combinations[1,])


      # Add the delta column
      for (i in seq_len(nrow(pairs))) {
        pair <- pairs[i, ]

        v_1 <- paste0("ndvi_", pair[[1]])
        v_2 <- paste0("ndvi_", pair[[2]])

        var <- sprintf("ndvi_delta_%s_%s", pair[[1]], pair[[2]])

        df[[var]] <- (df[[v_2]] - df[[v_1]]) / df[[v_2]]

        # Add the `group` for the map colouring
        df$var_left_q5 <- 5
        df$var_left_q5[df[[var]] < ndvi_delta_breaks[5]] <- 4
        df$var_left_q5[df[[var]] < ndvi_delta_breaks[4]] <- 3
        df$var_left_q5[df[[var]] < ndvi_delta_breaks[3]] <- 2
        df$var_left_q5[df[[var]] < ndvi_delta_breaks[2]] <- 1
        df$var_left_q5[is.na(df[[var]])] <- NA
        df[[var]] <- as.character(df$var_left_q5)
        df$var_left_q5 <- NULL

      }


      # Add the group to all the ndvi columns
      for (col in cols) {
        var <- paste0("ndvi_", col)

        df$col <- 5
        df$col[df[[var]] < 0.8] <- 4
        df$col[df[[var]] < 0.6] <- 3
        df$col[df[[var]] < 0.4] <- 2
        df$col[df[[var]] < 0.2] <- 1
        df$col[is.na(df[[var]])] <- NA

        df[[var]] <- as.character(df$col)
        df$col <- NULL

      }

      df <- sf::st_transform(df, 4326)
      tileset_upload_tile_source_large(
        id = scale_n,
        df = df,
        total_batches = if (scale_name == "grd30") 1000 else 100,
        username = username,
        access_token = access_token
      )
    }, names(all_scales), all_scales)
  }, names(regions), regions)

  # Create recipe, create tileset and publish
  maxzooms <- tibble::tibble(scale = names(max_zoom),
                             maxzoom = unlist(max_zoom))

  all_recipes <-
    mapply(function(region_name, reg_sf) {
      mapply(\(scale_name, scale_df) {

        source_names <- paste(inst_prefix, scale_name, region_name, sep = "_")
        sources <- paste0("mapbox://tileset-source/", username, "/", source_names)
        names(sources) <- source_names
        minzooms <- 3
        names(minzooms) <- source_names

        default_maxzoom <- maxzooms$maxzoom[maxzooms$scale == scale_name]
        new_maxzoom <- max(default_maxzoom, 14)
        maxzooms_ <- new_maxzoom
        names(maxzooms_) <- source_names
        layer_sizes <- 2500
        names(layer_sizes) <- source_names

        recipe <- tileset_create_recipe(
          layer_names = source_names,
          source = sources,
          minzoom = minzooms,
          maxzoom = maxzooms_,
          recipe_name = source_names,
          layer_size = layer_sizes
        )

        tileset_create_tileset(source_names,
                               recipe = recipe,
                               username = username,
                               access_token = access_token
        )

        tileset_publish_tileset(source_names,
                                username = username,
                                access_token = access_token
        )
      }, names(all_scales), all_scales, SIMPLIFY = FALSE)
    }, names(regions), regions)

  # Function to calculate on autozoom when the scale starts and when it ends
  calculate_zoom_levels <- function(zoom_levels) {
    # Initialize the output tibble
    result <- tibble::tibble(
      scale = character(), min_zoom = integer(),
      max_zoom = integer()
    )

    # Loop through the named numeric vector
    for (i in seq_along(zoom_levels)) {
      scale_name <- names(zoom_levels)[i]
      zoom_value <- zoom_levels[i]

      # Calculate min zoom
      min_zoom <- ifelse(i == 1, 0, result$max_zoom[i - 1] + 1)

      # Calculate max zoom
      max_zoom <-
        if (length(zoom_levels) == 1) {
          10
        } else {
          ifelse(i == length(zoom_levels), zoom_value, zoom_levels[i + 1] - 1)
        }


      # Add the min and max zoom to the result tibble
      result <-
        tibble::add_row(result,
                        scale = scale_name, min_zoom = min_zoom,
                        max_zoom = max_zoom
        )
    }

    return(result)
  }

  auto_zoom_recipes <-
    mapply(function(region_name, reg_sf) {

      zoom_levels <- map_zoom_levels$mzl_grd600_grd300_grd120_grd60_grd30

      az_name <- paste(inst_prefix, "ndvi_autozoom", region_name, sep = "_")
      scale_names <- paste(inst_prefix, names(zoom_levels), region_name, sep = "_")


      sources <- stats::setNames(paste0(
        "mapbox://tileset-source/", username, "/",
        scale_names
      ), scale_names)

      zooms <- calculate_zoom_levels(zoom_levels)
      minzooms <- zooms$min_zoom
      maxzooms <- zooms$max_zoom
      maxzooms[length(maxzooms)] <- 16
      names(minzooms) <- scale_names
      names(maxzooms) <- scale_names

      layer_sizes <-
        stats::setNames(rep(2500, length(scale_names)), scale_names)

      recipe <-
        tileset_create_recipe(
          layer_names = scale_names,
          source = sources,
          minzoom = minzooms,
          maxzoom = maxzooms,
          layer_size = layer_sizes,
          recipe_name = az_name
        )

      # Reset
      tileset_delete_tileset_source(
        id = az_name,
        username = username,
        access_token = access_token
      )
      tileset_delete_tileset(
        id = az_name,
        username = username,
        access_token = access_token
      )
      # Give some time so the deletion is completed
      Sys.sleep(5)

      # New tileset
      tileset_create_tileset(az_name,
                             recipe = recipe,
                             username = username,
                             access_token = access_token
      )
      tileset_publish_tileset(az_name,
                              username = username,
                              access_token = access_token
      )
    }, names(regions), regions)


  return(invisible(NULL))
}
