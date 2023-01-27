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
  if (sf::st_crs(df)$input != "EPSG:4326") stop("`df` must have the 4326 crs.")

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
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing when succesfull.
#' @export
tileset_upload_tile_source_large <-
  function(df, id, username, access_token) {
    iter_size <- ceiling(nrow(df) / 100)

    to_process_list <-
      lapply(1:100, \(x) {
        df[(((x - 1) * iter_size + 1):(x * iter_size)), ] |>
          geojsonsf::sf_geojson() |>
          paste0(collapse = " ") |>
          geojson::featurecollection()
      })

    # Iteratively post files to tile source
    tmp <- tempfile(fileext = ".json")
    tmp_list <- lapply(1:10, \(x) tempfile(fileext = ".json"))

    lapply(1:10, function(x) {
      to_process <- to_process_list[((x - 1) * 10 + 1):(x * 10)]
      mapply(geojson::ndgeo_write, to_process, tmp_list)

      # Concatenate geoJSONs
      if (Sys.info()[["sysname"]] == "Windows") {
        out <- paste0("type ", paste(tmp_list, collapse = " "), " > ", tmp)
        shell(out)
      } else {
        out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
        system(out)
      }

      # Upload to MTS
      out <- paste0(
        'curl -X POST "https://api.mapbox.com/tilesets/v1/sources/',
        username, "/", id, "?access_token=", access_token,
        '" -F file=@', tmp,
        ' --header "Content-Type: multipart/form-data"'
      )
      system(out)
    })
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
    times = 3,
    pause_min = 30
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
                                  layer_size = NULL, simp_zoom = NULL, simp_value = NULL,
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
#' @param prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
tileset_upload_all <- function(all_scales, map_zoom_levels,
                               prefix, username, access_token) {
  tn <- function(geo, scale_name) paste(prefix, geo, scale_name, sep = "_")

  # All tables
  all_tables <- reconstruct_all_tables(all_scales)

  # Arrange the data
  all_scales <- map_over_scales(
    all_scales = all_scales,
    fun = \(scale_df = scale_df, ...) {
      scale_df[, grepl("ID$", names(scale_df))]
    }
  )

  # Reset
  mapply(\(geo, scales) {
    sapply(scales, \(scale) {
      tileset_delete_tileset_source(
        id = tn(geo, scale),
        username = username,
        access_token = access_token
      )

      tileset_delete_tileset(
        id = tn(geo, scale),
        username = username,
        access_token = access_token
      )
    })
  }, names(all_tables), all_tables, SIMPLIFY = FALSE)

  # Tileset sources
  mapply(function(scales, geo) {
    lapply(scales, function(scale) {
      geo_scale <- tn(geo, scale)

      if (scale == "building") {
        building_to_process <- all_scales[[geo]][[scale]]
        building_to_process <- building_to_process[, c("ID", "DA_ID")]
        names(building_to_process) <- c("ID", "ID_color", "geometry")
        building_to_process <- sf::st_transform(building_to_process, 4326)

        tileset_upload_tile_source_large(
          id = geo_scale,
          df = building_to_process,
          username = username,
          access_token = access_token
        )
      } else {
        df <- all_scales[[geo]][[scale]]
        df[c("ID")]
        df$ID_color <- df$ID
        df <- sf::st_transform(df, 4326)
        df <- sf::st_set_agr(df, "constant")

        tileset_upload_tile_source(df,
          id = geo_scale,
          username = username,
          access_token = access_token
        )
      }
    })
  }, all_tables, names(all_tables))


  # Create recipe, create tileset and publish
  maxzooms <-
    tibble::tibble(
      scale = c("first_level", "CT", "DA", "DB", "building"),
      maxzoom = c(11, 12, 13, 14, 16)
    )

  all_recipes <-
    mapply(\(scales, geo) {
      mapply(function(scale, level) {
        scale_for_dict <- if (level == 1) "first_level" else scale
        name <- tn(geo, scale)

        recipe <-
          tileset_create_recipe(
            layer_names = name,
            source = paste0("mapbox://tileset-source/", username, "/", name),
            minzoom = 3,
            maxzoom = maxzooms$maxzoom[maxzooms$scale == scale_for_dict],
            layer_size = 2500,
            recipe_name = name
          )

        tileset_create_tileset(name,
          recipe = recipe,
          username = username,
          access_token = access_token
        )

        tileset_publish_tileset(name,
          username = username,
          access_token = access_token
        )
      }, scales, seq_along(scales), SIMPLIFY = FALSE)
    }, all_tables, names(all_tables), SIMPLIFY = FALSE)

  auto_zoom_recipes <-
    mapply(\(geo, zoom_levels) {
      mapply(\(mzl_name, mzl) {
        suffix <- gsub(paste0(".*_", geo), "", mzl_name)
        suffix <- if (grepl("_", suffix)) suffix else ""
        name <- tn(geo, scale_name = paste0("auto_zoom", suffix))
        scale_names <- tn(geo, names(mzl))

        sources <- stats::setNames(paste0(
          "mapbox://tileset-source/", username, "/",
          scale_names
        ), scale_names)

        minz_fun <- function(x) {
          if (x == 0) {
            return(2)
          } else {
            sum(x, 0.5)
          }
        }
        minzooms <- sapply(mzl, minz_fun)
        names(minzooms) <- scale_names

        maxz_fun <- function(x) {
          if (x == 0) {
            return(10)
          } else if (x == 15.5) {
            return(16)
          } else {
            return(sum(x, 1.5))
          }
        }
        maxzooms <- sapply(mzl, maxz_fun)
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
            recipe_name = name
          )

        # Reset
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
        # New tileset
        tileset_create_tileset(name,
          recipe = recipe,
          username = username,
          access_token = access_token
        )
        tileset_publish_tileset(name,
          username = username,
          access_token = access_token
        )
      }, names(zoom_levels), zoom_levels, SIMPLIFY = FALSE)
    }, names(map_zoom_levels), map_zoom_levels, SIMPLIFY = FALSE)

  return(invisible(NULL))
}

#' TKTK NOT EXPORT, NEEDS REWORK Upload a custom auto_zoom
#'
#' In cases where a new auto zoom needs to be created. While the normal auto-zoom
#' would usually be: CSD - CT - DA - building, for some modules missing data,
#' we might want to stop the auto zoom at CT.
#'
#' @param smaller_limit_scale <`character`> The scale which is the smaller limit
#' of the new auto-zoom, e.g. `CT` for an auto-zoom that stops at census tracts.
#' @param auto_zoom_suffix <`character`> To have a unique identifier to the new
#' auto-zoom tileset. If the smaller limit of the new auto-zoom is `CT`, we
#' could choose `max_CT`.
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`. The function will filter in all the scales
#' containing the `smaller_limit` scale.
#' @param prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
tileset_upload_custom_auto_zoom <- function(smaller_limit_scale,
                                            auto_zoom_suffix,
                                            all_scales, prefix,
                                            username, access_token) {
  # tn <- function(geo, scale_name) paste(prefix, geo, scale_name, sep = "_")
  #
  # # All combinations
  # all_tables <- reconstruct_all_tables(all_scales)
  # all_tables <- all_tables[sapply(all_tables, \(x) smaller_limit_scale %in% x)]
  # all_tables <- lapply(all_tables, \(x) x[1:which(x == smaller_limit_scale)])
  #
  # combinations <-
  #   mapply(\(scales, geo) {
  #     if (length(scales) > 1) {
  #       z <- list(auto_zoom = scales)
  #       names(z) <- paste0("auto_zoom_", auto_zoom_suffix)
  #       z
  #     }
  #   }, all_tables, names(all_tables), SIMPLIFY = FALSE)
  #
  # # Reset
  # map_over_scales(all_scales = combinations,
  #                 fun = \(geo = geo, scale_name = scale_name, ...) {
  #                   tileset_delete_tileset_source(id = tn(geo, scale_name),
  #                                                 username = username,
  #                                                 access_token = access_token)
  #                 })
  #
  # map_over_scales(all_scales = combinations,
  #                 fun = \(geo = geo, scale_name = scale_name, ...) {
  #                   tileset_delete_tileset(id = tn(geo, scale_name),
  #                                          username = username,
  #                                          access_token = access_token)
  #                 })
  #
  #
  # # Tileset recipes
  # first_level_recipe_fun <- function(name, scales) {
  #   tileset_create_recipe(
  #     layer_names = name,
  #     source = paste0("mapbox://tileset-source/", username, "/", name),
  #     minzoom = 3,
  #     maxzoom = 11,
  #     simp_zoom = 11,
  #     layer_size = 2500,
  #     recipe_name = name)
  # }
  #
  # CT_recipe_fun <- function(name, scales) {
  #   tileset_create_recipe(
  #     layer_names = name,
  #     source = paste0("mapbox://tileset-source/", username, "/", name),
  #     minzoom = 3,
  #     maxzoom = 12,
  #     simp_zoom = 12,
  #     layer_size = 2500,
  #     recipe_name = name)
  # }
  #
  # DA_recipe_fun <- function(name, scales) {
  #   tileset_create_recipe(
  #     layer_names = name,
  #     source = paste0("mapbox://tileset-source/", username, "/", name),
  #     minzoom = 3,
  #     maxzoom = 13,
  #     simp_zoom = 13,
  #     layer_size = 2500,
  #     recipe_name = name)
  # }
  #
  # DB_recipe_fun <- function(name, scales) {
  #   tileset_create_recipe(
  #     layer_names = name,
  #     source = paste0("mapbox://tileset-source/", username, "/", name),
  #     minzoom = 3,
  #     maxzoom = 14,
  #     simp_zoom = 14,
  #     layer_size = 2500,
  #     recipe_name = name)
  # }
  #
  # grid_recipe_fun <- function(name, scales) {
  #   tileset_create_recipe(
  #     layer_names = name,
  #     source = paste0("mapbox://tileset-source/", username, "/", name),
  #     minzoom = 3,
  #     maxzoom = 13,
  #     simp_zoom = 13,
  #     layer_size = 2500,
  #     recipe_name = name)
  # }
  #
  # building_recipe_fun <- function(name, scales) {
  #   tileset_create_recipe(
  #     layer_names = name,
  #     source = paste0("mapbox://tileset-source/", username, "/", name),
  #     minzoom = 3,
  #     maxzoom = 16,
  #     layer_size = 2500,
  #     recipe_name = name)
  # }
  #
  # auto_zoom_recipe_fun <- function(name, scales) {
  #   sources <- stats::setNames(paste0("mapbox://tileset-source/", username, "/",
  #                                     scales),
  #                              scales)
  #   minzooms <-
  #     sapply(scales, \(x) {
  #       if (grepl("_CT$", x)) return(11)
  #       if (grepl("_DA$", x)) return(13)
  #       if (grepl("_DB$", x)) return(16)
  #       if (grepl("_building$", x)) return(16)
  #       # For first level
  #       return(2)
  #     }, simplify = FALSE, USE.NAMES = TRUE)
  #
  #   maxzooms <-
  #     sapply(scales, \(x) {
  #       if (grepl("_CT$", x)) return(12)
  #       if (grepl("_DA$", x)) return(15)
  #       if (grepl("_DB$", x)) return(16)
  #       if (grepl("_building$", x)) return(16)
  #       # For first level
  #       return(10)
  #     }, simplify = FALSE, USE.NAMES = TRUE)
  #
  #
  #   layer_sizes <-
  #     stats::setNames(rep(NA, length(scales)), scales)
  #
  #   tileset_create_recipe(
  #     layer_names = scales,
  #     source = sources,
  #     minzoom = minzooms,
  #     maxzoom = maxzooms,
  #     layer_size = layer_sizes,
  #     recipe_name = name)
  # }
  #
  # all_recipes <-
  #   mapply(\(scales, geo) {
  #     mapply(function(scale, level) {
  #
  #       scale_fun <- if (level == 1) "first_level" else scale
  #       function_name <- paste0(scale_fun, "_recipe_fun")
  #       if (length(scale) != 1) function_name <- "auto_zoom_recipe_fun"
  #
  #       scale_name <- names(combinations[[geo]][level])
  #
  #       do.call(function_name, list(name = tn(geo, scale_name),
  #                                   scales = tn(geo, scale)))
  #     }, scales, seq_along(scales), SIMPLIFY = FALSE)
  #   }, combinations, names(combinations), SIMPLIFY = FALSE)
  #
  #
  # # Create tilesets
  # created <-
  #   mapply(\(recipes, geo) {
  #     mapply(function(recipe, level) {
  #       tileset_create_tileset(tn(geo, names(recipes)[level]), recipe = recipe,
  #                              username = username, access_token = access_token)
  #     }, recipes, seq_along(recipes))
  #   }, all_recipes, names(all_recipes))
  #
  # lapply(created, \(x) {
  #   lapply(x, \(y) {
  #     if (!stringr::str_detect(y, "^Success"))
  #       stop(paste0("One or more tileset hasn't succesfully been created.\n\n",
  #                   created))
  #   })})
  #
  #
  # # Publish tilesets
  # published <-
  #   mapply(\(recipes, geo) {
  #     mapply(function(recipe, level) {
  #       tileset_publish_tileset(tn(geo, names(recipes)[level]),
  #                               username = username, access_token = access_token)
  #     }, recipes, seq_along(recipes))
  #   }, all_recipes, names(all_recipes))
  #
  # lapply(published[1,], \(x) {
  #   if (!stringr::str_detect(x, "^Processing"))
  #     stop(paste0("One or more tileset hasn't succesfully published.\n\n",
  #                 created))
  # })
  #
  # return(invisible(NULL))
}

#' Create CSD labels tileset
#'
#' @param CSD_table <`sf data.frame`> The data.frame containing the most filled
#' set of CSDs. Their labels will be the `name` column, and the `population`
#' column will be used to weight which label to display first.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
tileset_labels <- function(CSD_table, crs, prefix, username, access_token) {
  ## Error catch
  if (all((!c("name", "population", "geometry") %in% names(CSD_table)))) {
    stop("One of `name`, `population` or `geometry` column is missing.")
  }

  name <- paste(prefix, "CSD_label", sep = "_")

  # Calculate centroid using a projection
  CSD_table <- sf::st_transform(CSD_table, crs)

  CSD_table <- CSD_table[c("name", "population", "geometry")]
  CSD_table$name <- stringi::stri_trans_general(CSD_table$name, id = "Latin-ASCII")
  CSD_table <- sf::st_set_agr(CSD_table, "constant")
  CSD_table <- sf::st_centroid(CSD_table)

  CSD_table <- sf::st_transform(CSD_table, 4326)
  tileset_upload_tile_source(
    df = CSD_table, id = name,
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
}





#' Create and publish street and park tilesets
#'
#' @param master_polygon <`sf data.frame`> Unioned multipolygon of all the
#' geometries for which data has been gathered.
#' @param street <`sf data.frame`> All the streets in the zone under study.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
tileset_streets <- function(master_polygon, street, crs, prefix, username,
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

  # Upload tile_source
  tileset_upload_tile_source(
    df = street_1,
    id = paste0(prefix, "_street_1"),
    username = username,
    access_token = access_token
  )
  tileset_upload_tile_source(
    df = street_2,
    id = paste0(prefix, "_street_2"),
    username = username,
    access_token = access_token
  )
  tileset_upload_tile_source_large(
    df = street_3,
    id = paste0(prefix, "_street_3"),
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

  tileset_upload_tile_source(
    df = park,
    id = paste0(prefix, "_park"),
    username = username,
    access_token = access_token
  )


  # Create recipes
  # Street 1
  recipe_street_1 <- paste0('
{
  "recipe": {
    "version": 1,
    "layers": {
      "street": {
        "source": "mapbox://tileset-source/', username, "/", prefix, '_street_1",
        "minzoom": 12,
        "maxzoom": 16,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 16 ], 1, 4
          ]
        }
      }
    }
  },
  "name": "', prefix, '_street_1"
}
')

  # Street 2
  recipe_street_2 <- paste0('
{
  "recipe": {
    "version": 1,
    "layers": {
      "street": {
        "source": "mapbox://tileset-source/', username, "/", prefix, '_street_2",
        "minzoom": 13,
        "maxzoom": 16,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 16 ], 1, 4
          ]
        }
      }
    }
  },
  "name": "', prefix, '_street_2"
}
')

  # Street 3
  recipe_street_3 <- paste0('
{
  "recipe": {
    "version": 1,
    "layers": {
      "street": {
        "source": "mapbox://tileset-source/', username, "/", prefix, '_street_3",
        "minzoom": 14,
        "maxzoom": 16,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 16 ], 1, 4
          ]
        }
      },
      "park": {
        "source": "mapbox://tileset-source/', username, "/", prefix, '_park",
        "minzoom": 14,
        "maxzoom": 16
      }
    }
  },
  "name": "', prefix, '_street_3"
}
')


  # Publish tileset

  tileset_create_tileset(
    tileset = paste0(prefix, "_street_1"),
    recipe = recipe_street_1,
    username = username,
    access_token = access_token
  )
  tileset_publish_tileset(
    tileset = paste0(prefix, "_street_1"),
    username = username,
    access_token = access_token
  )

  tileset_create_tileset(
    tileset = paste0(prefix, "_street_2"),
    recipe = recipe_street_2,
    username = username,
    access_token = access_token
  )
  tileset_publish_tileset(
    tileset = paste0(prefix, "_street_2"),
    username = username,
    access_token = access_token
  )

  tileset_create_tileset(
    tileset = paste0(prefix, "_street_3"),
    recipe = recipe_street_3,
    username = username,
    access_token = access_token
  )
  tileset_publish_tileset(
    tileset = paste0(prefix, "_street_3"),
    username = username,
    access_token = access_token
  )
}

#' Create a tileset from a stories dataframe
#'
#' @param stories <`dataframe`>A dataframe with columns "ID", "name_id", "lon",
#' "lat".
#' @param prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param username <`character`> Mapbox account username.
#' @param access_token <`character`> Private access token to the Mapbox account.
#'
#' @return Returns nothing if succeeds. Tilesets are created and published and
#' ready to be used.
#' @export
stories_create_tileset <- function(stories, prefix, username, access_token) {

  # Delete tileset and source
  tileset_delete_tileset(id = paste0(prefix, "_stories"),
                         username = username,
                         access_token = access_token)
  tileset_delete_tileset_source(id = paste0(prefix, "_stories"),
                                username = username,
                                access_token = access_token)

  # Upload source
  stories <- sf::st_as_sf(stories, coords = c("lon", "lat"), crs = 4326)
  stories <- stories[c("ID", "name_id", "geometry")]
  tileset_upload_tile_source(df = stories,
                             id = paste0(prefix, "_stories"),
                             username = username,
                             access_token = access_token)

  # Create recipe
  stories_recipe <-
    tileset_create_recipe(
      layer_names = "stories-stories",
      source = "mapbox://tileset-source/sus-mcgill/stories-stories",
      minzoom = 3,
      maxzoom = 13,
      recipe_name = paste0(prefix, "_stories"))

  # Create and publish
  tileset_create_tileset(tileset = paste0(prefix, "_stories"),
                         recipe = stories_recipe,
                         username = username,
                         access_token = access_token)
  tileset_publish_tileset(tileset = paste0(prefix, "_stories"),
                          username = username,
                          access_token = access_token)

}
