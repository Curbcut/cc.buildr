#' Produce the place explorer main card
#'
#' @param scales <`list`> Lists of spatial features dataframes with regions and
#' scales filled with at minimum census and alp data. Usually is
#' `scales_variables_modules$scales`.
#' @param DA_table <`sf data.frame`> Tables containing all the dissemination areas
#' of the zone under study. Usually is `census_scales$DA`
#' @param region_DA_IDs <`character vector`> Vector of all the dissemination area's
#' identifiers in the zone under study. Usually is `census_scales$DA$ID`
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param regions_dictionary <`data.frame`> The regions dictionary built using
#' \code{\link[cc.buildr]{regions_dictionary}}. Will be used to filter out scales
#' for which data should not be calculated.
#'
#' @return Lists containing all the main card indicators. Sub lists of regions
#' and scales for data values.
#' @export
placeex_main_card_data <- function(scales, DA_table, region_DA_IDs, crs,
                                   regions_dictionary) {
  # Init --------------------------------------------------------------------

  dict <- tibble::tibble(
    name = character(),
    title = character(),
    xaxis_title = character(),
    bs_icon = character(),
    date = numeric(),
    percent = logical(),
    high_is_good = logical(),
    val_digit = 0,
    text = character(),
    link_module = character(),
    link_dropdown = character(),
    link_var_code = character()
  )
  data <- list()

  DA_table <- DA_table["ID"]
  names(DA_table)[1] <- "DA_ID"
  current_census_year <- gsub("CA", "20", cc.buildr::current_census)
  regions <- regions_dictionary$region[regions_dictionary$pickable]
  scales <- scales[names(scales) %in% regions]

  # Which df can we show PE for?
  avail_df <- sapply(names(scales), \(reg) {
    scales_name <- names(scales[[reg]])
    scales_name <- scales_name[!scales_name %in% c("street", "building")]
    tibble::tibble(
      region = rep(reg, length(scales_name)),
      scale = scales_name,
      df = sprintf("%s_%s", reg, scales_name)
    )
  }, simplify = FALSE, USE.NAMES = TRUE)
  avail_df <- Reduce(rbind, avail_df)

  # Common functions --------------------------------------------------------

  mc_df_format <- function(df) {
    if (ncol(df) > 2) {
      stop("Only two columns needed. ID and variable.")
    }
    # Rename the variable to generic
    names(df)[2] <- "var"
    df$percentile <- {
      (rank(df$var, ties.method = "min", na.last = "keep") - 1) /
        (sum(!is.na(df$var)) - 1)
    }
    df$rank <- rank(df$var)
    df$var[is.na(df$var)] <- NA
    df$var[is.infinite(df$var)] <- NA
    return(df)
  }


  # NO2 ---------------------------------------------------------------------

  no2_dict <-
    tibble::tibble(
      name = "no2",
      title = "Air pollution",
      xaxis_title = "Nitrogen dioxide (ppb)",
      bs_icon = "wind",
      date = "2016",
      percent = FALSE,
      high_is_good = FALSE,
      val_digit = 1,
      text = paste0(
        "{data_rank} in terms of level of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>NO2</a> pollution. ",
        "{higher_than_threshold}(NO2 = {pretty_data_var}, ",
        "data from {data_date})"
      ),
      link_module = NA,
      link_dropdown = NA,
      link_var_code = NA
    )
  dict <- rbind(dict, no2_dict)

  NO2 <- cc.data::db_read_data("no2",
    column_to_select = "DA_ID",
    IDs = region_DA_IDs, crs = crs
  )
  NO2 <- merge(NO2, DA_table, all = TRUE)

  # Interpolate to all possible region and scales
  NO2_scales <- interpolate_from_census_geo(
    data = NO2,
    all_scales = scales,
    crs = crs,
    average_vars = "NO2",
    base_scale = "DA",
    weight_by = "area"
  )

  # Format correctly
  data$no2 <- map_over_scales(
    all_scales = NO2_scales$scales,
    fun = \(scale_df = scale_df, ...) {
      if (!"NO2" %in% names(scale_df)) {
        return(NULL)
      }
      out <- scale_df[c("ID", "NO2")]
      out <- sf::st_drop_geometry(out)
      out <- mc_df_format(out)
      return(out)
    }
  )
  # Remove empty lists
  data$no2 <- lapply(data$no2, \(x) {
    x[!sapply(x, is.null)]
  })
  data$no2 <- data$no2[sapply(data$no2, length) > 0]


  # NDVI --------------------------------------------------------------------

  ndvi_dict <-
    tibble::tibble(
      name = "ndvi",
      title = "Vegetation",
      xaxis_title = "Normalized Difference Vegetation Index",
      bs_icon = "tree",
      date = "2022",
      percent = TRUE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0(
        "{data_rank} in terms of vegetation (<a href='https://canue.ca/wp-c",
        "ontent/uploads/2018/11/CANUE-Metadata-NDVI-Landsat-Annual.pdf' ",
        "target='_blank'>NDVI</a> = {pretty_data_var}, data ",
        "from {data_date})"
      ),
      link_module = NA,
      link_dropdown = NA,
      link_var_code = NA
    )
  dict <- rbind(dict, ndvi_dict)

  # Format correctly
  data$ndvi <- map_over_scales(
    all_scales = scales,
    fun = \(scale_df = scale_df, ...) {
      if (!"ndvi_2022" %in% names(scale_df)) {
        return(NULL)
      }
      out <- scale_df[c("ID", "ndvi_2022")]
      out <- sf::st_drop_geometry(out)
      out <- mc_df_format(out)
      return(out)
    }
  )
  # Remove empty lists
  data$ndvi <- lapply(data$ndvi, \(x) {
    x[!sapply(x, is.null)]
  })
  data$ndvi <- data$ndvi[sapply(data$ndvi, length) > 0]


  # Sustainable transport ---------------------------------------------------

  sust_dict <-
    tibble::tibble(
      name = "sust",
      title = "Sustainable transport",
      xaxis_title = "Walk, cycle or transit to work (%)",
      bs_icon = "bus-front",
      date = current_census_year,
      percent = TRUE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0(
        "{pretty_data_var} of residents use public transit, ",
        "walk or bicycle to get to work. {data_rank}. ",
        "(Data from {data_date})"
      ),
      link_module = NA,
      link_dropdown = NA,
      link_var_code = NA
    )
  dict <- rbind(dict, sust_dict)


  vars <- paste0(c("trans_walk_or_bike_", "trans_transit_"), current_census_year)

  # Format correctly
  data$sust <- map_over_scales(
    all_scales = scales,
    fun = \(scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(NULL)
      }
      out <- scale_df[c("ID", vars)]
      out$var <- out[[vars[1]]] + out[[vars[2]]]
      out <- out[c("ID", "var")]
      out <- sf::st_drop_geometry(out)
      out <- mc_df_format(out)
      return(out)
    }
  )
  # Remove empty lists
  data$sust <- lapply(data$sust, \(x) {
    x[!sapply(x, is.null)]
  })
  data$sust <- data$sust[sapply(data$sust, length) > 0]


  # Housing -----------------------------------------------------------------

  singled_dict <-
    tibble::tibble(
      name = "singled",
      title = "Housing",
      xaxis_title = "Single-detached housing (%)",
      bs_icon = "houses",
      date = current_census_year,
      percent = TRUE,
      high_is_good = FALSE,
      val_digit = 0,
      text = paste0(
        "{pretty_data_var} of occupied dwellings are single-det",
        "ached houses. {data_rank}. (Data from {data_date})"
      ),
      link_module = "housing",
      link_dropdown = "housing-pimnd",
      link_var_code = "housing_single_detached"
    )
  dict <- rbind(dict, singled_dict)


  vars <- paste0("housing_single_detached_", current_census_year)

  # Format correctly
  data$singled <- map_over_scales(
    all_scales = scales,
    fun = \(scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(NULL)
      }
      out <- scale_df[c("ID", vars)]
      # out <- out[c("ID", "var")]
      out <- sf::st_drop_geometry(out)
      out <- mc_df_format(out)
      return(out)
    }
  )
  # Remove empty lists
  data$singled <- lapply(data$singled, \(x) {
    x[!sapply(x, is.null)]
  })
  data$singled <- data$singled[sapply(data$singled, length) > 0]


  # Active living -----------------------------------------------------------

  activel_dict <-
    tibble::tibble(
      name = "activel",
      title = "Active living",
      xaxis_title = "Active living potential index",
      bs_icon = "activity",
      date = current_census_year,
      percent = FALSE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0(
        "{data_rank} in terms of active living potential. (Data from ",
        "{data_date})"
      ),
      link_module = "alp",
      link_dropdown = "alp-pimnd",
      link_var_code = "alp"
    )
  dict <- rbind(dict, activel_dict)


  vars <- paste0("alp_", current_census_year)

  # Format correctly
  data$activel <- map_over_scales(
    all_scales = scales,
    fun = \(scale_df = scale_df, ...) {
      if (all(!vars %in% names(scale_df))) {
        return(NULL)
      }
      out <- scale_df[c("ID", vars)]
      # out <- out[c("ID", "var")]
      out <- sf::st_drop_geometry(out)
      out <- mc_df_format(out)
      return(out)
    }
  )
  # Remove empty lists
  data$activel <- lapply(data$activel, \(x) {
    x[!sapply(x, is.null)]
  })
  data$activel <- data$activel[sapply(data$activel, length) > 0]


  # Return ------------------------------------------------------------------

  return(list(
    main_card_dict = dict,
    main_card_data = data,
    avail_df = avail_df
  ))
}

#' Pre-process all the possible Rmds
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param pe_main_card_data <`list`> Data and dictionary necessary to knit the
#' rmds. The output of \code{\link[cc.buildr]{placeex_main_card_data}}.
#' @param regions_dictionary <`data.frame`> The regions dictionary built using
#' \code{\link[cc.buildr]{regions_dictionary}}. Will be used to filter out scales
#' for which data should not be calculated.
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link[cc.buildr]{build_census_scales}}
#' @param lang <`character`> If the Curbcut instance is bilingual, we can use
#' `c("en", "fr")` and HTML will be built for both languages. Defaults to `"en"`.
#' @param tileset_prefix <`character`> Prefix attached to every tile source and
#' created and published tileset. Should correspond to the Curbcut city, e.g. `mtl`.
#' @param mapbox_username <`character`> Mapbox account username.
#' @param rev_geocode_from_localhost <`character`> Use a local instance of nominatim
#' for reverse geocoding, using \code{\link[cc.data]{rev_geocode_localhost}}. If
#' TRUE, there must be a local instance of Nominatim runnin on localhost port 8080.
#' If set to FALSE, the reverse geolocation will be done using
#' \code{\link[cc.data]{rev_geocode_OSM}}, which uses a remote server (photon.komoot.io).
#' @param check_bslib_version <`logical`> The `bslib` library do not allow to
#' style individually each tab in a navigation bar. As we want to color each
#' tab depending on how much of an outlier the zone is in each theme, we have to
#' add `class` arguments to the navigation bar's list creation. This version can
#' be found at `devtools::install_github('bdbmax/bslib')`. Defaults to `TRUE.`
#' Set `FALSE` to continue with the version of `bslib` on your system.
#' @param overwrite <`logical`> Should the .html files be overwritten? Defaults
#' to `TRUE`.
#' @param skip_scales <`character vector`> Scales for which place explorer documents
#' should not be created.
#'
#' @return Returns nothing if successful. All place explorer possibilities are
#' saved in the `out_folder`.
#' @export
placeex_main_card_rmd <- function(scales_variables_modules,
                                  pe_main_card_data,
                                  regions_dictionary,
                                  scales_dictionary,
                                  lang = "en",
                                  tileset_prefix,
                                  mapbox_username = "curbcut",
                                  rev_geocode_from_localhost = TRUE,
                                  check_bslib_version = TRUE,
                                  overwrite = TRUE,
                                  skip_scales = c("building", "street")) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package \"rmarkdown\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (check_bslib_version) {
    if (utils::packageVersion("bslib") != "9.9.9.9999") {
      stop(paste0(
        "Wrong version of `bslib`. Place explorer tabs won't be ",
        "colored according to how much of an outlier the zone is for ",
        "every theme. To ignore, set `check_bslib_version = FALSE`. ",
        "To build with colored tabs, install ",
        "`devtools::install_github('bdbmax/bslib')`"
      ))
    }
  }


  # Setup -------------------------------------------------------------------

  variables <- scales_variables_modules$variables

  out_folder <- "www/place_explorer"
  if (!out_folder %in% list.files("www", full.names = TRUE)) dir.create(out_folder)
  if (!grepl("/$", out_folder)) out_folder <- paste0(out_folder, "/")

  all_tables <- reconstruct_all_tables(scales_variables_modules$scales)
  regions <- regions_dictionary$region[regions_dictionary$pickable]
  all_tables <- all_tables[names(all_tables) %in% regions]
  all_tables <- lapply(all_tables, \(scales) scales[!scales %in% skip_scales])

  possible_scales <- mapply(\(region, scale) {
    scales <- scales_variables_modules$scales
    scales[[region]][names(scales[[region]]) %in% scale]
  }, names(all_tables), all_tables, SIMPLIFY = FALSE)

  # Only pre-process the first scale now.
  possible_scales <-
    sapply(possible_scales, \(x) x[1], USE.NAMES = FALSE, simplify = FALSE)

  # Get the head file -------------------------------------------------------

  inp <- system.file(paste0("place_explorer/pe_en_outside_app.Rmd"),
    package = "curbcut"
  )

  header_file <- paste0(getwd(), "/", out_folder, "header.html")

  title_card_data <-
    curbcut::placeex_main_card_final_output(
      pe_main_card_data = pe_main_card_data,
      region = names(possible_scales)[1],
      df = names(possible_scales[[1]])[1],
      select_id = possible_scales[[1]][[1]]$ID[1],
      lang = "en",
      scales_dictionary = scales_dictionary,
      regions_dictionary = regions_dictionary
    )

  rmarkdown::render(inp,
    output_file = header_file,
    params = list(
      select_id = possible_scales[[1]][[1]]$ID[1],
      region = names(possible_scales)[1],
      df = names(possible_scales[[1]])[1],
      scale_sing = "",
      tileset_prefix = tileset_prefix,
      map_loc = possible_scales[[1]][[1]]$centroid[[1]],
      map_zoom = 10,
      mapbox_username = mapbox_username,
      title_card_data = title_card_data,
      variables = variables,
      scale_df = sf::st_drop_geometry(possible_scales[[1]][[1]])
    ), envir = new.env(), quiet = TRUE
  )

  x <- readLines(header_file)
  x <-
    x[{
      (which(stringr::str_detect(x, "<head"))):(which(stringr::str_detect(x, "</head")))
    }]
  writeLines(x, header_file)


  # Iterate over all possibilities ------------------------------------------

  all_files <- list.files(out_folder, full.names = TRUE)

  progressr::with_progress({
    pb <- progressr::progressor(steps = sum(
      unlist(sapply(
        possible_scales,
        sapply, nrow
      ))
    ) * length(lang))
    lapply(lang, \(lan) {
      inp <- system.file(paste0("place_explorer/pe_", lan, "_outside_app.Rmd"),
        package = "curbcut"
      )

      lapply(seq_along(possible_scales), \(region_n) {
        region <- names(possible_scales)[region_n]
        scales <- possible_scales[[region_n]]

        lapply(seq_along(scales), \(scale_n) {
          scale_name <- names(scales)[scale_n]
          scale_df <- scales[[scale_n]]
          scale_df <- suppressWarnings(sf::st_centroid(scale_df))
          scale_sing <-
            scales_dictionary$slider_title[scales_dictionary$scale == scale_name]
          # If there is only one row in the scale
          if (nrow(scale_df) <= 1) {
            return(NULL)
          }

          lapply(scale_df$ID, \(ID) {
            # If exists, pass
            geo_sc_id <- paste(region, scale_name, ID, lan, sep = "_")
            output_file <- paste0(out_folder, geo_sc_id, ".html")
            if (!overwrite & output_file %in% all_files) {
              pb()
              return(NULL)
            }
            output_file <- paste0(getwd(), "/", out_folder, geo_sc_id, ".html")
            df <- scale_df[scale_df$ID == ID, ]

            # Setup all necessary input
            map_loc <- df$centroid[[1]]
            title_card_data <-
              curbcut::placeex_main_card_final_output(
                region = region,
                df = scale_name,
                select_id = ID,
                lang = lan,
                pe_main_card_data = pe_main_card_data,
                scales_dictionary = scales_dictionary,
                regions_dictionary = regions_dictionary
              )
            map_zoom <- (\(x) {
              if (scale_name == "CT") {
                return(11)
              }
              if (scale_name == "DA") {
                return(13)
              }
              # For first level
              return(10)
            })()

            # Setup temporary .kmit.md files to not have names crashing on
            # each other when using parallelization
            new_rmd <- tempfile(pattern = geo_sc_id, fileext = ".Rmd")
            file.copy(inp, new_rmd)

            # Add title
            title <-
              # If the `name` column isn't alphabet
              if (!grepl("[a-z|A-Z]", df$name)) {
                name <- if (rev_geocode_from_localhost) {
                  cc.data::rev_geocode_localhost(df)
                } else {
                  cc.data::rev_geocode_OSM(df)
                }

                if (lan == "en") {
                  sing <- scales_dictionary$sing[scales_dictionary$scale == scale_name]
                  paste("The", sing, "around", name)
                } else if (lan == "fr") {
                  sing <-
                    if (scale_name == "CT") {
                      "Le secteur de recensement"
                    } else if (scale_name == "DA") {
                      "L'aire de diffusion"
                    } else {
                      "La zone"
                    }
                  paste(sing, "autour du", name)
                }
              } else {
                df$name
              }

            tryCatch(
              rmarkdown::render(
                new_rmd,
                output_file = output_file,
                params = list(
                  title = title,
                  select_id = ID,
                  region = region,
                  df = scale_name,
                  scale_sing = paste(
                    scale_sing,
                    if (lan == "en") "(count)" else if (lan == "fr") "(compte)"
                  ),
                  tileset_prefix = tileset_prefix,
                  map_loc = map_loc,
                  map_zoom = map_zoom,
                  mapbox_username = mapbox_username,
                  title_card_data = title_card_data,
                  variables = variables,
                  scale_df = sf::st_drop_geometry(scale_df)
                ), envir = new.env(), quiet = TRUE
              ),
              error = function(e) {
                stop(sprintf("Rmarkdown render crashed at file %s", geo_sc_id))
              }
            )

            # Remove header (too heavy)
            x <- readLines(output_file)
            x <-
              x[-{
                (which(stringr::str_detect(x, "<head"))):(which(stringr::str_detect(x, "</head")))
              }]
            writeLines(x, output_file)

            pb()
          })
        })
      })
    })
  })

  # Return nothing ----------------------------------------------------------

  return()
}
