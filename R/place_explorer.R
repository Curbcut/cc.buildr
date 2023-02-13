#' Produce the place explorer main card
#'
#' @param scales <`list`> Lists of spatial features dataframes with regions and
#' scales filled with at minimum census and canale data. Usually is
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
    text = character()
  )
  data <- list()

  DA_table <- DA_table["ID"]
  names(DA_table)[1] <- "DA_ID"
  current_census_year <- gsub("CA", "20", cc.buildr::current_census)
  regions <- regions_dictionary$geo[regions_dictionary$pickable]
  scales <- scales[names(scales) %in% regions]


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
        "{z$data_rank} in terms of level of NO2 pollution. ",
        "{higher_than_threshold}(NO2 = {z$pretty_data_var}, ",
        "data from {z$data_date})"
      )
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
      date = "2019",
      percent = TRUE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0(
        "{z$data_rank} in terms of vegetation (<a href='https://",
        "www.canuedata.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pdf' ",
        "target='_blank'>NDVI</a> = {z$pretty_data_var}, data ",
        "from {z$data_date})"
      )
    )
  dict <- rbind(dict, ndvi_dict)

  NDVI <- cc.data::db_read_data("ndvi",
    column_to_select = "DA_ID",
    IDs = region_DA_IDs, crs = crs
  )
  NDVI <- merge(NDVI, DA_table, all = TRUE)

  # Interpolate to all possible region and scales
  NDVI_scales <- interpolate_from_census_geo(
    data = NDVI,
    all_scales = scales,
    crs = crs,
    average_vars = "NDVI",
    base_scale = "DA",
    weight_by = "area"
  )

  # Format correctly
  data$ndvi <- map_over_scales(
    all_scales = NDVI_scales$scales,
    fun = \(scale_df = scale_df, ...) {
      if (!"NDVI" %in% names(scale_df)) {
        return(NULL)
      }
      out <- scale_df[c("ID", "NDVI")]
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
        "{z$pretty_data_var} of residents use public transit, ",
        "walk or bicycle to get to work. {z$data_rank}. ",
        "(Data from {z$data_date})"
      )
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
        "{z$pretty_data_var} of occupied dwellings are single-det",
        "ached houses. {z$data_rank}. (Data from {z$data_date})"
      )
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
      xaxis_title = "Can-ALE index",
      bs_icon = "activity",
      date = current_census_year,
      percent = FALSE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0(
        "{z$data_rank} in terms of active living. (Data from ",
        "{z$data_date})"
      )
    )
  dict <- rbind(dict, activel_dict)


  vars <- paste0("canale_", current_census_year)

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
    main_card_data = data
  ))
}

#' Prepare data for a title card
#'
#' This function prepares data for a title card for a specific region and indicator.
#' It returns information about the data value, data date, data rank and color.
#'
#' @param data <`list`> Contains data about the indicator in its region
#' and scale, e.g. `pe_main_card$main_card_data$no2$CMA$CSD`.
#' @param dict <`data.frame`> Row from the dictionary for the data
#' (title, text, etc.), e.g.
#' `pe_main_card$main_card_dict[pe_main_card$main_card_dict$name == "no2", ]`.
#' @param region <`character`> The geographic region.
#' @param df <`data.frame`> The scale, e.g. `DA`, `CT`, ...
#' @param select_id <`character`> The ID of the selected area.
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link[cc.buildr]{build_census_scales}}
#' @param regions_dictionary <`data.frame`> A dictionary containing information
#' about the regions.
#'
#' @return A list of information about the data, including the data value,
#' data date, data rank (in text), and color category (1-5).
#'
#' @export
placeex_main_card_prep_output_en <- function(data, dict, region, df, select_id,
                                             scales_dictionary,
                                             regions_dictionary) {
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop(
      "Package \"glue\" must be installed to use this function.",
      call. = FALSE
    )
  }


  # Prepare English ordinal form fun ----------------------------------------

  ordinal_form <- function(x, en_first = "first") {
    if (x > 20) {
      if (x %% 100 %in% c(11, 12, 13)) {
        form <- "th "
      } else {
        form <- switch(as.character(x %% 10),
          "1" = "st",
          "2" = "nd",
          "3" = "rd",
          "th"
        )
      }
      paste0(x, form)
    } else {
      switch(as.character(x),
        "1" = "",
        "2" = "second",
        "3" = "third",
        "4" = "fourth",
        "5" = "fifth",
        "6" = "sixth",
        "7" = "seventh",
        "8" = "eighth",
        "9" = "ninth",
        "10" = "tenth",
        paste0(as.character(x), "th")
      )
    }
  }


  # Setup --------------------------------------------------------------------

  df_scale <- paste("The", scales_dictionary$sing[scales_dictionary$scale == df])
  df_scales <- scales_dictionary$plur[scales_dictionary$scale == df]

  # To what it compares
  to_compare <- regions_dictionary$to_compare[regions_dictionary$geo == region]

  # Prepare list to store all data
  info <- list()

  # Get data value
  data_s <- data[data$ID == select_id, ]
  if ({
    length(data_s$var) == 0
  } | {
    is.na(data_s$var)
  }) {
    return(NULL)
  }


  # pretty_data_var ---------------------------------------------------------

  info$pretty_data_var <- if (dict$percent) {
    scales::percent(data_s$var)
  } else {
    round(data_s$var, digits = dict$val_digit)
  }


  # Data date ---------------------------------------------------------------

  info$data_date <- dict$date


  # Data rank ---------------------------------------------------------------

  info$data_rank <-
    # If the dataset is small
    if (nrow(data) < 40) {
      # How many non-na entries in the dataset
      df_row <- sum(!is.na(data$var))
      # If high is good, then last rank means 1st. Inverse!
      data_rank <- if (dict$high_is_good) df_row - data_s$rank + 1 else data_s$rank

      ordinal <- (\(x) {
        # if ranks in the bottom third
        if (data_rank > (2 / 3 * df_row)) {
          return({
            glue::glue("relatively low at {ordinal_form(data_rank)}")
          })
        }
        # if ranks in the second third
        if (data_rank > (1 / 3 * df_row)) {
          return(ordinal_form(data_rank))
        }
        # else
        return(glue::glue("{ordinal_form(data_rank)} best"))
      })()

      glue::glue("It ranks {ordinal} {to_compare}")


      # If the dataset is large
    } else {
      (\(x) {
        if (data_s$percentile > 0.75) {
          return({
            paste0(
              glue::glue("{df_scale} ranks in the top "),
              if (abs(data_s$percentile - 1) < 0.01) {
                "1%"
              } else {
                scales::percent(abs(data_s$percentile - 1))
              }
            )
          })
        }

        if (data_s$percentile < 0.25) {
          return({
            paste0(
              glue::glue("{df_scale} ranks in the bottom "),
              if (data_s$percentile < 1) "1%" else scales::percent(data_s$percentile)
            )
          })
        }

        pretty_perc <- scales::percent(data_s$percentile)

        if (dict$high_is_good) {
          glue::glue(
            "Its value is worse than {pretty_perc} ",
            "of {df_scales} {to_compare}"
          )
        } else {
          glue::glue(
            "Its value is higher than {pretty_perc} ",
            "of {df_scales} {to_compare}"
          )
        }
      })()
    }


  # Colour ------------------------------------------------------------------

  colours_which <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  if (!dict$high_is_good) colours_which <- rev(colours_which)

  info$hex_cat <- which.min(abs(colours_which - data_s$percentile))

  # In case it's higher than the threshold of 53 for Air Quality
  if (dict$name == "no2" && data_s$var >= 53) info$hex_cat <- 1


  # Percentile --------------------------------------------------------------

  info$percentile <-
    if (data_s$percentile > 0.50) {
      per <- scales::percent(abs(data_s$percentile - 1))
      if (per == "0%") per <- "1%"
      paste0(glue::glue("Top {per}"))
    } else {
      per <- scales::percent(abs(data_s$percentile))
      if (per == "0%") per <- "1%"
      paste0(glue::glue("Bottom {per}"))
    }


  # Return ------------------------------------------------------------------

  return(info)
}


#' Final function for the place explorer main card
#'
#' @param pe_main_card_data <`list`> Place explorer data for the maincard. Usually
#' the output of \code{\link[cc.buildr]{placeex_main_card_data}} saved in the
#' data folder.
#' @param region <`character`> Region under study, e.g. `CMA`.
#' @param df <`character`> Scale under study, e.g. `DA`.
#' @param select_id <`character`> Selected identifier for the selected combinasion
#' of `region` and `df`.
#' @param regions_dictionary <`data.frame`> The regions dictionary built using
#' \code{\link[cc.buildr]{regions_dictionary}}. Will be used to filter out scales
#' for which data should not be calculated.
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link[cc.buildr]{build_census_scales}}
#'
#' @return Returns a list of all the main card variables and how the selected
#' id compares in its dataset.
#' @export
placeex_main_card_final_output <- function(pe_main_card_data, region, df, select_id,
                                           scales_dictionary, regions_dictionary) {
  ## Generate output grid ---------------------------------------------------

  to_grid <- lapply(pe_main_card_data$main_card_dict$name, \(x) {
    data <- pe_main_card_data$main_card_data[[x]][[region]][[df]]
    dict <- pe_main_card_data$main_card_dict[pe_main_card_data$main_card_dict$name == x, ]

    z <- placeex_main_card_prep_output_en(
      data = data,
      dict = dict,
      region = region,
      df = df,
      select_id = select_id,
      scales_dictionary = scales_dictionary,
      regions_dictionary = regions_dictionary
    )

    if (is.null(z)) {
      return(list(
        row_title = dict$title,
        percentile = "No data.",
        bs_icon = dict$bs_icon
      ))
    }

    # Exception - additional text for no2 if over the threshold of 53 ppm
    if (x == "no2") {
      higher_than_threshold <-
        if (z$pretty_data_var > 53) {
          "Its value is higher than the WHO's guideline value of 53. "
        } else {
          ""
        }
    }

    list(
      row_title = dict$title,
      percentile = z$percentile,
      text = glue::glue(dict$text),
      hex_cat = z$hex_cat,
      bs_icon = dict$bs_icon,
      xaxis_title = dict$xaxis_title,
      data = data
    )
  })

  names(to_grid) <- pe_main_card_data$main_card_dict$name

  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid
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
                                  mapbox_username = "sus-mcgill",
                                  rev_geocode_from_localhost = FALSE,
                                  check_bslib_version = TRUE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package \"rmarkdown\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (check_bslib_version) {
    if (packageVersion("bslib") != "9.9.9.9999") {
      stop(paste0("Wrong version of `bslib`. Place explorer tabs won't be ",
                  "colored according to how much of an outlier the zone is for ",
                  "every theme. To ignore, set `check_bslib_version = FALSE`. ",
                  "To build with colored tabs, install ",
                  "`devtools::install_github('bdbmax/bslib')`"))
    }
  }


  # Setup -------------------------------------------------------------------

  variables <- scales_variables_modules$variables

  out_folder <-  "www/place_explorer"
  if (!out_folder %in% list.files("www", full.names = TRUE)) dir.create(out_folder)
  if (!grepl("/$", out_folder)) out_folder <- paste0(out_folder, "/")

  all_tables <- reconstruct_all_tables(scales_variables_modules$scales)
  regions <- regions_dictionary$geo[regions_dictionary$pickable]
  all_tables <- all_tables[names(all_tables) %in% regions]
  all_tables <- lapply(all_tables, \(scales) scales[!scales %in% c("building", "street")])

  possible_scales <- mapply(\(region, scale) {
    scales <- scales_variables_modules$scales
    scales[[region]][names(scales[[region]]) %in% scale]
  }, names(all_tables), all_tables, SIMPLIFY = FALSE)


  # Get the head file -------------------------------------------------------

  inp <- system.file(paste0("place_explorer_rmd/pe_en.Rmd"),
    package = "cc.buildr"
  )

  header_file <- paste0(getwd(), "/", out_folder, "header.html")

  title_card_data <-
    placeex_main_card_final_output(
      pe_main_card_data = pe_main_card_data,
      region = names(possible_scales)[1],
      df = names(possible_scales[[1]])[1],
      select_id = possible_scales[[1]][[1]]$ID[1],
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

  progressr::with_progress({
    pb <- progressr::progressor(steps = sum(
      unlist(sapply(possible_scales,
                    sapply, nrow))) * length(lang))
    lapply(lang, \(lan) {
      inp <- system.file(paste0("place_explorer_rmd/pe_", lan, ".Rmd"),
        package = "cc.buildr"
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

          future.apply::future_lapply(seq_along(scale_df$ID), \(n) {
            # Setup all necessary input
            ID <- scale_df$ID[n]
            map_loc <- scale_df$centroid[[n]]
            title_card_data <-
              placeex_main_card_final_output(
                pe_main_card_data = pe_main_card_data,
                region = region,
                df = scale_name,
                select_id = ID,
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
            geo_sc_id <- paste(region, scale_name, ID, lan, sep = "_")

            # Setup temporary .kmit.md files to not have names crashing on
            # each other when using parallelization
            new_rmd <- tempfile(pattern = geo_sc_id, fileext = ".Rmd")
            file.copy(inp, new_rmd)

            output_file <- paste0(getwd(), "/", out_folder, geo_sc_id, ".html")

            # Add title
            title <-
              # If the `name` column isn't alphabet
              if (!grepl("[a-z|A-Z]", scale_df$name[n])) {
                name <- if (rev_geocode_from_localhost) {
                  cc.data::rev_geocode_localhost(scale_df[n, ])
                } else {
                  cc.data::rev_geocode_OSM(scale_df[n, ])
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
                scale_df$name[n]
              }

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
            )

            # Remove header (too heavy)
            x <- readLines(output_file)
            x <-
              x[-{
                (which(stringr::str_detect(x, "<head"))):(which(stringr::str_detect(x, "</head")))
              }]
            writeLines(x, output_file)

            pb()
          }, future.seed = NULL)
        })
      })
    })
  })

  # Return nothing ----------------------------------------------------------

  return()
}
