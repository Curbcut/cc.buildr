#' Produce the place explorer main card
#'
#' @param scales_dictionary <`list`> Scales dictionary
#' @param DA_table <`sf data.frame`> Tables containing all the dissemination areas
#' of the zone under study. Usually is `census_scales$DA`
#' @param region_DA_IDs <`character vector`> Vector of all the dissemination area's
#' identifiers in the zone under study. Usually is `census_scales$DA$ID`
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param regions_dictionary <`data.frame`> The regions dictionary built using
#' \code{\link[cc.buildr]{regions_dictionary}}. Will be used to filter out scales
#' for which data should not be calculated.
#' @param first_scales <`character vector`> Scales for which place explorer should
#' be calculated
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return Lists containing all the main card indicators. Sub lists of regions
#' and scales for data values.
#' @export
placeex_main_card_data <- function(scales_dictionary, DA_table, region_DA_IDs, crs,
                                   inst_prefix,
                                   regions_dictionary,
                                   ignore_scales = c(
                                     "building", "street", "grd30", "grd60", "grd120",
                                     "grd300", "grd600"),
                                   first_scales) {
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

  # Which scales can we show PE for?
  scales <- scales_dictionary$scale[!scales_dictionary$scale %in% ignore_scales]
  scales <- sapply(scales, \(x) qs::qread(sprintf("data/geometry_export/%s.qs", x)),
                   simplify = FALSE, USE.NAMES = TRUE)

  # Filter regions ----------------------------------------------------------

  regions <- regions_dictionary$region[regions_dictionary$pickable]

  all_scales <- sapply(regions, \(reg) {
    reg_db <- regions_dictionary$scales[regions_dictionary$region == reg][[1]]
    out <- lapply(names(scales), \(scale_name) {
      scale_df <- scales[[scale_name]]
      scale_df[scale_df$ID %in% reg_db[[scale_name]], ]
    })
    names(out) <- names(scales)
    out
  }, simplify = FALSE, USE.NAMES = TRUE)


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
    weight_by = "area",
    inst_prefix = inst_prefix
  )$scales

  # Format correctly
  data$no2 <- map_over_scales(
    all_scales = all_scales,
    fun = \(scale_df = scale_df, scale_name = scale_name, geo = geo, ...) {
      no2 <- NO2_scales[[scale_name]]

      if (!"NO2" %in% names(no2)) {
        return(NULL)
      }

      out <- no2[c("ID", "NO2")]
      out <- sf::st_drop_geometry(out)
      out <- merge(scale_df, out, by = "ID", all.x = TRUE)[c("ID", "NO2")]
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
    all_scales = all_scales,
    fun = \(scale_df = scale_df, scale_name = scale_name, ...) {
      data_file <- sprintf("data/%s/ndvi.qs", scale_name)
      if (!file.exists(data_file)) return(NULL)

      dat <- qs::qread(data_file)
      out <- dat[c(1, ncol(dat))]
      out <- out[out$ID %in% scale_df$ID, ]

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


  vars <- c("trans_walk_or_bike", "trans_transit")

  # Format correctly
  data$sust <- map_over_scales(
    all_scales = all_scales,
    fun = \(scale_df = scale_df, scale_name = scale_name, ...) {
      data_file <- sprintf("data/%s/", scale_name)
      data_file <- paste0(data_file, vars, ".qs")
      if (!file.exists(data_file[1])) return(NULL)

      dat <- lapply(data_file, qs::qread)
      dat <- lapply(dat, \(x) x[c(1, ncol(x))])
      dat <- lapply(dat, \(x) x[x$ID %in% scale_df$ID, ])
      out <- Reduce(merge, dat)
      out$var <- out[[2]] + out[[3]]
      out <- out[c("ID", "var")]

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

  # Format correctly
  data$singled <- map_over_scales(
    all_scales = all_scales,
    fun = \(scale_df = scale_df, scale_name = scale_name, ...) {
      data_file <- sprintf("data/%s/housing_single_detached.qs", scale_name)
      if (!file.exists(data_file)) return(NULL)

      dat <- qs::qread(data_file)
      out <- dat[c(1, ncol(dat))]
      out <- out[out$ID %in% scale_df$ID, ]

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

  # Format correctly
  data$activel <- map_over_scales(
    all_scales = all_scales,
    fun = \(scale_df = scale_df, scale_name = scale_name, ...) {
      data_file <- sprintf("data/%s/alp.qs", scale_name)
      if (!file.exists(data_file)) return(NULL)

      dat <- qs::qread(data_file)
      out <- dat[c(1, ncol(dat))]
      out <- out[out$ID %in% scale_df$ID, ]

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

  # For now, place explorer only takes first scales
  data <- lapply(data, lapply, `[`, first_scales)

  return(list(
    main_card_dict = dict,
    main_card_data = data
  ))
}

#' Final function for the place explorer main card
#'
#' @param pe_main_card_data <`list`> Place explorer data for the maincard. Usually
#' the output of \code{\link[cc.buildr]{placeex_main_card_data}} saved in the
#' data folder.
#' @param region <`character`> Region under study, e.g. `CMA`.
#' @param scale <`character`> Scale under study, e.g. `DA`.
#' @param select_id <`character`> Selected identifier for the selected combinasion
#' of `region` and `df`.
#' @param lang <`character`> Language that should be used to produce the main card
#' output. There need to be a function `placeex_main_card_prep_output_x` available
#' in that language. `en` or `fr`.
#' @param regions_dictionary <`data.frame`> The regions dictionary built using
#' \code{\link[cc.buildr]{regions_dictionary}}. Will be used to filter out scales
#' for which data should not be calculated.
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link[cc.buildr]{build_census_scales}}
#'
#' @return Returns a list of all the main card variables and how the selected
#' id compares in its dataset.
#' @export
placeex_main_card_final_output <- function(region, scale, scale_df, select_id, lang = "en",
                                           pe_main_card_data, scales_dictionary,
                                           regions_dictionary) {
  ## Generate output grid ---------------------------------------------------

  to_grid <- lapply(pe_main_card_data$main_card_dict$name, \(x) {
    data <- pe_main_card_data$main_card_data[[x]][[region]][[scale]]
    dict <- pe_main_card_data$main_card_dict[pe_main_card_data$main_card_dict$name == x, ]

    fun <- sprintf("placeex_main_card_prep_output_%s", lang)
    z <- do.call(fun, list(
      data = data,
      dict = dict,
      region = region,
      scale = scale,
      select_id = select_id,
      regions_dictionary = regions_dictionary,
      scales_dictionary = scales_dictionary
    ))

    if (is.null(z)) {
      return(list(
        row_title = dict$title,
        percentile = curbcut::cc_t("No data.", lang = lang),
        bs_icon = dict$bs_icon
      ))
    }

    # Exception - additional text for no2 if over the threshold of 53 ppm
    if (x == "no2") {
      higher_than_threshold <-
        if (z$pretty_data_var > 53) {
          curbcut::cc_t("Its value is higher than the WHO's guideline value of 53. ",
                        lang = lang
          )
        } else {
          ""
        }
    }

    # To use with glue_safe instead of glue
    data_rank <- z$data_rank
    pretty_data_var <- z$pretty_data_var
    data_date <- z$data_date

    # Assign translation_df to the global environment if it's not present,
    # so cc_t works.
    if (!exists("translation_df"))
      assign("translation_df", curbcut::cc_translation_df, envir = .GlobalEnv)

    list(
      row_title = curbcut::cc_t(dict$title, lang = lang),
      percentile = z$percentile,
      text = glue::glue_safe(curbcut::cc_t(dict$text, lang = lang)),
      hex_cat = z$hex_cat,
      bs_icon = dict$bs_icon,
      xaxis_title = curbcut::cc_t(dict$xaxis_title, lang = lang),
      data = data,
      link_module = dict$link_module,
      link_dropdown = dict$link_dropdown,
      link_var_code = dict$link_var_code
    )
  })

  names(to_grid) <- pe_main_card_data$main_card_dict$name

  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid
}

#' Prepare data for a title card (english)
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
#' @param scale <`data.frame`> The scale, e.g. `DA`, `CT`, ...
#' @param select_id <`character`> The ID of the selected area.
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link[cc.buildr]{build_census_scales}}
#' @param regions_dictionary <`data.frame`> A dictionary containing information
#' about the regions.
#'
#' @return A list of information about the data, including the data value,
#' data date, data rank (in text), and color category (1-5).
placeex_main_card_prep_output_en <- function(data, dict, region, scale, select_id,
                                             regions_dictionary, scales_dictionary) {
  # Setup --------------------------------------------------------------------

  df_scale <- paste("The", scales_dictionary$sing[scales_dictionary$scale == scale])
  df_scales <- scales_dictionary$plur[scales_dictionary$scale == scale]

  # To what it compares
  to_compare <- regions_dictionary$to_compare[regions_dictionary$region == region]

  # Prepare list to store all data
  info <- list()

  # Get data value
  data_s <- data[data$ID == select_id, ]

  if (length(data_s) == 0) return(NULL)
  if (is.data.frame(data_s) && nrow(data_s) == 0) return(NULL)

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
          rk <- curbcut::ordinal_form(x = data_rank, lang = "en")
          return({
            glue::glue_safe("relatively low at {rk}")
          })
        }
        # if ranks in the second third
        if (data_rank > (1 / 3 * df_row)) {
          return(curbcut::ordinal_form(x = data_rank, lang = "en"))
        }
        # else
        rk <- curbcut::ordinal_form(x = data_rank, lang = "en")
        return(glue::glue_safe("{rk} best"))
      })()

      glue::glue_safe("It ranks {ordinal} {to_compare}")


      # If the dataset is large
    } else {
      (\(x) {
        if (data_s$percentile > 0.75) {
          return({
            paste0(
              glue::glue_safe("{df_scale} ranks in the top "),
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
              glue::glue_safe("{df_scale} ranks in the bottom "),
              if (data_s$percentile < 1) "1%" else scales::percent(data_s$percentile)
            )
          })
        }

        pretty_perc <- scales::percent(data_s$percentile)

        if (dict$high_is_good) {
          glue::glue_safe(
            "Its value is worse than {pretty_perc} ",
            "of {df_scales} {to_compare}"
          )
        } else {
          glue::glue_safe(
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
      paste0(glue::glue_safe("Top {per}"))
    } else {
      per <- scales::percent(abs(data_s$percentile))
      if (per == "0%") per <- "1%"
      paste0(glue::glue_safe("Bottom {per}"))
    }


  # Return ------------------------------------------------------------------

  return(info)
}

#' Prepare data for a title card (french)
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
#' @param scale <`data.frame`> The scale, e.g. `DA`, `CT`, ...
#' @param select_id <`character`> The ID of the selected area.
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link[cc.buildr]{build_census_scales}}
#' @param regions_dictionary <`data.frame`> A dictionary containing information
#' about the regions.
#'
#' @return A list of information about the data, including the data value,
#' data date, data rank (in text), and color category (1-5).
placeex_main_card_prep_output_fr <- function(data, dict, region, scale, select_id,
                                             regions_dictionary, scales_dictionary) {
  # Setup --------------------------------------------------------------------

  df_scale <- "La zone"
  df_scales <- curbcut::cc_t(scales_dictionary$plur[scales_dictionary$scale == scale],
                             lang = "fr"
  )

  # To what it compares
  to_compare <- curbcut::cc_t(regions_dictionary$to_compare[regions_dictionary$region == region],
                              lang = "fr"
  )

  # Prepare list to store all data
  info <- list()

  # Get data value
  data_s <- data[data$ID == select_id, ]

  if (length(data_s) == 0) return(NULL)
  if (is.data.frame(data_s) && nrow(data_s) == 0) return(NULL)

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
          rk <- curbcut::ordinal_form(x = data_rank, lang = "fr")
          return({
            glue::glue_safe("relativement bas \u00e0  {rk}")
          })
        }
        # if ranks in the second third
        if (data_rank > (1 / 3 * df_row)) {
          return(curbcut::ordinal_form(x = data_rank, lang = "fr"))
        }
        # else
        rk <- curbcut::ordinal_form(x = data_rank, lang = "fr")
        if (rk == "premier") {
          return(glue::glue_safe("premier"))
        }
        return(glue::glue_safe("{rk} meilleure"))
      })()

      glue::glue_safe("Elle se classe {ordinal} {to_compare}")


      # If the dataset is large
    } else {
      (\(x) {
        if (data_s$percentile > 0.75) {
          return({
            paste0(
              glue::glue_safe("{df_scale} se classe parmis les "),
              if (abs(data_s$percentile - 1) < 0.01) {
                "1%"
              } else {
                scales::percent(abs(data_s$percentile - 1))
              }, " plus \u00e9lev\u00e9s"
            )
          })
        }

        if (data_s$percentile < 0.25) {
          return({
            paste0(
              glue::glue_safe("{df_scale} se classe parmis les "),
              if (data_s$percentile < 1) "1%" else scales::percent(data_s$percentile),
              " plus faibles"
            )
          })
        }

        pretty_perc <- scales::percent(data_s$percentile)

        if (dict$high_is_good) {
          glue::glue_safe(
            "Sa valeur est inf\u00e9rieure \u00e0 celle de {pretty_perc} des {df_scales} {to_compare}"
          )
        } else {
          glue::glue_safe(
            "Sa valeur est sup\u00e9rieure \u00e0 celle de {pretty_perc} des {df_scales} {to_compare}"
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
      paste0(glue::glue_safe("{per} plus haut"))
    } else {
      per <- scales::percent(abs(data_s$percentile))
      if (per == "0%") per <- "1%"
      paste0(glue::glue_safe("{per} plus bas"))
    }


  # Return ------------------------------------------------------------------

  return(info)
}

#' Generate a Mapbox Tile JSON
#'
#' Given a Mapbox username, tileset prefix, and tile name, this function
#' generates a Mapbox Tile JSON using \code{\link[rdeck]{tile_json}}. If the
#' specified tile is not found, a warning message is displayed and NULL is
#' returned. This prevents the app from crashing.
#'
#' @param mapbox_username <`character`> string representing the Mapbox username.
#' @param tileset_prefix <`character`> Prefix attached to every tileset. Should
#' correspond to the Curbcut city, e.g. `mtl`.
#' @param tile <`character`> The tile name to be fetched.
#' @param return_error <`logical`> Print the error if the tileset isn't found.
#'
#' @return A JSON list if succesfull. If missing tile, returns NULL preventing
#' the app from crashing. If the tile is missing and it's a _building tile,
#' grab the first region of the regions_dictionary and show buildings for those.
#'
#' @export
tilejson <- function(mapbox_username, tileset_prefix, tile, return_error = FALSE) {
  # urltools is necessary for tile_json use
  requireNamespace("urltools", quietly = TRUE)
  tile_link <- paste0(mapbox_username, ".", tileset_prefix, "_", tile)
  out <- tryCatch(
    suppressWarnings(rdeck::tile_json(tile_link)),
    error = function(e) {
      if (curbcut::is_scale_in("building", tile)) {
        regions_dictionary <- curbcut::get_from_globalenv("regions_dictionary")
        base_building_tile <-
          sprintf(
            "%s.%s_%s_building", mapbox_username, tileset_prefix,
            regions_dictionary$region[1]
          )
        rdeck::tile_json(base_building_tile)
      } else {
        if (return_error) print(e)
        return(NULL)
      }
    }
  )
  return(out)
}

#' Pre-process all the possible Rmds
#'
#' This function processes data to knit R Markdown documents for the place explorer,
#' implementing custom styling features not available in the standard `bslib` package.
#' It temporarily uses a custom version of `bslib` from GitHub to allow individual styling
#' of each tab in a navigation bar, which is essential for the visualization requirements of
#' Curbcut. After processing, it installes the newest version of `bslib`.
#'
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
#' @param reinstall_bslib <`logical`> Whether to reinstall the newest version of
#' bslib on exit, considering 'bdbmax/bslib' must be installed. Defaults to TRUE.
#' @param overwrite <`logical`> Should the .html files be overwritten? Defaults
#' to `TRUE`.
#'
#' @return Returns nothing if successful. All place explorer possibilities are
#' saved in the `out_folder`.
#' @export
placeex_main_card_rmd <- function(pe_main_card_data,
                                  regions_dictionary,
                                  scales_dictionary,
                                  lang = "en",
                                  tileset_prefix,
                                  mapbox_username = "curbcut",
                                  rev_geocode_from_localhost = TRUE,
                                  check_bslib_version = TRUE,
                                  reinstall_bslib = TRUE,
                                  overwrite = TRUE,
                                  scales_sequences,
                                  full_data_path = sprintf("%s/data/", getwd())) {
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
  if (reinstall_bslib) on.exit(install.packages("bslib"))


  # Setup -------------------------------------------------------------------

  variables <- qs::qread(sprintf("%svariables.qs", full_data_path))

  out_folder <- "www/place_explorer"
  if (!out_folder %in% list.files("www", full.names = TRUE)) dir.create(out_folder)
  if (!grepl("/$", out_folder)) out_folder <- paste0(out_folder, "/")

  regions <- regions_dictionary$region[regions_dictionary$pickable]

  all_scales <- unlist(lapply(pe_main_card_data$main_card_data, lapply, names),
                       use.names = FALSE)
  all_scales <- unique(all_scales)
  all_scales <- all_scales[!is.na(all_scales)]
  all_scales <- sapply(all_scales, \(x) qs::qread(sprintf("data/geometry_export/%s.qs", x)),
                       simplify = FALSE, USE.NAMES = TRUE)


  # Get the head file -------------------------------------------------------

  inp <- system.file(paste0("place_explorer/pe_en.Rmd"),
                     package = "cc.buildr"
  )

  header_file <- paste0(getwd(), "/", out_folder, "header.html")

  title_card_data <-
    placeex_main_card_final_output(
      pe_main_card_data = pe_main_card_data,
      region = regions[[1]],
      scale = names(all_scales)[[1]],
      scale_df = all_scales[[1]],
      select_id = all_scales[[1]]$ID[1],
      lang = "en",
      scales_dictionary = scales_dictionary,
      regions_dictionary = regions_dictionary
    )

  rmarkdown::render(inp,
                    output_file = header_file,
                    params = list(
                      select_id = all_scales[[1]]$ID[1],
                      region = regions[[1]],
                      scale = names(all_scales)[[1]],
                      scale_sing = "",
                      tileset_prefix = tileset_prefix,
                      map_loc = sf::st_centroid(all_scales[[1]][1, ])$geometry |> unlist(),
                      map_zoom = 10,
                      mapbox_username = mapbox_username,
                      title_card_data = title_card_data,
                      variables = variables,
                      scale_df = sf::st_drop_geometry(all_scales[[1]]),
                      data_path = full_data_path
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
    pb <- progressr::progressor(steps = sum(sapply(regions, \(reg) {
      sum(sapply(scales_dictionary$regions, \(x) reg %in% x))
    })) * length(lang))
    lapply(lang, \(lan) {
      inp <- system.file(paste0("place_explorer/pe_", lan, ".Rmd"),
                         package = "cc.buildr"
      )

      future.apply::future_lapply(regions, \(region) {

        scales <- scales_dictionary$scale[sapply(scales_dictionary$regions, \(x) region %in% x)]
        scales <- scales[scales %in% names(all_scales)]

        lapply(scales, \(scale) {
          scale_df <- all_scales[[scale]]
          scale_df <- suppressWarnings(sf::st_centroid(scale_df))

          # FILTER DF USING REGION!
          r_scales <- regions_dictionary$scales[regions_dictionary$region == region][[1]]
          scale_df <- scale_df[scale_df$ID %in% r_scales[[scale]], ]

          scale_sing <-
            scales_dictionary$slider_title[scales_dictionary$scale == scale]
          # If there is only one row in the scale
          if (nrow(scale_df) <= 1) {
            return(NULL)
          }

          lapply(scale_df$ID, \(ID) {

            # If exists, pass
            geo_sc_id <- paste(region, scale, ID, lan, sep = "_")
            output_file <- paste0(out_folder, geo_sc_id, ".html")
            if (!overwrite & output_file %in% all_files) {
              pb()
              return(NULL)
            }
            output_file <- paste0(getwd(), "/", out_folder, geo_sc_id, ".html")
            df <- scale_df[scale_df$ID == ID, ]
            if (nrow(df) == 0) return(NULL)

            # Setup all necessary input
            map_loc <- sf::st_centroid(df)$geometry |> unlist()
            title_card_data <-
              placeex_main_card_final_output(
                region = region,
                scale = scale,
                select_id = ID,
                lang = lan,
                pe_main_card_data = pe_main_card_data,
                scales_dictionary = scales_dictionary,
                regions_dictionary = regions_dictionary
              )
            map_zoom <- (\(x) {
              if (scale == "CT") {
                return(11)
              }
              if (scale == "DA") {
                return(13)
              }
              # For first level
              return(9.9)
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
                  sing <- scales_dictionary$sing[scales_dictionary$scale == scale]
                  paste("The", sing, "around", name)
                } else if (lan == "fr") {
                  sing <-
                    if (scale == "CT") {
                      "Le secteur de recensement"
                    } else if (scale == "DA") {
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
                  scale = scale,
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
                  scale_df = sf::st_drop_geometry(scale_df),
                  data_path = full_data_path
                ), envir = new.env(), quiet = TRUE
              ),
              error = function(e) {
                stop(sprintf("Rmarkdown render crashed at file %s.\n%s", geo_sc_id, e))
              }
            )

            # Remove header (too heavy)
            x <- readLines(output_file)
            x <-
              x[-{
                (which(stringr::str_detect(x, "<head"))):(which(stringr::str_detect(x, "</head")))
              }]
            writeLines(x, output_file)

          })
          pb()
        })
      })
    })
  })

  # Return nothing ----------------------------------------------------------

  return()
}
