placeex_main_card <- function(scales, DA_table, region_DA_IDs = DA_table$ID, crs,
                              regions_dictionary) {


  # Init --------------------------------------------------------------------

  dict <- tibble::tibble(name = character(),
                         title = character(),
                         date = numeric(),
                         percent = logical(),
                         high_is_good = logical(),
                         val_digit = 0,
                         text = character())
  data <- list()

  DA_table <- DA_table["ID"]
  names(DA_table)[1] <- "DA_ID"
  current_census_year <- gsub("CA", "20", cc.buildr::current_census)
  regions <- regions_dictionary$geo[regions_dictionary$pickable]
  scales <- scales[names(scales) %in% regions]


  # Common functions --------------------------------------------------------

  mc_df_format <- function(df) {
    if (ncol(df) > 2)
      stop("Only two columns needed. ID and variable.")
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
      date = "2016",
      percent = FALSE,
      high_is_good = FALSE,
      val_digit = 1,
      text = paste0("{z$data_rank} in terms of level of NO2 pollution. ",
                    "{higher_than_threshold}(NO2 = {z$pretty_data_var}, ",
                    "data from {z$data_date})")
    )
  dict <- rbind(dict, no2_dict)

  NO2 <- cc.data::db_read_data("no2",
                               column_to_select = "DA_ID",
                               IDs = region_DA_IDs, crs = crs)
  NO2 <- merge(NO2, DA_table, all = TRUE)

  # Interpolate to all possible region and scales
  NO2_scales <- interpolate_from_census_geo(data = NO2,
                                            all_scales = scales,
                                            crs = crs,
                                            average_vars = "NO2",
                                            base_scale = "DA",
                                            weight_by = "area")

  # Format correctly
  data$NO2 <- map_over_scales(all_scales = NO2_scales$scales,
                              fun = \(scale_df = scale_df, ...) {
                                if (!"NO2" %in% names(scale_df)) return(NULL)
                                out <- scale_df[c("ID", "NO2")]
                                out <- sf::st_drop_geometry(out)
                                out <- mc_df_format(out)
                                return(out)
                              })
  # Remove empty lists
  data$NO2 <- lapply(data$NO2, \(x) {
    x[!sapply(x, is.null)]
  })
  data$NO2 <- data$NO2[sapply(data$NO2, length) > 0]


  # NDVI --------------------------------------------------------------------

  ndvi_dict <-
    tibble::tibble(
      name = "ndvi",
      title = "Vegetation",
      date = "2019",
      percent = TRUE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0("{z$data_rank} in terms of vegetation (<a href='https://",
                    "www.canuedata.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pdf' ",
                    "target='_blank'>NDVI</a> = {z$pretty_data_var}, data ",
                    "from {z$data_date})")
    )
  dict <- rbind(dict, ndvi_dict)

  NDVI <- cc.data::db_read_data("ndvi",
                               column_to_select = "DA_ID",
                               IDs = region_DA_IDs, crs = crs)
  NDVI <- merge(NDVI, DA_table, all = TRUE)

  # Interpolate to all possible region and scales
  NDVI_scales <- interpolate_from_census_geo(data = NDVI,
                                             all_scales = scales,
                                             crs = crs,
                                             average_vars = "NDVI",
                                             base_scale = "DA",
                                             weight_by = "area")

  # Format correctly
  data$NDVI <- map_over_scales(all_scales = NDVI_scales$scales,
                               fun = \(scale_df = scale_df, ...) {
                                 if (!"NDVI" %in% names(scale_df)) return(NULL)
                                 out <- scale_df[c("ID", "NDVI")]
                                 out <- sf::st_drop_geometry(out)
                                 out <- mc_df_format(out)
                                 return(out)
                               })
  # Remove empty lists
  data$NDVI <- lapply(data$NDVI, \(x) {
    x[!sapply(x, is.null)]
  })
  data$NDVI <- data$NDVI[sapply(data$NDVI, length) > 0]


  # Sustainable transport ---------------------------------------------------

  sust_dict <-
    tibble::tibble(
      name = "sust",
      title = "Sustainable transport",
      date = current_census_year,
      percent = TRUE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0("{z$pretty_data_var} of residents use public transit, ",
                    "walk or bicycle to get to work. {z$data_rank}. ",
                    "(Data from {z$data_date})")
    )
  dict <- rbind(dict, sust_dict)


  vars <- paste0(c("trans_walk_or_bike_", "trans_transit_"), current_census_year)

  # Format correctly
  data$sust <- map_over_scales(all_scales = scales,
                               fun = \(scale_df = scale_df, ...) {
                                 if (all(!vars %in% names(scale_df))) return(NULL)
                                 out <- scale_df[c("ID", vars)]
                                 out$var <- out[[vars[1]]] + out[[vars[2]]]
                                 out <- out[c("ID", "var")]
                                 out <- sf::st_drop_geometry(out)
                                 out <- mc_df_format(out)
                                 return(out)
                               })
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
      date = current_census_year,
      percent = TRUE,
      high_is_good = FALSE,
      val_digit = 0,
      text = paste0("{z$pretty_data_var} of occupied dwellings are single-det",
                    "ached houses. {z$data_rank}. (Data from {z$data_date})")
    )
  dict <- rbind(dict, singled_dict)


  vars <- paste0("housing_single_detached_", current_census_year)

  # Format correctly
  data$singled <- map_over_scales(all_scales = scales,
                               fun = \(scale_df = scale_df, ...) {
                                 if (all(!vars %in% names(scale_df))) return(NULL)
                                 out <- scale_df[c("ID", vars)]
                                 # out <- out[c("ID", "var")]
                                 out <- sf::st_drop_geometry(out)
                                 out <- mc_df_format(out)
                                 return(out)
                               })
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
      date = current_census_year,
      percent = FALSE,
      high_is_good = TRUE,
      val_digit = 0,
      text = paste0("{z$data_rank} in terms of active living. (Data from ",
                    "{z$data_date})")
    )
  dict <- rbind(dict, activel_dict)


  vars <- paste0("canale_", current_census_year)

  # Format correctly
  data$activel <- map_over_scales(all_scales = scales,
                                  fun = \(scale_df = scale_df, ...) {
                                    if (all(!vars %in% names(scale_df))) return(NULL)
                                    out <- scale_df[c("ID", vars)]
                                    # out <- out[c("ID", "var")]
                                    out <- sf::st_drop_geometry(out)
                                    out <- mc_df_format(out)
                                    return(out)
                                  })
  # Remove empty lists
  data$activel <- lapply(data$activel, \(x) {
    x[!sapply(x, is.null)]
  })
  data$activel <- data$activel[sapply(data$activel, length) > 0]


  # Return ------------------------------------------------------------------

  return(list(main_card_dict = dict,
              main_card_data = data))


}
