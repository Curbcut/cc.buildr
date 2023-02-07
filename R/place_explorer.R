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
placeex_main_card <- function(scales, DA_table, region_DA_IDs, crs,
                              regions_dictionary) {


  # Init --------------------------------------------------------------------

  dict <- tibble::tibble(name = character(),
                         title = character(),
                         bs_icon = character(),
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
      bs_icon = "wind",
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
  data$no2 <- map_over_scales(all_scales = NO2_scales$scales,
                              fun = \(scale_df = scale_df, ...) {
                                if (!"NO2" %in% names(scale_df)) return(NULL)
                                out <- scale_df[c("ID", "NO2")]
                                out <- sf::st_drop_geometry(out)
                                out <- mc_df_format(out)
                                return(out)
                              })
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
      bs_icon = "tree",
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
  data$ndvi <- map_over_scales(all_scales = NDVI_scales$scales,
                               fun = \(scale_df = scale_df, ...) {
                                 if (!"NDVI" %in% names(scale_df)) return(NULL)
                                 out <- scale_df[c("ID", "NDVI")]
                                 out <- sf::st_drop_geometry(out)
                                 out <- mc_df_format(out)
                                 return(out)
                               })
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
      bs_icon = "bus-front",
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
      bs_icon = "houses",
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
      bs_icon = "activity",
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
