#' Bill 44 Zoning Analysis Page Function
#'
#' This function performs a zoning analysis based on Bill 44 legislation, focusing
#' on the impact of small-scale, multi-unit housing in various zones. It integrates
#' geographic data transformation, intersection analysis, and potential housing unit
#' calculations. The function finally updates modules and variables tables.
#'
#' @param scales_variables_modules <`list`> A list containing scales, variables, and
#' modules data structures.
#' @param scales_sequences <`list`> A list of scale sequences for data representation.
#' @param crs <`numeric`> Coordinate reference system to be used for spatial data.
#' @param overwrite <`logical`> Flag to determine if existing data should be overwritten.
#'
#' @return <`list`> A list containing updated scales, variables, and modules.
#' @export
bill44_page <- function(scales_variables_modules, scales_sequences, crs,
                        overwrite) {

  # Get list of data variables ----------------------------------------------

  qs::qload("dev/data/built/zoning.qsm")

  # Whichs CSDs are covered by the zoning information?
  CSD <- scales_variables_modules$scales$CSD
  if (is.null(CSD)) {
    csds <- scales_variables_modules$scales$DA$CSD_ID |> unique()
    CSD <- cancensus::get_census("CA21", regions = list(CSD = csds), level = "CSD",
                                 geo_format = "sf")
    CSD <- CSD[c("GeoUID", "Households", "Population")]
    names(CSD) <- c("ID", "households", "population", "geometry")
  }
  CSD <- sf::st_transform(CSD, crs)
  CSD$previous_area <- cc.buildr::get_area(CSD)
  CSD_int <- sf::st_intersection(CSD, zoning_lots)
  CSD_int$new_area <- cc.buildr::get_area(CSD_int)
  CSD_int <- CSD_int |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ID, previous_area) |>
    dplyr::summarize(new_area = sum(new_area)) |>
    dplyr::ungroup()

  CSD_covered <- CSD_int$ID[(CSD_int$new_area / CSD_int$previous_area) > 0.25]

  DB <- scales_variables_modules$scales$DB
  DB <- sf::st_transform(DB, crs)
  DB$DB_area <- cc.buildr::get_area(DB)
  DB <-  DB[DB$CSD_ID %in% CSD_covered, ]
  DB <- DB[c("ID", grep("_ID", names(DB), value = TRUE), "households", "DB_area")]
  # Only keep DBs in an area we have zoning information

  res <- zoning_lots_residential[c("res_category_before", "res_category_after")]
  res$area <- cc.buildr::get_area(res)
  res$lot_area <- cc.buildr::get_area(res)

  lots <- sf::st_join(sf::st_centroid(res), DB)

  # For every category previous to the Bill 44, how many units are built
  # per zone?
  hou_p_lots <-
    lots |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ID) |>
    dplyr::filter(length(unique(res_category_before)) == 1) |>
    dplyr::filter((sum(lot_area) / DB_area) > 0.33) |>
    dplyr::mutate(nb_lots = dplyr::n()) |>
    dplyr::select(res_category_before, households, nb_lots) |>
    dplyr::group_by(ID, res_category_before) |>
    dplyr::summarize(households = unique(households),
                     nb_lots = unique(nb_lots), .groups = "drop") |>
    dplyr::group_by(res_category_before) |>
    dplyr::summarize(households = sum(households),
                     nb_lots = sum(nb_lots)) |>
    dplyr::group_by(res_category_before) |>
    dplyr::summarize(households_per_lots = households / nb_lots)

  hou_p_lots <-
    rbind(hou_p_lots,
          tibble::tibble(res_category_before = "Single",
                         households_per_lots = 1))

  lots_new_zoning <- lots[lots$res_category_before != "Multiresidential", ] |>
    sf::st_drop_geometry() |>
    dplyr::mutate(new_zoning = dplyr::case_when(res_category_after == "Multiresidential (3 units)" ~ 3,
                                                res_category_after == "Multiresidential (4 units)" ~ 4,
                                                res_category_after == "Multiresidential (6 units)" ~ 6,
                                                res_category_after == "Single + Secondary suite/ADU" ~ 2))

  # Merge and fix missing 'Single'
  lots_new_zoning <- merge(lots_new_zoning, hou_p_lots, all.x = TRUE)

  # Remove Multi
  lots_new_zoning <- lots_new_zoning[lots_new_zoning$res_category_before != "Multiresidential", ]

  potentiel_new_units_per_DBs <-
    lots_new_zoning |>
    dplyr::group_by(ID) |>
    dplyr::summarize(
      rz_hou = sum(households_per_lots), # Sum of households per lot for each ID (when MULTI is removed)
      new_potential = sum(new_zoning),   # Sum of new zoning potential for each ID (for all the RESTRICTED ZONES (none-multi))
      potential_new_units = new_potential - rz_hou # Calculating the potential new units
    ) |>
    dplyr::select(ID, potential_new_units)

  additions <-
    merge(DB, potentiel_new_units_per_DBs, by = "ID", all.x = TRUE) |>
    dplyr::mutate(potential_new_units =
                    ifelse(is.na(potential_new_units), 0, potential_new_units)) |>
    sf::st_drop_geometry()

  # Calculate values for every scale
  only_scales <- scales_greater_than(base_scale = scales_variables_modules$scales$DB,
                                     all_scales = scales_variables_modules$scales,
                                     crs = crs)

  data <- mapply(\(scale_name, scale_df) {

    id_col <- sprintf("%s_ID", scale_name)

    out <- lapply(unique(additions[[id_col]]), \(ID) {

      all_dbs <- additions[additions[[id_col]] == ID, ]
      out <- tibble::tibble(ID = ID)

      for (i in c(0, 2, 5, 100)) {
        name <- sprintf("bill44_count_%03d", i)

        # Calculate the updated number of households
        updated_households <- all_dbs$households + (all_dbs$potential_new_units * (i/100))
        out[[name]] <- sum(updated_households)

        # Calculate the number of households per square kilometer and add it as a new column
        name_sqkm <- sprintf("bill44_sqkm_%03d", i)
        out[[name_sqkm]] <- sum(updated_households) / sum(all_dbs$DB_area / 1e6)
      }

      out
    })

    merge(scale_df, data.table::rbindlist(out), all.x = TRUE)

  }, only_scales, scales_variables_modules$scales[only_scales], SIMPLIFY = FALSE, USE.NAMES = TRUE)


  # Check if we are really looking at a single variable.
  names(additions)[1] <- "DB_ID"
  time_regex <- "_\\d{3}$"
  var <- grep("bill44", names(data$DB), value = TRUE)
  unique_var <- unique(gsub(time_regex, "", var))

  # Interpolate data to all possible scales ---------------------------------

  avail_scale <- only_scales
  interpolated_ref <- tibble::tibble(scale = avail_scale,
                                     interpolated_from = rep("DB", length(avail_scale)))
  interpolated_ref$interpolated_from[
    interpolated_ref$scale == "DB"
  ] <- "FALSE"


  # Declare the types of the variables in a named list ----------------------

  types <- list("bill44_count" = "count",
                "bill44_sqkm" = "sqkm")

  # Data tibble -------------------------------------------------------------

  data_construct(scales_data = data,
                 unique_var = unique_var,
                 time_regex = time_regex,
                 breaks_var = list(bill44_count = "bill44_count_000",
                                   bill44_sqkm = "bill44_sqkm_000"))


  # Variables table ---------------------------------------------------------

  ## Dates at which the data is available
  dates <- unique(curbcut::s_extract(time_regex, var))
  dates <- gsub("^_", "", dates)
  names(dates) <- c("Status quo", "2% is built", "5% is built", "Everything gets built")

  new_variables <- lapply(unique_var, \(var) {

    type <- if (grepl("count", var)) "count" else "sqkm"
    parent <- if (grepl("count", var)) "private_households" else "area"
    title <- if (grepl("count", var)) "Dwelling units (count)" else "Dwelling units per square kilometre"
    short <- if (grepl("count", var)) "Dwellings" else "Dwellings (km^2)"
    exp <- if (grepl("count", var)) {
      sprintf("the number of dwelling units")
    } else {
      sprintf("the number of dwelling units per square kilometre")
    }
    exp_q5 <- if (grepl("count", var)) {
      sprintf("would be living under")
    } else {
      sprintf("the density of dwelling units would be _X_ dwellings per square kilometre")
    }

    # Auto variables
    group_name <- if (grepl("count", var)) "Number of dwelling units" else "Dwelling units per square kilometre"

    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = type,
      var_title = group_name,
      var_short = short,
      explanation = exp,
      exp_q5 = exp_q5,
      parent_vec = parent,
      theme = "Land use",
      private = FALSE,
      pe_include = FALSE,
      dates = dates,
      avail_scale = avail_scale,
      source = "Curbcut",
      interpolated = interpolated_ref,
      rankings_chr = c(
        "exceptionally sparse", "unusually sparse",
        "just about average", "unusually dense",
        "exceptionally dense"
      )
    ) |>
      (\(x) x[nrow(x), ])()
  })

  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))

  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = avail_scale)


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "bill44",
      theme = "Land use",
      nav_title = "Bill 44",
      title_text_title = "Small-Scale, Multi-Unit Housing (SSMUH) legislation",
      title_text_main = paste0(
        "<p>The Small-Scale, Multi-Unit Housing (SSMUH) legislation, or Bill 44, a",
        "ims to increase housing supply, create more diverse housing choices, a",
        "nd over time, contribute to more affordable housing across BC. 70-85% ",
        "of privately held residential land in BC communities often falls under",
        " zoning regulations that exclusively permit single-family detached hom",
        "es. However, this is far from meeting the actual housing needs of many",
        " people in most BC communities. The current zoning regulations limit t",
        "he diversity of housing supply that is needed.<p>The SSMUH legislation",
        " aims to contribute to diversifying housing supply and increasing dens",
        "ity by permitting multiple units of housing on single-family and duple",
        "x lots without rezoning processes. Local governments across the Provin",
        "ce are now required to permit a minimum of two to six units of housing",
        " (depending on location and context) on formerly single-family or dupl",
        "ex lots."
      ),
      paste0(
        "<p>This page allows you to explore the current residential zoning and X p",
        "otential future scenarios resulting from Bill 44. These scenarios are ",
        "modelled on a set of assumptions based on past precedents. In addition",
        " to viewing the potential outcomes, select compare to see how two scen",
        "arios might differ.<p>For more information about the SSMUH legislation",
        " please visit: <a href=’https://www2.gov.bc.ca/assets/gov/housing-and-",
        "tenancy/tools-for-government/local-governments-and-housing/ssmuh_provi",
        "ncial_policy_manual.pdf’>Provincial Policy Manual & Site Standards</a>"
      ),
      metadata = FALSE,
      dataset_info = "",
      var_left = unique_var,
      # var_left = variables[grepl("^bill44_", variables$var_code),
      #                      c("var_code", "group_name", "group_diff")],
      dates = dates,
      main_dropdown_title = "Data representation",
      var_right = scales_variables_modules$variables$var_code[
        scales_variables_modules$variables$source == "Canadian census" &
          !is.na(scales_variables_modules$variables$parent_vec)
      ],
      default_var = "bill44_sqkm",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))
}
