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
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return <`list`> A list containing updated scales, variables, and modules.
#' @export
bill44_page <- function(scales_variables_modules, scales_sequences, crs,
                        overwrite, inst_prefix) {

  # Get list of data variables ----------------------------------------------

  qs::qload("dev/data/built/zoning.qsm")

  # Grab dwelling constructions
  csds <- scales_variables_modules$scales$DA$CSD_ID |> unique()
  CSD <- cancensus::get_census("CA21", regions = list(CSD = csds), level = "CSD",
                               geo_format = "sf")
  CSD <- CSD[c("GeoUID", "Dwellings 2016", "Dwellings")]
  names(CSD) <- c("ID", "dwellings_2016", "dwellings", "geometry")

  CSD$fiver_years_dev <- CSD$dwellings - CSD$dwellings_2016


  # ### VISUALIZE AT DB SCALE WHAT DEVELOPMENT LOOKED LIKE ?
  # DB_21 <- cancensus::get_census("CA21", regions = list(CSD = 5935010), level = "DB",
  #                                geo_format = "sf")
  # DB_21 <- DB_21[c("GeoUID", "CSD_UID", "Dwellings")]
  # names(DB_21)[1] <- "ID"
  # DB_16 <- cancensus::get_census("CA16", regions = list(CSD = 5935010), level = "DB",
  #                                geo_format = "sf")
  # DB_16 <- DB_16[c("GeoUID", "CSD_UID", "Dwellings")]
  # names(DB_16)[3] <- "Dwellings_2016"
  #
  # DB_change <- interpolate_from_area(DB_21, DB_16, additive_vars = "Dwellings_2016",
  #                       crs = crs, round_additive = FALSE)
  # DB_change$dwellings_diff <-
  #   (DB_change$Dwellings - DB_change$Dwellings_2016) / DB_change$Dwellings_2016
  # DB_change$dwellings_diff[is.infinite(DB_change$dwellings_diff) | is.na(DB_change$dwellings_diff)] <- NA
  #
  #
  # # DB_change$dwellings_diff[DB_change$dwellings_diff > 1] <- 1
  # DB_change["dwellings_diff"] |> .mv()


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
  DB <- DB[c("ID", grep("_ID", names(DB), value = TRUE), "dwellings", "DB_area")]
  # Only keep DBs in an area we have zoning information

  res <- zoning_lots_residential[c("res_category_before", "res_category_after")]
  res$area <- cc.buildr::get_area(res)
  res$lot_area <- cc.buildr::get_area(res)

  lots <- sf::st_join(sf::st_centroid(res), DB[c("ID", "CSD_ID", "DB_area", "dwellings")])
  lots <- lots[!is.na(lots$ID), ]

  # For every category previous to the Bill 44, how many units (per lot) are built
  # per zone?
  hou_p_lots <-
    lots |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ID) |>
    dplyr::filter(length(unique(res_category_before)) == 1) |>
    dplyr::filter((sum(lot_area) / DB_area) > 0.33) |>
    dplyr::mutate(nb_lots = dplyr::n()) |>
    dplyr::select(res_category_before, dwellings, nb_lots) |>
    dplyr::group_by(ID, res_category_before) |>
    dplyr::summarize(dwellings = unique(dwellings),
                     nb_lots = unique(nb_lots), .groups = "drop") |>
    dplyr::group_by(res_category_before) |>
    dplyr::summarize(dwellings = sum(dwellings),
                     nb_lots = sum(nb_lots)) |>
    dplyr::group_by(res_category_before) |>
    dplyr::summarize(dwellings_per_lots = dwellings / nb_lots)

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
  lots_new_zoning$lot_id <- sprintf("lot_%s", seq_along(lots_new_zoning$ID))




  # CALCULATE POTENTIAL FOR DEVELOPMENT
  lots_new_zoning$development_potential <- pmax(lots_new_zoning$new_zoning - lots_new_zoning$dwellings_per_lots, 0)
  lots_new_zoning <- lots_new_zoning[!is.na(lots_new_zoning$development_potential), ]


  # Function to allocate units within a single CSD
  allocate_units_in_csd <- function(csd_lots, units_to_allocate) {

    # Percentage of units to add to all lots
    pct_add <- units_to_allocate / sum(csd_lots$development_potential)

    # Of the potential, x pct is developed. Add it to the allocated units number
    csd_lots$allocated_units <- csd_lots$allocated_units + (csd_lots$development_potential * pct_add)

    # Remove the allocated units from the development potential, so it can be
    # calculated incrementally
    csd_lots$development_potential <- csd_lots$development_potential - csd_lots$allocated_units

    return(csd_lots)


    # while(units_to_allocate > 0) {
    #   # Probabilities based on development potential
    #   probabilities <- csd_lots$development_potential / sum(csd_lots$development_potential)
    #
    #   # Select a lot
    #   selected_lot <- sample(nrow(csd_lots), 1, prob = probabilities)
    #
    #   # Units to allocate to the selected lot
    #   units_for_lot <- min(units_to_allocate, csd_lots$development_potential[selected_lot])
    #   csd_lots$allocated_units[selected_lot] <- csd_lots$allocated_units[selected_lot] + units_for_lot
    #
    #   # Update remaining units and development potential
    #   units_to_allocate <- units_to_allocate - units_for_lot
    #   csd_lots$development_potential[selected_lot] <- csd_lots$development_potential[selected_lot] - units_for_lot
    # }
    # return(csd_lots)
  }

  end_tibble <- tibble::tibble()

  # Iterate over each CSD and allocate units
  for (csd_id in unique(lots_new_zoning$CSD_ID)) {
    # Filter lots for the current CSD
    current_csd_lots <- subset(lots_new_zoning, CSD_ID == csd_id)

    # Initialize allocated units for these lots
    current_csd_lots$allocated_units <- rep(0, nrow(current_csd_lots))

    # Additional number of units in the last 5 years
    fiver_years_dev <- CSD$fiver_years_dev[CSD$ID == csd_id]
    # Bring it up to 2029 (8 years)
    total_new_units_for_csd <- fiver_years_dev / 5 * 8

    # Allocate units in the current CSD (baseline scenario)
    updated_csd_lots <- allocate_units_in_csd(current_csd_lots, total_new_units_for_csd)
    updated_csd_lots$allocated_units_baseline <- updated_csd_lots$allocated_units

    # Add 50% of the 4.11% additional units (of housing stock in 2024)
    dwellings_2021 <- CSD$dwellings[CSD$ID == csd_id]
    dwellings_2024 <- dwellings_2021 + (fiver_years_dev / 5 * 3)
    auckland_units_addition <- dwellings_2024 * 0.0411 * 0.5
    updated_csd_lots <- allocate_units_in_csd(updated_csd_lots, auckland_units_addition)
    updated_csd_lots$allocated_units_auckland50 <- updated_csd_lots$allocated_units

    # Add 100% of the 4.11% additional units (of housing stock)
    # No change to `auckland_units_addition`, we just add it to the previous numbers
    updated_csd_lots <- allocate_units_in_csd(updated_csd_lots, auckland_units_addition)
    updated_csd_lots$allocated_units_auckland100 <- updated_csd_lots$allocated_units

    # Add 150% of the 4.11% additional units (of housing stock)
    updated_csd_lots <- allocate_units_in_csd(updated_csd_lots, auckland_units_addition)
    updated_csd_lots$allocated_units_auckland150 <- updated_csd_lots$allocated_units

    # Update the main dataset
    updated_csd_lots <- updated_csd_lots[
      c("lot_id", "ID", "allocated_units_baseline", "allocated_units_auckland50",
        "allocated_units_auckland100", "allocated_units_auckland150")]
    end_tibble <- rbind(end_tibble, updated_csd_lots)
  }

  # Sum per DBs
  additions <-
    end_tibble |>
    dplyr::group_by(ID) |>
    dplyr::summarize(`000` = sum(allocated_units_baseline),
                     `050` = sum(allocated_units_auckland50),
                     `100` = sum(allocated_units_auckland100),
                     `150` = sum(allocated_units_auckland150))

  additions <- merge(DB, additions, by = "ID", all.x = TRUE)
  additions[is.na(additions)] <- 0
  # The first scenario is 2021 dwelling counts!
  additions$`000` <- additions$dwellings

  # Calculate values for every scale
  only_scales <- scales_greater_than(base_scale = scales_variables_modules$scales$DB,
                                     all_scales = scales_variables_modules$scales,
                                     crs = crs)
  only_scales <- only_scales[only_scales %in% c("CSD", "CD", "CT", "DA", "DB")]

  data <- mapply(\(scale_name, scale_df) {

    id_col <- sprintf("%s_ID", scale_name)

    out <- lapply(unique(additions[[id_col]]), \(ID) {

      all_dbs <- additions[additions[[id_col]] == ID, ]
      # ID exists?
      if (length(scale_df$area[scale_df$ID == ID]) == 0) return(NULL)
      # DBs cover enough of the ID?
      if (sum(all_dbs$DB_area) < (scale_df$area[scale_df$ID == ID] * 0.75)) return(NULL)
      out <- tibble::tibble(ID = ID)

      for (i in c("000", "050", "100", "150")) {
        name <- sprintf("bill44_count_%s", i)

        # Calculate the updated number of dwellings
        updated_dwellings <- round(all_dbs$dwellings + (all_dbs[[i]]))
        if (i == "000") updated_dwellings <- all_dbs$dwellings # Baseline scenario is 2021 dwellings
        out[[name]] <- sum(updated_dwellings)

        # Calculate the number of dwellings per square kilometer and add it as a new column
        name_sqkm <- sprintf("bill44_sqkm_%s", i)
        out[[name_sqkm]] <- sum(updated_dwellings) / sum(all_dbs$DB_area / 1e6)
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
                                   bill44_sqkm = "bill44_sqkm_000"),
                 inst_prefix = inst_prefix)


  # Variables table ---------------------------------------------------------

  ## Dates at which the data is available
  dates <- unique(curbcut::s_extract(time_regex, var))
  dates <- gsub("^_", "", dates)
  names(dates) <- c("Baseline (2021)", "Low growth (2029)", "Medium growth (2029)", "High growth (2029)")

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
      ),
      classification = "physical",
      schema = list()
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
      nav_title = "Small-Scale, Multi-Unit Housing legislation (Bill 44)",
      title_text_title = "Small-Scale, Multi-Unit Housing (SSMUH) legislation",
      title_text_main = paste0(
        "<p>The Small-Scale, Multi-Unit Housing (SSMUH) legislation, or Bill 44",
        ", aims to increase housing supply, create more diverse housing choices",
        ", and over time, contribute to more affordable housing across BC. 70-8",
        "5% of privately held residential land in BC communities often falls un",
        "der zoning regulations that exclusively permit single-family detached ",
        "homes. However, this is far from meeting the actual housing needs of m",
        "any people in most BC communities. The current zoning regulations limi",
        "t the diversity of housing supply that is needed.<p>The SSMUH legislat",
        "ion aims to contribute to diversifying housing supply and increasing d",
        "ensity by permitting multiple units of housing on single-family and du",
        "plex lots without rezoning processes. Local governments across the Pro",
        "vince are now required to permit a minimum of two to six units of hous",
        "ing (depending on location and context) on formerly single-family or d",
        "uplex lots."
      ),
      title_text_extra = paste0(
        "<p>This page allows you to explore the current (2021) residential zoni",
        "ng and three potential future scenarios resulting from Bill 44: low gr",
        "owth, medium growth and high growth. These scenarios are modelled on a",
        " set of assumptions based on past precedents. Many cities have recentl",
        "y implemented upzoning policies which eliminate single-family zoning, ",
        "Auckland, New Zealand being a unique case. Most cities’ zoning reforms",
        " are too recent to identify trends or evidence of success, however Auc",
        "kland implemented major changes to their zoning regulations in 2016. <",
        "p>The potential future scenarios on this page have been modelled based",
        " on findings related to the outcomes in the zoning reform in Auckland ",
        "as cited in The impact of upzoning on housing construction in Auckland",
        " (Greenaway-McGrevy, Philips, 2023) in the Journal of Economics. This ",
        "study found that over the five years following the reform, the housing",
        " supply increased by 4.11%. The potential future scenarios in BC have ",
        "been modelled on this basis: <ul style='margin-left:20px; list-style:",
        " disc;'><li>The 2021 scenario represents the existing dwelling informa",
        "tion from the Canadian census without any zoning changes.<li>The Low G",
        "rowth scenario projects a 2.055% increase in evenly distributed develo",
        "pment in areas impacted by Bill 44 by 2029. This scenario represents t",
        "he potential future increase in housing supply following the assumptio",
        "n that construction trends follow a reduced version (50%) of the outco",
        "mes in Auckland. The projection includes the historical rate of housin",
        "g growth from 2016-2021, based on Census data, and the assumption that",
        " this rate remains unchanged from 2021-2029. Following the implementat",
        "ion of Bill 44, from 2024-2029, this scenario projects an additional 2",
        ".055% increase in housing growth based on Auckland’s past trends.<li>T",
        "he Medium Growth scenario projects a 4.11% increase in evenly distribu",
        "ted development in areas impacted by Bill 44 by 2029. This scenario re",
        "presents the potential future increase in housing supply following the",
        " assumption that construction trends follow the same outcomes in Auckl",
        "and (100%). The projection includes the historical rate of housing gro",
        "wth from 2016-2021, based on Census data, and the assumption that this",
        " rate remains unchanged from 2021-2029. Following the implementation o",
        "f Bill 44, from 2024-2029, this scenario projects an additional 4.11% ",
        "increase in housing growth based on Auckland’s past trends.<li>The Hig",
        "h Growth scenario projects a 6.165% increase in evenly distributed dev",
        "elopment in areas impacted by Bill 44 by 2029. This scenario represent",
        "s the potential future increase in housing supply following the assump",
        "tion that construction trends surpass the outcomes in Auckland (150%).",
        " The projection includes the historical rate of housing growth from 20",
        "16-2021, based on Census data, and the assumption that this rate remai",
        "ns unchanged from 2021-2029. Following the implementation of Bill 44, ",
        "from 2024-2029, this scenario projects an additional 6.165% increase i",
        "n housing growth based on Auckland’s past trends.</ul><p>Use the slide",
        "r on the left-hand panel to view these different scenarios and their p",
        "otential outcomes and select ‘Compare scenarios’ to see how two scenar",
        "ios might differ. <p>The modelling on this page is Curbcut’s interpret",
        "ation of some of the possible outcomes of Bill 44 and is not intended ",
        "as a legal interpretation.<p>For more information about how these scen",
        "arios were modelled, select table view for a technical explanation. <p",
        ">For the full article The impact of upzoning on housing construction i",
        "n Auckland (Greenaway-McGrevy, Philips, 2023) in the Journal of Econom",
        "ics please visit: <a href = ‘The impact of upzoning on housing constru",
        "ction in Auckland - ScienceDirect’>The impact of upzoning on housing c",
        "onstruction in Auckland</a><p>For more information about the SSMUH leg",
        "islation please visit: <a href=’https://www2.gov.bc.ca/assets/gov/hous",
        "ing-and-tenancy/tools-for-government/local-governments-and-housing/ssm",
        "uh_provincial_policy_manual.pdf’>Provincial Policy Manual & Site Standards</a>"
      ),
      metadata = FALSE,
      dataset_info = paste0(
        "<p>In the projections of distribution of new dwelling units across com",
        "munities, we use a methodology that prioritizes lots based on their po",
        "tential for development. This is determined by comparing the current n",
        "umber of dwellings on a lot to the new maximum number of dwellings all",
        "owed under the revised zoning regulations. Here's a simplified explana",
        "tion of the process:</p><ol><li><strong>Determining Development Potent",
        "ial:</strong> For each lot, we calculate its development potential. Th",
        "is is determined by the difference between the new maximum number of d",
        "wellings allowed under Bill 44 and the current number of dwellings on ",
        "a lot. Lots with a larger difference have a greater potential for deve",
        "lopment.</li><li><strong>Determination of Total Development Potential:",
        "</strong> The total development potential within a Census Subdivision ",
        "(CSD) is found by adding up the development potentials of all the lots",
        " within that CSD.</li><li><strong>Proportional Allocation of Units:</s",
        "trong> First, the total number of units to be allocated within a CSD i",
        "s determined. Then, each lot is allocated a number of units proportion",
        "al to its individual development potential.</li><li><strong>Incorporat",
        "ing Auckland-Inspired Scenarios in the Context of Bill 44 in British C",
        "olumbia:</strong> The methodology applies scenarios inspired by Auckla",
        "nd's experience with a similar bill, to project the potential impacts ",
        "of Bill 44 in BC. These scenarios are used to anticipate how housing d",
        "evelopment might unfold under similar policy changes in BC:<ol type=’a",
        "’><li><strong>Baseline Scenario:</strong> Units are allocated based on",
        " the historical increase in development from 2016 to 2021. This growth",
        " is projected up to 2029 (8 years), providing a baseline that reflects",
        " expected development without the influence of Bill 44.</li><li><stron",
        "g>Auckland 50% Scenario:</strong> An additional 2.055% of the total pr",
        "ojected 2024 housing stock of the CSD is allocated, reflecting the ini",
        "tial impact of similar zoning changes in Auckland. This scenario proje",
        "cts the early effects of Bill 44 in BC, drawing on Auckland's experien",
        "ce within the first five years of its implementation.</li><li><strong>",
        "Auckland 100% Scenario:</strong> This scenario builds upon the 50% sce",
        "nario, adding another 2.055% of the CSD's 2024 housing stock. The tota",
        "l increase of 4.11% mirrors the full expected impact observed in Auckl",
        "and, providing a parallel for the potential outcomes of Bill 44 in BC ",
        "over a five-year period.</li><li><strong>Auckland 150% Scenario:</stro",
        "ng> Continuing from the 100% scenario, an additional 2.055% of the 202",
        "4 housing stock is allocated. This ambitious projection explores the p",
        "ossible extended impact of Bill 44 in BC, akin to a 150% increase in h",
        "ousing development seen in Auckland's study over the same time frame.<",
        "/li></ol></li></ol>"
      ),
      var_left = unique_var,
      # var_left = variables[grepl("^bill44_", variables$var_code),
      #                      c("var_code", "group_name", "group_diff")],
      dates = dates,
      main_dropdown_title = "Data representation",
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
