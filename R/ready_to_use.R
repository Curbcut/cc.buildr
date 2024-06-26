#' Add a ready to use ALP data and module
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param region_DB_IDs <`character vector`> All the current census'
#' DB IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param overwrite <`logical`> Should the data already processed and stored be
#' overwriten?
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the ALP variable added, its addition
#' in the variables table and the module table.
#' @export
ru_alp <- function(scales_variables_modules, regions_dictionary, region_DB_IDs,
                   scales_sequences, crs, overwrite = FALSE,
                   inst_prefix) {
  data <- cc.data::db_read_data("alp_DB",
                                column_to_select = "DB_ID",
                                IDs = region_DB_IDs, crs = crs
  )
  cols <- names(data)[names(data) != "DB_ID"]
  dates <- gsub("alp_", "", cols)


  ba_var(
    data = data,
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    base_scale = "DB",
    weight_by = "households",
    crs = crs,
    average_vars = cols,
    inst_prefix = inst_prefix,
    variable_var_code = "alp",
    variable_type = "ind",
    variable_var_title = "Active living potential",
    variable_var_short = "Active living",
    variable_explanation = "the potential for active living",
    variable_exp_q5 = "are living in areas with _X_ potential for active living",
    variable_classification = "other",
    variable_theme = "Health",
    variable_private = FALSE,
    variable_source = "Curbcut",
    variable_pe_include = TRUE,
    module_id = "alp",
    module_theme = "Health",
    module_nav_title = "Active living potential",
    module_title_text_title = "Active living potential",
    module_title_text_main = paste0(
      "<p>The walkability of an area is influenced by both the built environment ",
      "and socio-economic factors. The Active Living Potential index ",
      "quantifies which areas provide walkable environments to their residents."
    ),
    module_title_text_extra = paste0(
      "<p>The datasets visualized on this page come from Curbcut using data from",
      " the Canadian Censuses and DMTI. Our index considers street connectivity",
      ", building density, and points of interest. Active Living Potential is",
      " then calculated based on dissemination blocks accessible within a 15-m",
      "inute walk. The work on this page was highly influenced by the <a href",
      " = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf', ",
      "target = '_blank'>CanALE index</a> developed by Prof. Nancy Ross and h",
      "er team."
    ),
    module_metadata = TRUE,
    module_dataset_info = paste0(
      "<p>The data visualized on this page come from Curbcut. Active Living P",
      "otential (ALP) is an index created using datasets from DMTI, Statistic",
      "s Canada road network files and the Canadian Census. The index conside",
      "rs three variables—street connectivity, building density, and points o",
      "f interest—for which high values collectively describe areas that stro",
      "ngly support active living. The percentile of each variable is calcula",
      "ted at the dissemination block scale, based on dissemination blocks ac",
      "cessible within a 15-minute walk from a dissemination block centroid, ",
      "and the sum of these percentiles is the ALP index value. The dataset i",
      "s calculated from 2001 through 2021 in five-year intervals (correspond",
      "ing to Census years). Our ALP index was highly influenced by the <a hr",
      "ef = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf'",
      ", target = '_blank'>CanALE index developed by Ross et al. (2018)</a>. ",
      "Our index differs by calculating a buffer using a 15-minute walk on th",
      "e street network using our internal travel time matrix dataset instead",
      " of a 1km buffer around the centroid of dissemination blocks. Our inde",
      "x also differs by using a sum of percentiles rather than a sum of z-sc",
      "ores. This method reduces the influence of extreme outliers, especiall",
      "y in the case of points of interest which have a very large variance. ",
      "Thus, this percentile approach offers a balanced and nuanced understan",
      "ding of an area's walkability."
    ),
    module_dates = dates,
    overwrite = overwrite
  )
}

#' Add a ready to use Can-BICS data and module
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param overwrite <`logical`> Should the data already processed and stored be
#' overwriten?
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the Can-BICS variable added, its addition
#' in the variables table and the module table.
#' @export
ru_canbics <- function(scales_variables_modules, region_DA_IDs,
                       scales_sequences, crs, overwrite = FALSE,
                       inst_prefix) {
  data <- cc.data::db_read_data("canbics",
                                column_to_select = "DA_ID",
                                IDs = region_DA_IDs, crs = crs
  )
  cols <- names(data)[names(data) != "DA_ID"]
  dates <- gsub("canbics_", "", cols)

  out <- ba_var(
    data = data,
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    base_scale = "DA",
    weight_by = "households",
    crs = crs,
    average_vars = cols,
    inst_prefix = inst_prefix,
    variable_var_code = "canbics",
    variable_type = "ind",
    variable_var_title = "Can-BICS metric",
    variable_var_short = "Can-BICS",
    variable_explanation = "the comfort and safety of bikeways",
    variable_exp_q5 = "are living in areas with _X_ cycling infrastructure comfort and safety",
    variable_classification = "physical",
    variable_theme = "Transport",
    variable_pe_include = TRUE,
    variable_private = FALSE,
    variable_source = "Meghan Winters (and her team) at Faculty of Health Sciences, Simon Fraser University",
    module_id = "canbics",
    module_theme = "Transport",
    module_nav_title = "Bikeway comfort and safety",
    module_title_text_title = "Bikeway comfort and safety: the Can-BICS index",
    module_title_text_main = paste0(
      "Can-BICS, or Canadian Bikeway Comfort and Safety, is a classification ",
      "system for rating cycling infrastructure across Canada on safety and ",
      "user comfort."
    ),
    module_title_text_extra = paste0(
      "<p>Can-BICS rates bikeways into three tiers: high-, medium-,",
      " and low-comfort. The datasets visualized on this page come from CANUE ",
      "and the 2021 Canadian Census. Can-BICS was developed by Meghan Winters ",
      "and her team. Understanding the spatialization of cycling infrastructure ",
      "as classified by Can-BICS can help to high",
      "light the availability and infrastructure types across a re",
      "gion and support efforts in improving bikeways. For more information a",
      "bout Can-BICS visit: <a target = '_blank' href='https://www.canada.ca/",
      "en/public-health/services/reports-publications/health-promotion-chroni",
      "c-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/",
      "canbics-classification-system-naming-convention-cycling-infrastructure",
      ".html'>At-a-glance – The Canadian Bikeway Comfort and Safety (Can-BICS",
      ") Classification System: a common naming convention for cycling infras",
      "tructure</a>"
    ),
    module_metadata = TRUE,
    module_dataset_info =
      paste0(
        "<p><a target = '_blank' href = 'https://www.canada.ca/en/public-health/services/reports",
        "-publications/health-promotion-chronic-disease-prevention-canada-",
        "research-policy-practice/vol-40-no-9-2020/canbics-classification-",
        "system-naming-convention-cycling-infrastructure.html'>",
        "The Canadian Bikeway Comfort and Safety (Can-BICS) Classification System</a> dataset is ",
        "a geographic-based set of measures charac",
        "terizing the cycling infrastructure of Canadian communities. ",
        "The data is initially provided at the dissemination area level.</p>"
      ),
    module_dates = dates,
    overwrite = overwrite
  )

  # Change DA interpolation to postal codes
  interpolated_df <- out$variables$interpolated[out$variables$var_code == "canbics"][[1]]
  interpolated_df$interpolated_from[grepl("_DA$", interpolated_df$scale)] <- "postal codes"
  out$variables$interpolated[out$variables$var_code == "canbics"][[1]] <- interpolated_df

  # Return
  return(out)
}

#' Add a ready to use Land Surface Temperature data and module
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param overwrite <`logical`> Should the data already processed and stored be
#' overwriten?
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the Can-BICS variable added, its addition
#' in the variables table and the module table.
#' @export
ru_lst <- function(scales_variables_modules, region_DA_IDs,
                   scales_sequences, crs, overwrite = FALSE,
                   inst_prefix) {
  data <- cc.data::db_read_data("lst",
    column_to_select = "DA_ID",
    IDs = region_DA_IDs, crs = crs
  )
  cols <- names(data)[names(data) != "DA_ID"]


  out <- ba_var(
    data = data,
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    base_scale = "DA",
    weight_by = "area",
    crs = crs,
    average_vars = cols,
    inst_prefix = inst_prefix,
    variable_var_code = "lst",
    variable_type = c("avg", "degree"),
    variable_var_title = "Land surface temperature",
    variable_var_short = "Land temp.",
    variable_explanation = "the average warm-season land surface temperature",
    variable_exp_q5 = "the average warm-season land surface temperature is _X_ degrees celsius",
    variable_classification = "physical",
    variable_theme = "Climate",
    variable_pe_include = TRUE,
    variable_private = FALSE,
    variable_source = "The Canadian Urban Environmental Health Research Consortium",
    module_id = "lst",
    module_theme = "Climate",
    module_nav_title = "Land surface temperature",
    module_title_text_title = "Land surface temperature",
    module_title_text_main = paste0(
      "Land surface temperature measures the maximum mean warm-season ",
      "temperature at a specific location. It is a crucial indicator of urban ",
      "heat islands and ecological balance within a region."
    ),
    module_title_text_extra = paste0(
      "<p>This data represents the highest mean warm-season temperature recorded ",
      "at a location over a three-year span, helping to minimize ",
      "the impact of missing data or cloud cover. LST is instrumental ",
      "in identifying areas that are hotter during the day and more likely to radiate ",
      "excess heat at night, contributing to urban heat phenomena. Understanding ",
      "LST is essential for urban planning, health assessments, ",
      "and environmental protection. To learn more about how LST is calculated, ",
      "<a href='https://www.canuedata.ca/tmp/CANUE_METADATA_WTLST_AVA_YY.pdf' target='_blank'>click here</a>.</p>"
    ),
    module_metadata = TRUE,
    module_dataset_info =
      paste0(
        "<p>This dataset, provided by the Canadian Urban Environmental Health ",
        "Research Consortium, includes annual estimates of LST developed using a public ",
        "algorithm in Google Earth Engine. The data, derived from LandSat 8 imagery, ",
        "represents a 3 years annual maximum mean warm-season land surface temperature.</p>"
      ),
    module_dates = as.numeric(paste0(20, 15:21)),
    overwrite = overwrite
  )

  # Change DA interpolation to postal codes
  interpolated_df <- out$variables$interpolated[out$variables$var_code == "lst"][[1]]
  interpolated_df$interpolated_from[grepl("_DA$", interpolated_df$scale)] <- "postal codes"
  out$variables$interpolated[out$variables$var_code == "lst"][[1]] <- interpolated_df

  # Return
  return(out)
}

#' Add a ready to use Vacancy Rate data and module
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param geo_uid <`numeric`> Cancensus CMA code, which can be found using
#' \code{\link[cancensus]{list_census_regions}}.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param approximate_name_match <`logical`> CMHC zone naming can be different
#' year to year (A single typo, or other). Should the function search for
#' approximate matches to the name of the `cmhczone` table in
#' `scales_variables_modules`? Useful for Montreal where names are more or less
#' unique and so an approximate match can be beneficial. less for Toronto where
#' the name `York` is used in many different names.
#' @param overwrite <`logical`> Should the data already processed and stored be
#' overwriten?
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the CMHC's vacancy rate variables added,
#' their addition in the variables table and the module table.
#' @export
ru_vac_rate <- function(scales_variables_modules, crs, geo_uid,
                        scales_sequences = scales_sequences,
                        approximate_name_match = TRUE,
                        overwrite = FALSE,
                        inst_prefix = inst_prefix) {
  # Relevant dimensions
  # Rent ranges temporarily taken out as they are not available for the
  # Rental Univers series (parent vector)
  dimensions <-
    c("Bedroom Type", "Year of Construction") # , "Rent Ranges")
  dimensions_short <-
    c("bed", "year") # , "rent_range")
  years <- 2010:2022
  time_regex <- "_\\d{4}$"

  # Skip data building
  unique_vars <- c("vac_rate_bachelor_bed", "vac_rate_1_bed_bed", "vac_rate_2_bed_bed",
                   "vac_rate_3_bed_plus_bed", "vac_rate_total_bed", "vac_rate_before_1960_year",
                   "vac_rate_1960_1979_year", "vac_rate_1980_1999_year", "vac_rate_2000_or_later_year",
                   "vac_rate_total_year", "rental_universe_bachelor_bed", "rental_universe_1_bed_bed",
                   "rental_universe_2_bed_bed", "rental_universe_3_bed_plus_bed",
                   "rental_universe_total_bed", "rental_universe_before_1960_year",
                   "rental_universe_1960_1979_year", "rental_universe_1980_1999_year",
                   "rental_universe_2000_or_later_year", "rental_universe_total_year")

  scs <- exclude_processed_scales(unique_vars = unique_vars,
                                  scales = scales_variables_modules$scales["cmhczone"],
                                  overwrite = overwrite,
                                  inst_prefix = inst_prefix)

  if (length(scs) > 0) {
    # Retrieval
    cmhc_vac_rate <-
      sapply(years, \(yr) {
        over_year <-
          mapply(\(x, y) {
            # Get data
            out <- cmhc::get_cmhc(
              survey = "Rms",
              series = "Vacancy Rate",
              dimension = x,
              breakdown = "Survey Zones",
              geo_uid = geo_uid,
              year = yr
            )[, 1:3]
            # Rename column and update for real percentage
            names(out)[2] <- y
            out[3] <- out[3] / 100

            # Pivot and rename
            out <- tidyr::pivot_wider(out,
                                      names_from = tidyr::all_of(y),
                                      values_from = "Value"
            )
            names(out) <- gsub("Non-Market/Unknown", "non_market", names(out))
            names(out) <- gsub(" |-", "_", tolower(names(out)))
            names(out) <- gsub("___", "_", names(out))
            names(out) <- gsub("\\+", "plus", names(out))
            names(out) <- gsub("_units", "", names(out))
            names(out) <- gsub("bedroom", "bed", names(out))
            names(out) <- gsub("\\$", "", names(out))
            names(out) <- gsub("less_than", "less", names(out))
            names(out) <- gsub(",", "", names(out))
            names(out) <- paste("vac_rate", names(out), y, yr, sep = "_")
            names(out)[1] <- "name"

            # Change the name to the closest string in the CMHC zone scale
            out <- out[!is.na(out$name), ]
            if (approximate_name_match) {
              out$name <-
                sapply(out$name,
                       agrep,
                       x = scales_variables_modules$scales$cmhczone$name,
                       value = TRUE, USE.NAMES = FALSE
                )
              if (!all(sapply(out$name, length) == 1)) {
                stop(paste0(
                  "Approximate name matching matched more than one ",
                  "name in `", yr,
                  "`. Consider using `approximate_name_match = FALSE`"
                ))
              }
            }
            # Return
            out
          }, dimensions, dimensions_short, SIMPLIFY = FALSE, USE.NAMES = TRUE)
        cmhc <- Reduce(merge, over_year)
        tibble::as_tibble(cmhc)
      }, simplify = FALSE, USE.NAMES = TRUE)

    cmhc_rental_units <-
      sapply(years, \(yr) {
        over_year <-
          mapply(\(x, y) {
            # Get data
            out <- cmhc::get_cmhc(
              survey = "Rms",
              series = "Rental Universe",
              dimension = x,
              breakdown = "Survey Zones",
              geo_uid = geo_uid,
              year = yr
            )[, 1:3]
            # Rename column and update for real percentage
            names(out)[2] <- y
            out[3] <- out[3]

            # Pivot and rename
            out <- tidyr::pivot_wider(out,
                                      names_from = tidyr::all_of(y),
                                      values_from = "Value"
            )
            names(out) <- gsub("Non-Market/Unknown", "non_market", names(out))
            names(out) <- gsub(" |-", "_", tolower(names(out)))
            names(out) <- gsub("___", "_", names(out))
            names(out) <- gsub("\\+", "plus", names(out))
            names(out) <- gsub("_units", "", names(out))
            names(out) <- gsub("bedroom", "bed", names(out))
            names(out) <- gsub("\\$", "", names(out))
            names(out) <- gsub("less_than", "less", names(out))
            names(out) <- gsub(",", "", names(out))
            names(out) <- paste("rental_universe", names(out), y, yr, sep = "_")
            names(out)[1] <- "name"

            # Change the name to the closest string in the CMHC zone scale
            out <- out[!is.na(out$name), ]
            if (approximate_name_match) {
              out$name <-
                sapply(out$name,
                       agrep,
                       x = scales_variables_modules$scales$cmhczone$name,
                       value = TRUE, USE.NAMES = FALSE
                )
              if (!all(sapply(out$name, length) == 1)) {
                stop(paste0(
                  "Approximate name matching matched more than one ",
                  "name in `", yr,
                  "`. Consider using `approximate_name_match = FALSE`"
                ))
              }
            }
            # Return
            out
          }, dimensions, dimensions_short, SIMPLIFY = FALSE, USE.NAMES = TRUE)
        cmhc <- Reduce(merge, over_year)
        tibble::as_tibble(cmhc)
      }, simplify = FALSE, USE.NAMES = TRUE)

    # Merge vacancy rate with rental units
    cmhc <- mapply(merge, cmhc_vac_rate, cmhc_rental_units,
                   SIMPLIFY = FALSE
    )

    # Merge with the `sf`
    merged <-
      Reduce(\(x, y) merge(x, y, by = "name", all.x = TRUE),
             cmhc,
             init = sf::st_drop_geometry(
               scales_variables_modules$scales$cmhczone
             )[, "name"]
      )

    # Variables
    vars <- names(merged)[!grepl("name|ID|geometry", names(merged))]
    unique_vars <- unique(gsub("_\\d{4}$", "", vars))

    # Append data
    merged_to_svm <- scales_variables_modules$scales
    merged_to_svm$cmhczone <-
      merge(merged_to_svm$cmhczone,
            merged,
            by = "name"
      )

    # Data tibble
    data_construct(scales_data = merged_to_svm,
                   unique_var = unique_vars,
                   time_regex = time_regex,
                   inst_prefix = inst_prefix)
  }

  # Types
  types <- rep(list("pct"), sum(grepl("^vac_rate", unique_vars)))
  names(types) <- unique_vars[grepl("^vac_rate", unique_vars)]
  types_count <- rep(list("count"), sum(grepl("^rental_universe", unique_vars)))
  names(types_count) <- unique_vars[grepl("^rental_universe", unique_vars)]
  types <- c(types, types_count)

  # Parent strings
  parent_strings <- lapply(gsub("vac_rate", "rental_universe", unique_vars[
    grepl("^vac_rate", unique_vars)
  ]), c)
  names(parent_strings) <- unique_vars[grepl("^vac_rate", unique_vars)]
  parent_strings_count <- lapply(rep(NA, sum(grepl("^rental_universe", unique_vars))), c)
  names(parent_strings_count) <- unique_vars[grepl("^rental_universe", unique_vars)]
  parent_strings <- c(parent_strings, parent_strings_count)


  # Add to the variables table
  variables <-
    lapply(unique_vars[grepl("^vac_rate", unique_vars)], \(var) {
      # Create title and explanation
      cat_title <- (\(x) {
        # Bedroom types
        if (grepl("_bed$", var)) {
          if (grepl("bachelor", var)) {
            return("studio apartments")
          }
          suff <- "housing units"
          if (grepl("1_bed", var)) {
            return(paste("one-bedroom", suff))
          }
          if (grepl("2_bed", var)) {
            return(paste("two-bedroom", suff))
          }
          if (grepl("3_bed_plus", var)) {
            return(paste("three-bedroom and larger", suff))
          }
          if (grepl("total_bed", var)) {
            return(paste("all", suff))
          }
        }
        # Year of construction
        if (grepl("_year$", var)) {
          pre <- "housing units built"
          if (grepl("before_1960", var)) {
            return(paste(pre, "before 1960"))
          }
          if (grepl("1960_1979", var)) {
            return(paste(pre, "between 1960 and 1979"))
          }
          if (grepl("1980_1999", var)) {
            return(paste(pre, "between 1980 and 1999"))
          }
          if (grepl("2000_or_later", var)) {
            return(paste(pre, "after 2000"))
          }
          if (grepl("total_year", var)) {
            return(paste("all housing units"))
          }
        }
        # Rent ranges
        if (grepl("rent_range$", var)) {
          pre <- "housing units with a rent"
          if (grepl("less_750", var)) {
            return(paste(pre, "below $750"))
          }
          if (grepl("750_999", var)) {
            return(paste(pre, "between $750 and $999"))
          }
          if (grepl("1000_1249", var)) {
            return(paste(pre, "between $1,000 and $1,249"))
          }
          if (grepl("1250_1499", var)) {
            return(paste(pre, "between $1,250 and $1,499"))
          }
          if (grepl("1500_plus", var)) {
            return(paste(pre, "higher than $1,500"))
          }
          if (grepl("non_market", var)) {
            return(paste("housing units with an unknown rent"))
          }
          if (grepl("total_rent_range", var)) {
            return(paste("all housing units"))
          }
        }
      })(var)
      title <- paste("Vacancy rate in", cat_title)
      explanation <- paste(
        "the percentage of available rental",
        gsub("^all ", "", cat_title),
        "that are vacant or unoccupied"
      )

      # Create short title
      cat_short <- (\(x) {
        # Bedroom types
        if (grepl("_bed$", var)) {
          if (grepl("bachelor", var)) {
            return("studio")
          }
          if (grepl("1_bed", var)) {
            return("1bed")
          }
          if (grepl("2_bed", var)) {
            return("2bed")
          }
          if (grepl("3_bed_plus", var)) {
            return("3+bed")
          }
          if (grepl("total_bed", var)) {
            return("total")
          }
        }
        # Year of construction
        if (grepl("_year$", var)) {
          if (grepl("before_1960", var)) {
            return("<1960")
          }
          if (grepl("1960_1979", var)) {
            return(">1960<1979")
          }
          if (grepl("1980_1999", var)) {
            return(">1980<1999")
          }
          if (grepl("2000_or_later", var)) {
            return(">2000")
          }
          if (grepl("total_year", var)) {
            return("total")
          }
        }
        # Rent ranges
        if (grepl("_rent_range$", var)) {
          if (grepl("less_750", var)) {
            return("<$750")
          }
          if (grepl("750_999", var)) {
            return(">$750<$999")
          }
          if (grepl("1000_1249", var)) {
            return(">$1k<$1.25k")
          }
          if (grepl("1250_1499", var)) {
            return(">$1.25k<$1.5k")
          }
          if (grepl("1500_plus", var)) {
            return(">$1.5k")
          }
          if (grepl("non_market", var)) {
            return("?$")
          }
          if (grepl("total_rent_range", var)) {
            return("total")
          }
        }
      })(var)
      short <- paste("Vac. rate", cat_short)

      # Create group_name
      cat_group_name <- (\(x) {
        # Bedroom types
        if (grepl("_bed$", var)) {
          return("Bedroom type")
        }
        # Year of construction
        if (grepl("_year$", var)) {
          return("Year of construction")
        }
        # Rent ranges
        if (grepl("_rent_range$", var)) {
          return("Rent range")
        }
      })(var)
      group_name <- paste("Vacancy rate by", tolower(cat_group_name))

      # Create group_diff
      group_diff <- list(paste("For", cat_title))
      names(group_diff) <- cat_group_name

      # Include in place exporer
      pe_include <-
        if (grepl("2_bed|2000_or_later|1500_plus|vac_rate_total_bed", var)) TRUE else FALSE

      out <-
        add_variable(
          variables = scales_variables_modules$variables,
          var_code = var,
          type = "pct",
          var_title = title,
          var_short = short,
          explanation = explanation,
          exp_q5 = "are vacant or unoccupied",
          parent_vec = parent_strings[[var]],
          classification = "other",
          theme = "Vacancy rate",
          private = FALSE,
          pe_include = pe_include,
          dates = years,
          avail_scale = "cmhczone",
          source = "Canada Mortgage and Housing Corporation",
          interpolated = tibble::tibble(
            scale = "cmhczone",
            interpolated_from = FALSE
          ),
          group_name = group_name,
          group_diff = group_diff,
          rankings_chr = c(
            "an exceptionally low vacancy rate",
            "an unusually low vacancy rate",
            "a just about average vacancy rate",
            "an unusually high vacancy rate",
            "an exceptionally high vacancy rate"
          )
        )

      out[out$var_code == var, ]
    }) |> (\(x) Reduce(rbind, x, init = scales_variables_modules$variables))()

  variables <-
    lapply(unique_vars[grepl("^rental_universe", unique_vars)], \(var) {
      # Create title and explanation
      cat_title <- (\(x) {
        # Bedroom types
        if (grepl("_bed$", var)) {
          if (grepl("bachelor", var)) {
            return("rental studio apartments")
          }
          suff <- "rental housing units"
          if (grepl("1_bed", var)) {
            return(paste("one-bedroom", suff))
          }
          if (grepl("2_bed", var)) {
            return(paste("two-bedroom", suff))
          }
          if (grepl("3_bed_plus", var)) {
            return(paste("three-bedroom and larger", suff))
          }
          if (grepl("total_bed", var)) {
            return(paste(suff))
          }
        }
        # Year of construction
        if (grepl("_year$", var)) {
          pre <- "rental housing units built"
          if (grepl("before_1960", var)) {
            return(paste(pre, "before 1960"))
          }
          if (grepl("1960_1979", var)) {
            return(paste(pre, "between 1960 and 1979"))
          }
          if (grepl("1980_1999", var)) {
            return(paste(pre, "between 1980 and 1999"))
          }
          if (grepl("2000_or_later", var)) {
            return(paste(pre, "after 2000"))
          }
          if (grepl("total_year", var)) {
            return(paste("rental housing units"))
          }
        }
        # Rent ranges
        if (grepl("_rent_range$", var)) {
          pre <- "rental housing units with a rent"
          if (grepl("less_750", var)) {
            return(paste(pre, "below $750"))
          }
          if (grepl("750_999", var)) {
            return(paste(pre, "between $750 and $999"))
          }
          if (grepl("1000_1249", var)) {
            return(paste(pre, "between $1,000 and $1,249"))
          }
          if (grepl("1250_1499", var)) {
            return(paste(pre, "between $1,250 and $1,499"))
          }
          if (grepl("1500_plus", var)) {
            return(paste(pre, "higher than $1,500"))
          }
          if (grepl("non_market", var)) {
            return(paste("rental housing units with an unknown rent"))
          }
          if (grepl("total_rent_range", var)) {
            return(paste("rental housing units"))
          }
        }
      })(var)
      title <- stringr::str_to_sentence(cat_title)
      explanation <- paste("the number of", cat_title)


      # Create short title
      cat_short <- (\(x) {
        # Bedroom types
        if (grepl("_bed$", var)) {
          if (grepl("bachelor", var)) {
            return("studio")
          }
          if (grepl("1_bed", var)) {
            return("1bed")
          }
          if (grepl("2_bed", var)) {
            return("2bed")
          }
          if (grepl("3_bed_plus", var)) {
            return("3+bed")
          }
          if (grepl("total_bed", var)) {
            return("total")
          }
        }
        # Year of construction
        if (grepl("_year$", var)) {
          if (grepl("before_1960", var)) {
            return("<1960")
          }
          if (grepl("1960_1979", var)) {
            return(">1960<1979")
          }
          if (grepl("1980_1999", var)) {
            return(">1980<1999")
          }
          if (grepl("2000_or_later", var)) {
            return(">2000")
          }
          if (grepl("total_year", var)) {
            return("total")
          }
        }
        # Rent ranges
        if (grepl("_rent_range$", var)) {
          if (grepl("less_750", var)) {
            return("<$750")
          }
          if (grepl("750_999", var)) {
            return(">$750<$999")
          }
          if (grepl("1000_1249", var)) {
            return(">$1k<$1.25k")
          }
          if (grepl("1250_1499", var)) {
            return(">$1.25k<$1.5k")
          }
          if (grepl("1500_plus", var)) {
            return(">$1.5k")
          }
          if (grepl("non_market", var)) {
            return("?$")
          }
          if (grepl("total_rent_range", var)) {
            return("total")
          }
        }
      })(var)
      short <- paste("Vac. rate", cat_short)

      out <-
        add_variable(
          variables = scales_variables_modules$variables,
          var_code = var,
          type = "pct",
          var_title = title,
          var_short = short,
          explanation = explanation,
          exp_q5 = NA,
          parent_vec = NA,
          classification = "other",
          theme = "Vacancy rate",
          private = FALSE,
          dates = years,
          avail_scale = "cmhczone",
          source = "Canada Mortgage and Housing Corporation",
          interpolated = tibble::tibble(
            scale = "cmhczone",
            interpolated_from = FALSE
          )
        )

      out[out$var_code == var, ]
    }) |> (\(x) Reduce(rbind, x, init = variables))()

  # Modules table
  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = "cmhczone")

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "vacancy",
      theme = "Housing",
      nav_title = "Vacancy rate",
      title_text_title = "Vacancy rate",
      title_text_main = paste0(
        "<p>The rental vacancy rate measures the percentage of purpose-built ",
        "rental apartments which are vacant and available for rent at a given ",
        "time. Vacancy rates below 3% suggest a serious rental housing crisis."
      ),
      title_text_extra = paste0(
        "<p>The vacancy rate is the most important indicator of rental housing ",
        "availability. A higher rate means more available housing, and a lower ",
        "rate means the opposite. The datasets visualized on this page come ",
        "from the CMHC and the 2021 Canadian Census."
      ),
      metadata = TRUE,
      dataset_info = "<p>The datasets visualized on this page come from the CMHC and the 2021 Canadian Census.",
      var_left = variables[grepl("^vac_rate", variables$var_code), c("var_code", "group_name", "group_diff")],
      dates = years,
      main_dropdown_title = "Vacancy rate distribution",
      default_var = "vac_rate_2_bed_bed",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))
}
