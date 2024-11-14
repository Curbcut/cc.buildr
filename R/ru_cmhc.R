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
  years <- 2010:(as.numeric(format(Sys.Date(), "%Y"))-1)
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
          ),
          schema = list(time = time_regex)
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
          ),
          schema = list(time = time_regex)
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
      dataset_info = "<p>The datasets visualized on this page come from CMHC.",
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

#' @export
ru_starts_completions <- function(scales_variables_modules, crs, geo_uid,
                                  scales_sequences = scales_sequences,
                                  overwrite = FALSE,
                                  inst_prefix = inst_prefix,
                                  large_tables_db = NULL) {

  years <- 2010:(as.numeric(format(Sys.Date(), "%Y"))-1)
  time_regex <- "_\\d{4}$"

  cat <- c(Starts = "housingdev_s", Completions = "housingdev_c",
           `Absorbed Units` = "housingdev_au")
  subcat <- c("Dwelling Type" = "dw", "Intended Market" = "im")
  top_vars <- c(sapply(cat, paste, subcat, sep = "_"))

  # Categories dictionary
  dw_dict <- c(Single = "single",
               `Semi-Detached` = "semi",
               Row = "row",
               Apartment = "apt",
               All = "total")
  im_dict <- c(Unknown = "unkown",
               Homeowner = "homeowner",
               Rental = "rental",
               Condo = "condo",
               `Co-Op` = "coop",
               All = "total")

  unique_vars <- c(sapply(c("housingdev_s_dw", "housingdev_c_dw", "housingdev_au_dw"),
                          paste, dw_dict, sep = "_"),
                   sapply(c("housingdev_s_im", "housingdev_c_im", "housingdev_au_im"),
                          paste, im_dict, sep = "_"))

  # Table of which `sf` CTs census year should be used
  start_years <- seq(2006, max(years), by = 5)
  CTs_geo <- list()
  for (start in start_years) {
    label <- paste0("CA", substr(as.character(start), 3, 4))
    group_years <- years[years >= start & years < (start + 5)]
    if (length(group_years) > 0) CTs_geo[[label]] <- group_years
  }

  scs <- exclude_processed_scales(unique_vars = unique_vars,
                                  scales = scales_variables_modules$scales,
                                  overwrite = overwrite,
                                  inst_prefix = inst_prefix)

  housing_dev <-
    lapply(names(cat), \(cat_name) {
      series <- cat_name
      lapply(names(subcat), \(subcat_name) {
        sapply(names(CTs_geo), \(census_dt) {
          years <- CTs_geo[[census_dt]]
          lapply(years, \(yr) {
            cmhc::get_cmhc(
              survey = "Scss",
              series = cat_name,
              dimension = subcat_name,
              breakdown = "Census Tracts",
              geo_uid = geo_uid,
              year = yr
            ) |> dplyr::mutate(year = yr)
          })
        }, simplify = FALSE, USE.NAMES = TRUE)
      })
    })

  housing_dev_wide <-
    sapply(housing_dev, \(cat) {
      sub_df <-
        sapply(cat, \(subcat) {
          interpolated <-
            sapply(names(subcat), \(census_dt) {
              sub <- subcat[[census_dt]]

              # # Add the pct column
              # sub <- lapply(sub, function(tbl) {
              #   # Add a new column "Percentage of All"
              #   tbl <- do.call(rbind, lapply(
              #     split(tbl, list(tbl$GeoUID, tbl$Year)),
              #     function(group) {
              #       total_value <- group$Value[group$`Dwelling Type` == "All"]
              #       group$Percentage <- ifelse(group$`Dwelling Type` != "All",
              #                                  ifelse(is.na(group$Value) | is.na(total_value),
              #                                         NA, (group$Value / total_value)),
              #                                  NA)
              #       group$Percentage[is.nan(group$Percentage)] <- 0 # Replace NaN (from 0/0) with 0
              #       return(group)
              #     }))
              #
              #   return(tbl)
              # })

              binded <- Reduce(rbind, sub)
              dw <- sum(grepl("Dwelling Type", names(binded)))
              serie <- unique(binded$Series)
              serie <- if (serie == "Starts") "s" else if (serie == "Completions") "c" else "au"

              if (dw) {
                binded$`Dwelling Type` <- dw_dict[as.character(binded$`Dwelling Type`)]
              } else {
                binded$`Intended Market` <- im_dict[as.character(binded$`Intended Market`)]
              }

              # Make it wider
              out <- tidyr::pivot_wider(binded, id_cols = "GeoUID",
                                        names_from = c(if (dw) "Dwelling Type" else "Intended Market", year),
                                        values_from = Value)
              # names(binded)[names(binded) == "Value"] <- "count"
              # names(binded)[names(binded) == "Percentage"] <- "pct"
              # out <- tidyr::pivot_wider(
              #   binded,
              #   id_cols = "GeoUID",
              #   names_from = c(if (dw) "Dwelling Type" else "Intended Market", year),
              #   values_from = c(count, pct),
              #   names_glue = "{`Dwelling Type`}_{.value}_{year}"
              # )

              # Rename cols
              names(out)[2:ncol(out)] <- sapply(names(out)[2:ncol(out)], \(x) {
                paste("housingdev", serie, if (dw) "dw" else "im", x,
                      sep = "_")
              }, USE.NAMES = FALSE)
              col_names <- names(out)[2:ncol(out)]
              # add_col_names <- grep("_count_", col_names, value = TRUE)
              # pct_col_names <- grep("_pct_", col_names, value = TRUE)

              cr <- cancensus::list_census_regions(census_dt)
              region_name <- cr$level[cr$region == geo_uid]
              regions <- list(geo_uid)
              names(regions) <- region_name

              geo <- cancensus::get_census(census_dt, regions = regions, level = "CT",
                                           geo_format = "sf")[c("GeoUID")]

              out <- merge(geo, out)
              names(out)[1] <- "ID"

              # Interpolate to recent CTs
              cc.buildr::interpolate_from_area(to = scales_variables_modules$scales$CT["ID"],
                                               from = out,
                                               additive_vars = col_names,
                                               crs = crs,
                                               round_additive = TRUE) |>
                sf::st_drop_geometry()
            })
          Reduce(\(x, y) merge(x, y, by = "ID", all = TRUE), interpolated)
        }, simplify = FALSE)
      Reduce(\(x, y) merge(x, y, by = "ID", all = TRUE), sub_df)
    }, simplify = FALSE)
  housing_dev_wide <- Reduce(\(x, y) merge(x, y, by = "ID", all = TRUE), housing_dev_wide)
  housing_dev_wide <- tibble::as_tibble(housing_dev_wide)

  # Do the same but for the CMHC zones
  housing_dev_CMHC <-
    lapply(names(cat), \(cat_name) {
      series <- cat_name
      lapply(names(subcat), \(subcat_name) {
        lapply(years, \(yr) {
          cmhc::get_cmhc(
            survey = "Scss",
            series = cat_name,
            dimension = subcat_name,
            breakdown = "Survey Zones",
            geo_uid = geo_uid,
            year = yr
          ) |> dplyr::mutate(year = yr)
        })
      })
    })
  housing_dev_wide_CMHC <-
    sapply(housing_dev_CMHC, \(cat) {
      sub_df <-
        sapply(cat, \(subcat) {

          binded <- Reduce(rbind, subcat)
          dw <- sum(grepl("Dwelling Type", names(binded)))
          serie <- unique(binded$Series)
          serie <- if (serie == "Starts") "s" else if (serie == "Completions") "c" else "au"

          if (dw) {
            binded$`Dwelling Type` <- dw_dict[as.character(binded$`Dwelling Type`)]
          } else {
            binded$`Intended Market` <- im_dict[as.character(binded$`Intended Market`)]
          }

          # Make it wider
          out <- tidyr::pivot_wider(binded, id_cols = "Survey Zones",
                                    names_from = c(if (dw) "Dwelling Type" else "Intended Market", year),
                                    values_from = Value)

          # Rename cols
          names(out)[2:ncol(out)] <- sapply(names(out)[2:ncol(out)], \(x) {
            paste("housingdev", serie, if (dw) "dw" else "im", x,
                  sep = "_")
          }, USE.NAMES = FALSE)
          names(out)[names(out) == "Survey Zones"] <- "name"
          out
        }, simplify = FALSE)
      Reduce(\(x, y) merge(x, y, by = "name", all = TRUE), sub_df)
    }, simplify = FALSE)
  housing_dev_wide_CMHC <- Reduce(\(x, y) merge(x, y, by = "name", all = TRUE), housing_dev_wide_CMHC)
  housing_dev_wide_CMHC <- tibble::as_tibble(housing_dev_wide_CMHC)

  # Interpolate data to all possible scales ---------------------------------

  names(housing_dev_wide)[names(housing_dev_wide) == "ID"] <- "CT_ID"
  vars_years <- names(housing_dev_wide)[names(housing_dev_wide) != "CT_ID"]
  unique_vars <- unique(gsub(time_regex, "", vars_years))

  data_interpolated <-
    interpolate_from_census_geo(
      data = housing_dev_wide,
      base_scale = "CT",
      all_scales = scales_variables_modules$scales,
      weight_by = "area",
      crs = crs,
      additive_vars = vars_years,
      overwrite = overwrite,
      time_regex = time_regex,
      inst_prefix = inst_prefix
    )


  # Use CMHC zones real values
  data_interpolated$scales$cmhczone <-
    data_interpolated$scales$cmhczone[, !names(data_interpolated$scales$cmhczone) %in% vars_years] |>
    merge(housing_dev_wide_CMHC, by = "name", all.x = TRUE)
  data_interpolated$interpolated_ref$interpolated_from[
    data_interpolated$interpolated_ref$scale == "cmhczone"
  ] <- "FALSE"


  # Declare the types of the variables in a named list ----------------------

  types <- sapply(unique_vars, \(x) "count", simplify = FALSE, USE.NAMES = TRUE)


  # Data tibble -------------------------------------------------------------

  data_construct(scales_data = data_interpolated$scales,
                 unique_var = unique_vars,
                 time_regex = time_regex,
                 inst_prefix = inst_prefix,
                 large_tables_db = large_tables_db)


  # Variables table ---------------------------------------------------------

  dw_vars <- grep("_dw_", unique_vars, value = TRUE)
  im_vars <- grep("_im_", unique_vars, value = TRUE)

  variables <-
    lapply(dw_vars, \(var) {

      # ALSO MUST BE ADDED
      parent_vec <- (\(x) {
        # Bedroom types
        if (grepl("_s_", var)) {
          return("housingdev_starts")
        }
        # Year of construction
        if (grepl("_c_", var)) {
          return("housingdev_completions")
        }
        if (grepl("_au_", var)) {
          return("housingdev_absorbedunits")
        }
      })(var)

      # Dwelling type
      type <- stringr::str_extract(var, "dw_.*$")
      type <- gsub("dw_", "", type)
      group_dw <- names(dw_dict)[which(type == dw_dict)]
      group_dw <- stringr::str_to_sentence(group_dw)
      if (group_dw == "Row") group_dw <- "Row house"
      if (group_dw == "Single") group_dw <- "Single-detached"

      # exp_q5
      exp_q5 = tolower(paste("were", group_dw, "dwellings"))
      if (group_dw == "All") exp_q5 <- "were distributed across all dwelling types"

      # Development type
      group_dev <- (\(x) {
        if (grepl("_s_", var)) {
          return("starts")
        }
        if (grepl("_c_", var)) {
          return("completions")
        }
        if (grepl("_au_", var)) {
          return("absorbed units")
        }
      })(var)

      # Create title and explanation
      title <- paste(group_dw, group_dev)
      explanation <- tolower(paste(
        "the count of", group_dw, "housing", group_dev
      ))
      # if (explanation == "the count of all housing absorbed units")
      #   explanation <- "the count of all absorbed housing units"
      explanation <- gsub("housing absorbed units", "housing absorptions",
                          explanation)

      # Create short title
      short <- group_dw

      # Create group_name
      group_name <- (\(x) {
        if (grepl("_s_", var)) {
          return("Housing starts")
        }
        if (grepl("_c_", var)) {
          return("Housing completions")
        }
        if (grepl("_au_", var)) {
          return("Aborbed units")
        }
      })(var)

      # Create group_diff
      group_diff <- list(
        "Dimension" = "Dwelling type",
        "Dwelling type" = group_dw
      )

      out <-
        add_variable(
          variables = scales_variables_modules$variables,
          var_code = var,
          type = "count",
          var_title = title,
          var_short = short,
          explanation = explanation,
          exp_q5 = exp_q5,
          parent_vec = parent_vec,
          classification = "physical",
          theme = "Housing starts and completions",
          private = FALSE,
          pe_include = grepl("total", var),
          dates = years,
          avail_scale = "cmhczone",
          source = "Canada Mortgage and Housing Corporation",
          interpolated = data_interpolated$interpolated_ref,
          group_name = group_name,
          group_diff = group_diff,
          schema = list(time = time_regex)
        )

      out[out$var_code == var, ]
    }) |> (\(x) Reduce(rbind, x, init = scales_variables_modules$variables))()

  variables <-
    lapply(im_vars, \(var) {

      # ALSO MUST BE ADDED
      parent_vec <- (\(x) {
        # Bedroom types
        if (grepl("_s_", var)) {
          return("housingdev_starts")
        }
        # Year of construction
        if (grepl("_c_", var)) {
          return("housingdev_completions")
        }
        if (grepl("_au_", var)) {
          return("housingdev_absorbedunits")
        }
      })(var)

      # Dwelling type
      type <- stringr::str_extract(var, "im_.*$")
      type <- gsub("im_", "", type)
      group_dw <- names(im_dict)[which(type == im_dict)]
      group_dw <- stringr::str_to_sentence(group_dw)

      # exp_q5
      exp_q5 = tolower(paste("were intended to the", group_dw, "market"))
      if (group_dw == "All") exp_q5 <- "were distributed across all intended markets"

      # Development type
      group_dev <- (\(x) {
        if (grepl("_s_", var)) {
          return("starts")
        }
        if (grepl("_c_", var)) {
          return("completions")
        }
        if (grepl("_au_", var)) {
          return("absorbed units")
        }
      })(var)

      # Create title and explanation
      title <- paste(group_dw, group_dev)
      explanation <- tolower(paste(
        "the count of", group_dw, "housing", group_dev
      ))
      # if (explanation == "the count of all housing absorbed units")
      #   explanation <- "the count of all absorbed housing units"
      explanation <- gsub("housing absorbed units", "housing absorptions",
                          explanation)

      # Create short title
      short <- group_dw

      # Create group_name
      group_name <- (\(x) {
        if (grepl("_s_", var)) {
          return("Housing starts")
        }
        if (grepl("_c_", var)) {
          return("Housing completions")
        }
        if (grepl("_au_", var)) {
          return("Aborbed units")
        }
      })(var)

      # Create group_diff
      group_diff <- list(
        "Dimension" = "Intended market",
        "Intended market" = group_dw
      )

      out <-
        add_variable(
          variables = scales_variables_modules$variables,
          var_code = var,
          type = "count",
          var_title = title,
          var_short = short,
          explanation = explanation,
          exp_q5 = exp_q5,
          parent_vec = parent_vec,
          classification = "physical",
          theme = "Housing starts and completions",
          private = FALSE,
          pe_include = grepl("total", var),
          dates = years,
          avail_scale = "cmhczone",
          source = "Canada Mortgage and Housing Corporation",
          interpolated = data_interpolated$interpolated_ref,
          group_name = group_name,
          group_diff = group_diff,
          schema = list(time = time_regex)
        )

      out[out$var_code == var, ]
    }) |> (\(x) Reduce(rbind, x, init = variables))()

  # var_left for the modules table
  var_left <- variables[grepl("^housingdev", variables$var_code),
                        c("var_code", "group_name", "group_diff")]

  variables <-
    add_variable(
      variables = variables,
      var_code = "housingdev_starts",
      type = "count",
      var_title = "Housing starts",
      var_short = "Starts",
      explanation = "the count of housing starts",
      exp_q5 = NA,
      parent_vec = NA,
      classification = "physical",
      theme = "Housing starts and completions",
      private = FALSE,
      pe_include = FALSE,
      dates = years,
      avail_scale = "cmhczone",
      source = "Canada Mortgage and Housing Corporation",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex)
    ) |>
    add_variable(
      var_code = "housingdev_completions",
      type = "count",
      var_title = "Housing completions",
      var_short = "Completions",
      explanation = "the count of housing completions",
      exp_q5 = NA,
      parent_vec = NA,
      classification = "physical",
      theme = "Housing starts and completions",
      private = FALSE,
      pe_include = FALSE,
      dates = years,
      avail_scale = "cmhczone",
      source = "Canada Mortgage and Housing Corporation",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex)
    ) |>
    add_variable(
      var_code = "housingdev_absorbedunits",
      type = "count",
      var_title = "Absorbed housing units",
      var_short = "Absorbed",
      explanation = "the count of absorbed housing units",
      exp_q5 = NA,
      parent_vec = NA,
      classification = "physical",
      theme = "Housing starts and completions",
      private = FALSE,
      pe_include = FALSE,
      dates = years,
      avail_scale = "cmhczone",
      source = "Canada Mortgage and Housing Corporation",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex)
    )


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "housingdev",
      theme = "Housing",
      nav_title = "Housing starts and completions",
      title_text_title = "Housing starts and completions",
      title_text_main = paste0(
        "<p>Housing starts and completions represent the number of new residential",
        " construction projects that have begun or reached completion, segmente",
        "d by dwelling type (such as single-family homes, apartments, or townho",
        "uses) and intended market (e.g., owner-occupied or rental)."
      ),
      title_text_extra = paste0(
        "<p> Monitoring",
        " these figures provides valuable insight into the pace of housing supp",
        "ly growth and market trends. A consistent increase in completions is c",
        "rucial for addressing housing shortages and meeting demand in diverse ",
        "housing markets."
      ),
      metadata = TRUE,
      dataset_info = paste0("<p>The datasets visualized on this page come from ",
                            "CMHC."),
      var_left = var_left,
      dates = years,
      main_dropdown_title = "Housing development phase",
      default_var = "housingdev_s_dw_total",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))

}
