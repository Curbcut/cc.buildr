#' Add a ready to use Can-ALE data and module
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the Can-ALE variable added, its addition
#' in the variables table and the module table.
#' @export
ru_canale <- function(scales_variables_modules, region_DA_IDs, crs) {
  ba_var(
    data = cc.data::db_read_data("canale",
      column_to_select = "DA_ID",
      IDs = region_DA_IDs, crs = crs
    ),
    scales_variables_modules = scales_variables_modules,
    base_scale = "DA",
    weight_by = "households",
    crs = crs,
    average_vars = c("canale_2016"),
    variable_var_code = "canale",
    variable_type = "ind",
    variable_var_title = "Can-ALE index",
    variable_var_short = "Can-ALE",
    variable_explanation = "the potential for active living",
    variable_theme = "Urban life",
    variable_private = FALSE,
    variable_source = "McGill Geo-Social Determinants of Health Research Group",
    module_id = "canale",
    module_theme = "Urban life",
    module_nav_title = "Active living potential",
    module_title_text_title = "Active living potential: the CanALE index",
    module_title_text_main = paste0(
      "The CanALE dataset (developed by Prof. Nancy Ross ",
      "and her team) captures four key elements related to ",
      "active living environments: population density, ",
      "points of interest, street grid, and proximity of transit service."
    ),
    module_title_text_extra = paste0(
      "<p>A safe and inviting pedestrian environment is not ",
      "a given in all neighbourhoods: it is influenced by ",
      "socio-economic factors. The risks of pedestrian ",
      "injuries and fatalities are higher in low-income and ",
      "racialized communities where residents often rely on ",
      "walking as a daily mode of transport but where the ",
      "local environment is not necessarily inviting and ",
      "safe.<p>In addition to evidence pointing towards ",
      "large discrepancies in the provision of walkable ",
      "urban space across income and racial lines, concern ",
      "has been raised with regard to the possible ",
      "gentrification and displacement impacts of ",
      "improved pedestrian infrastructure. In other words, ",
      "who can afford to live in walkable neighbourhoods?",
      "<br><p>Further resources: <ul><li><a href='https://",
      "www150.statcan.gc.ca/n1/pub/82-003-x/2019005/",
      "article/00002-eng.htm' target = '_blank'>Thomas Herrmann, William ",
      "Gleckner, Rania A. Wasfi, Benoît Thierry, Yan ",
      "Kestens and Nancy A. Ross. 2019. 'A pan-Canadian ",
      "measure of active living environments using open ",
      "data. Statistics Canada Health Reports, 82-003-X.",
      "</a><li>Kevin Manaugh, Linnea Soli, Samuel Kohn, ",
      "Robin Basalaev-Binder, Ty Tuff, David Wachsmuth. ",
      "2020. 'Montreal's response to COVID-19: An equity ",
      "analysis of new active transport infrastructure.' ",
      "Transportation Research Board working paper. ",
      "<b>(MSSI research)</b></ul><br><p><i>Module lead ",
      "authors: David Wachsmuth, Robin Basalaev-Binder</i>"
    ),
    module_metadata = TRUE,
    module_dataset_info =
      paste0(
        "<p><a target = '_blank' href = 'https://nancyrossresearchgroup.ca/",
        "research/can-ale/'>",
        "The Canadian Active Living Environments (Can-ALE)</a> dataset is ",
        "a geographic-based set of measures charac",
        "terizing the active living environments (often referred to as '",
        "walkability') of Canadian communities. The data is provided at ",
        "the dissemination area level.</p>"
      )
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
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the Can-BICS variable added, its addition
#' in the variables table and the module table.
#' @export
ru_canbics <- function(scales_variables_modules, region_DA_IDs, crs) {
  ba_var(
    data = cc.data::db_read_data("canbics",
      column_to_select = "DA_ID",
      IDs = region_DA_IDs, crs = crs
    ),
    scales_variables_modules = scales_variables_modules,
    base_scale = "DA",
    weight_by = "households",
    crs = crs,
    average_vars = c("canbics_2021"),
    variable_var_code = "canbics",
    variable_type = "ind",
    variable_var_title = "Can-BICS metric",
    variable_var_short = "Can-BICS",
    variable_explanation = "the bikeway comfort and safety classification system",
    variable_theme = "Transport",
    variable_private = FALSE,
    variable_source = "Meghan Winters at Faculty of Health Sciences, Simon Fraser University",
    module_id = "canbics",
    module_theme = "Transport",
    module_nav_title = "Bikeway comfort and safety",
    module_title_text_title = "Bikeway comfort and safety: the Can-BICS index",
    module_title_text_main = paste0(
      "Can-BICS, or Canadian Bikeway Comfort and Safety, is a classification ",
      "system for cycling infrastructure in Canada. This system is based on ",
      "three tiers that considers safety and user comfort: high-comfort bikeways, ",
      "medium-comfort bikeways, and low-comfort bikeways."
    ),
    module_title_text_extra = paste0(
      "The information seen in this module is based on data from CANUE. ",
      "Can-BICS was developed by Meghan Winters, PhD, Moreno Zanotto, MSc, ",
      "and Gregory Butler, MSc. In selecting different areas on the map, ",
      "insights can be gained about the type and quality of cycling infrastructure. ",
      "Understanding the spatialization of cycling ",
      "infrastructure as classified by Can-BICS can help to highlight the ",
      "availability and infrastructure types across the region and ",
      "support efforts in improving bikeways. <p>For more information about ",
      "Can-BICS visit: <a target = '_blank' href='https://www.canada.ca/en/public-health/services/",
      "reports-publications/health-promotion-chronic-disease-prevention-canada-",
      "research-policy-practice/vol-40-no-9-2020/canbics-classification-system-",
      "naming-convention-cycling-infrastructure.html'>At-a-glance – The Canadi",
      "an Bikeway Comfort and Safety (Can-BICS) Classification System: a commo",
      "n naming convention for cycling infrastructure</a>."
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
        "The data is provided at the dissemination area level.</p>"
      )
  )
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
#' @param approximate_name_match <`logical`> CMHC zone naming can be different
#' year to year (A single typo, or other). Should the function search for
#' approximate matches to the name of the `cmhczone` table in
#' `scales_variables_modules`? Useful for Montreal where names are more or less
#' unique and so an approximate match can be beneficial. less for Toronto where
#' the name `York` is used in many different names.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the CMHC's vacancy rate variables added,
#' their addition in the variables table and the module table.
#' @export
ru_vac_rate <- function(scales_variables_modules, crs, geo_uid,
                        approximate_name_match = TRUE) {
  # Relevant dimensions
  dimensions <-
    c("Bedroom Type", "Year of Construction", "Rent Ranges")
  dimensions_short <-
    c("bed", "year", "rent_range")

  # Retrieval
  cmhc <-
    sapply(2010:2021, \(yr) {
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
          names(out) <- paste("vac_rate", y, names(out), yr, sep = "_")
          names(out)[1] <- "name"

          # Change the name to the closest string in the CMHC zone scale
          out <- out[!is.na(out$name), ]
          if (approximate_name_match) {
            out$name <-
              sapply(out$name,
                agrep,
                x = scales_variables_modules$scales$cmhc$cmhczone$name,
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

  merged <-
    Reduce(\(x, y) merge(x, y, by = "name", all.x = TRUE),
      cmhc,
      init = sf::st_drop_geometry(
        scales_variables_modules$scales$cmhc$cmhczone
      )[, "name"]
    )

  # Variables
  vars <- names(merged)[!grepl("name|ID|geometry", names(merged))]
  unique_vars <- unique(gsub("_\\d{4}$", "", vars))

  # Append data
  scales_variables_modules$scales$cmhc$cmhczone <-
    merge(scales_variables_modules$scales$cmhc$cmhczone,
      merged,
      by = "name"
    )

  # Calculate breaks
  with_breaks <-
    calculate_breaks(
      all_scales = scales_variables_modules$scales,
      vars = vars,
      time_regex = "\\d{4}"
    )

  # Add to the variables table
  variables <-
    lapply(unique_vars, \(var) {
      # Create title and explanation
      cat_title <- (\(x) {
        # Bedroom types
        if (grepl("_bed_", var)) {
          if (grepl("bachelor$", var)) {
            return("studio apartments")
          }
          suff <- "housing units"
          if (grepl("1_bed$", var)) {
            return(paste("one-bedroom", suff))
          }
          if (grepl("2_bed$", var)) {
            return(paste("two-bedroom", suff))
          }
          if (grepl("3_bed_plus$", var)) {
            return(paste("three-bedroom and larger", suff))
          }
          if (grepl("bed_total$", var)) {
            return(paste("all", suff))
          }
        }
        # Year of construction
        if (grepl("_year_", var)) {
          pre <- "housing units built"
          if (grepl("before_1960$", var)) {
            return(paste(pre, "before 1960"))
          }
          if (grepl("1960_1979$", var)) {
            return(paste(pre, "between 1960 and 1979"))
          }
          if (grepl("1980_1999$", var)) {
            return(paste(pre, "between 1980 and 1999"))
          }
          if (grepl("2000_or_later$", var)) {
            return(paste(pre, "after 2000"))
          }
          if (grepl("year_total$", var)) {
            return(paste("all housing units"))
          }
        }
        # Rent ranges
        if (grepl("_rent_range_", var)) {
          pre <- "housing units with a rent"
          if (grepl("less_750$", var)) {
            return(paste(pre, "below $750"))
          }
          if (grepl("750_999$", var)) {
            return(paste(pre, "between $750 and $999"))
          }
          if (grepl("1000_1249$", var)) {
            return(paste(pre, "between $1,000 and $1,249"))
          }
          if (grepl("1250_1499$", var)) {
            return(paste(pre, "between $1,250 and $1,499"))
          }
          if (grepl("1500_plus$", var)) {
            return(paste(pre, "higher than $1,500"))
          }
          if (grepl("non_market$", var)) {
            return(paste("housing units with an unknown rent"))
          }
          if (grepl("rent_range_total$", var)) {
            return(paste("all housing units"))
          }
        }
      })(var)
      title <- paste("Vacancy rate in", cat_title)
      explanation <- paste(
        "the percentage of all available",
        gsub("^all ", "", cat_title),
        "in a rental property that are vacant or unoccupied"
      )

      # Create short title
      cat_short <- (\(x) {
        # Bedroom types
        if (grepl("_bed_", var)) {
          if (grepl("bachelor$", var)) {
            return("studio")
          }
          if (grepl("1_bed$", var)) {
            return("1bed")
          }
          if (grepl("2_bed$", var)) {
            return("2bed")
          }
          if (grepl("3_bed_plus$", var)) {
            return("3+bed")
          }
          if (grepl("bed_total$", var)) {
            return("total")
          }
        }
        # Year of construction
        if (grepl("_year_", var)) {
          if (grepl("before_1960$", var)) {
            return("<1960")
          }
          if (grepl("1960_1979$", var)) {
            return(">1960<1979")
          }
          if (grepl("1980_1999$", var)) {
            return(">1980<1999")
          }
          if (grepl("2000_or_later$", var)) {
            return(">2000")
          }
          if (grepl("year_total$", var)) {
            return("total")
          }
        }
        # Rent ranges
        if (grepl("_rent_range_", var)) {
          if (grepl("less_750$", var)) {
            return("<$750")
          }
          if (grepl("750_999$", var)) {
            return(">$750<$999")
          }
          if (grepl("1000_1249$", var)) {
            return(">$1k<$1.25k")
          }
          if (grepl("1250_1499$", var)) {
            return(">$1.25k<$1.5k")
          }
          if (grepl("1500_plus$", var)) {
            return(">$1.5k")
          }
          if (grepl("non_market$", var)) {
            return("?$")
          }
          if (grepl("rent_range_total$", var)) {
            return("total")
          }
        }
      })(var)
      short <- paste("Vac. rate", cat_short)

      # Create group_name
      cat_group_name <- (\(x) {
        # Bedroom types
        if (grepl("_bed_", var)) {
          return("Bedroom type")
        }
        # Year of construction
        if (grepl("_year_", var)) {
          return("Year of construction")
        }
        # Rent ranges
        if (grepl("_rent_range_", var)) {
          return("Rent range")
        }
      })(var)
      group_name <- paste("Vacancy rate by", tolower(cat_group_name))

      # Create group_diff
      group_diff <- list(paste("For", cat_title))
      names(group_diff) <- cat_group_name

      # Include in place exporer
      pe_include <-
        if (grepl("2_bed$|2000_or_later$|1500_plus$", var)) TRUE else FALSE

      out <-
        add_variable(
          variables = scales_variables_modules$variables,
          var_code = var,
          type = "pct",
          var_title = title,
          var_short = short,
          explanation = explanation,
          theme = "Housing",
          private = FALSE,
          pe_include = pe_include,
          dates = with_breaks$avail_dates[[var]],
          scales = tibble::tibble(geo = "cmhc", scale = "zone"),
          breaks_q3 = with_breaks$q3_breaks_table[[var]],
          breaks_q5 = with_breaks$q5_breaks_table[[var]],
          source = "Canada Mortgage and Housing Corporation",
          interpolated = tibble::tibble(
            geo = "cmhc", scale = "zone",
            interpolated_from = FALSE
          ),
          group_name = group_name,
          group_diff = group_diff
        )

      out[out$var_code == var, ]
    }) |> (\(x) Reduce(rbind, x, init = scales_variables_modules$variables))()


  # Create a module
  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "vac_rate",
      theme = "Housing",
      nav_title = "Vacancy rate",
      title_text_title = "Vacancy rate",
      title_text_main = paste0(
        "Examining residential rental vacancy rates is an important part of ",
        "understanding the housing landscape in a region. Information ",
        "about vacancy rates and its related variables can help define past ",
        "and current trends in the housing market and what is needed to ",
        "better provide adequate rental housing."
      ),
      title_text_extra = paste0(
        "The comparative analysis seen in this module is based on data from ",
        "the CMHC. In selecting different options from the drop-down menus, ",
        "insights can be gained on how vacancy rates vary over time and spatially ",
        "by type of unit, year of construction, and rent range."
      ),
      regions = "cmhc",
      metadata = TRUE,
      dataset_info = "TKTK"
    )


  # Return ------------------------------------------------------------------

  with_breaks$scales <- cc.buildr::reorder_columns(with_breaks$scales)

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = modules
  ))
}
