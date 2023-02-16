#' Build and append census data
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param census_vectors <`character vector`> Data variables that should be added
#' to the scales. By default, all: \code{\link[cc.data]{census_vectors}}. Look
#' at the \code{\link[cc.data]{census_vectors_table}} to view all
#' variables explained.
#' @param census_years <`character vector`> Years for which the census data
#' should be added to the scales. Defaults to \code{\link[cc.data]{census_years}}
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param housing_module <`logical`> Should a housing module be added to
#' the list of modules.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with census variable added, their addition
#' in the variables table and the module table.
#' @export
ba_census_data <- function(scales_variables_modules,
                           region_DA_IDs,
                           census_vectors = cc.data::census_vectors,
                           census_years = cc.data::census_years,
                           crs,
                           housing_module = TRUE) {
  # Declare all variables from the census -----------------------------------

  vars <-
    sapply(census_vectors,
      \(x) paste(x, census_years, sep = "_"),
      simplify = FALSE, USE.NAMES = FALSE
    ) |> unlist()

  unique_var <- census_vectors


  # Build census data for all possible scales -------------------------------

  census_dat <- build_census_data(
    scales_consolidated = scales_variables_modules$scales,
    region_DA_IDs = region_DA_IDs,
    census_vectors = census_vectors,
    census_years = census_years,
    crs = crs
  )


  # Calculate breaks --------------------------------------------------------

  with_breaks <-
    calculate_breaks(
      all_scales = census_dat$scales,
      vars = vars
    )


  # Variables table ---------------------------------------------------------

  variables <-
    lapply(unique_var, \(u_var) {
      out <- add_variable(
        variables = scales_variables_modules$variables,
        var_code = u_var,
        type = cc.data::census_vectors_table$type[
          cc.data::census_vectors_table$var_code == u_var
        ],
        var_title = cc.data::census_vectors_table$var_title[
          cc.data::census_vectors_table$var_code == u_var
        ],
        var_short = cc.data::census_vectors_table$var_short[
          cc.data::census_vectors_table$var_code == u_var
        ],
        explanation = cc.data::census_vectors_table$explanation[
          cc.data::census_vectors_table$var_code == u_var
        ],
        theme = cc.data::census_vectors_table$theme[
          cc.data::census_vectors_table$var_code == u_var
        ],
        private = FALSE,
        dates = with_breaks$avail_dates[[u_var]],
        scales = census_dat$avail_scales,
        breaks_q3 = with_breaks$q3_breaks_table[[u_var]],
        breaks_q5 = with_breaks$q5_breaks_table[[u_var]],
        source = "Canadian census",
        interpolated = census_dat$interpolated_ref
      )
      out[out$var_code == u_var, ]
    })

  variables <- Reduce(rbind, variables, init = scales_variables_modules$variables)


  # Modules table -----------------------------------------------------------

  modules <-
    if (housing_module) {
      scales_variables_modules$modules |>
        add_module(
          id = "housing",
          theme = "Housing",
          nav_title = "Housing system",
          title_text_title = "The housing system",
          title_text_main = paste0(
            "Housing is at the centre of our lives. Our ability to find affordable, ",
            "adequate and healthy accommodations profoundly affects our life ",
            "chances."
          ),
          title_text_extra = paste0(
            "<p>Access to affordable and adequate housing is a core element of ",
            "social equity in cities. In Canada, the National Housing Strategy aims ",
            "to housing needs and houselessness through modernization, new ",
            "construction, and innovation and research. Within the City of Montreal, ",
            "important housing initiatives include the Diverse Metropolis by-law and ",
            "the 12,000 housing unit strategy. <p>This module presents housing data ",
            "from the Census from 1996 to the present, and explores relationships ",
            "with demographic patterns.<br><p><i>Further reading:</i></p><ul><li>",
            "<a href = 'https://www.cmhc-schl.gc.ca/en/nhs/'>CMHC. (n.d.). National ",
            "Housing Strategy.</a><li><a href ='https://montreal.ca/articles/",
            "metropole-mixte-les-grandes-lignes-du-reglement-7816'>Ville de ",
            "Montréal. (4 octobre 2021). Métropole Mixte: Les grandes lignes du ",
            "règlement.</a><li>Madden, D., & Marcuse, P. (2016). <i>In Defense of ",
            "Housing: The Politics of Crisis</i>. New York and London: Verso ",
            "Books.</ul>"
          ),
          regions = unique(census_dat$avail_scales$geo),
          metadata = TRUE,
          dataset_info = paste0(
            "<p>This module presents <a href = 'https://www.statcan",
            ".gc.ca/en/census/census-engagement/about'>housing data",
            " from the 1996 to the latest, Canadian Censuses</a></p>"
          )
        )
    } else {
      scales_variables_modules$modules
    }


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = modules
  ))
}
