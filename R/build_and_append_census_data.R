#' Build and append census data
#'
#' @param scales_variables_modules <`names list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
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
                           crs,
                           housing_module = TRUE) {

  # Declare all variables from the census -----------------------------------

  vars <-
    sapply(susdata::census_vectors$var_code,
      \(x) paste(x, susdata::census_years, sep = "_"),
      simplify = FALSE, USE.NAMES = FALSE
    ) |> unlist()

  unique_var <- susdata::census_vectors$var_code


  # Build census data for all possible scales -------------------------------

  # TKTK THIS FOLLOWING DATA MUST BE RETRIEVED THROUGH AN AWS BUCKET OR SMTH
  qs::qload("census.qsm")

  census_dat <- build_census_data(
    scales_consolidated = scales_consolidated,
    census_data = census_data,
    census_data_raw_DA = data_raw$DA,
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
      out <- cc.buildr::add_variable(
        variables = scales_variables_modules$variables,
        var_code = u_var,
        type = susdata::census_vectors$type[
          susdata::census_vectors$var_code == u_var
        ],
        var_title = susdata::census_vectors$var_title[
          susdata::census_vectors$var_code == u_var
        ],
        var_short = susdata::census_vectors$var_short[
          susdata::census_vectors$var_code == u_var
        ],
        explanation = susdata::census_vectors$explanation[
          susdata::census_vectors$var_code == u_var
        ],
        theme = susdata::census_vectors$theme[
          susdata::census_vectors$var_code == u_var
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
        cc.buildr::add_module(
          id = "housing",
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
