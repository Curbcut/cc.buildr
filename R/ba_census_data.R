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
#' @param scales_to_interpolate <`character vector`> Scales for which census
#' data should be interpolated (e.g. very small scales like 25m grid cells should be excluded.).
#' Defaults to all the scales except building and street.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param DB_table <`named list`> Named list of all the years for which DBs existed.
#' Every dataframe in the list must have the DB ID, DA ID, population and dwellings
#' count. It is created using \code{\link[cc.data]{DB_get}}.
#' @param housing_module <`logical`> Should a housing module be added to
#' the list of modules.
#' @param age_module <`logical`> Should an age module be added to
#' the list of modules.
#' @param overwrite <`logical`> Should the data already processed and stored be
#' overwriten?
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with census variable added, their addition
#' in the variables table and the module table.
#' @export
ba_census_data <- function(scales_variables_modules,
                           region_DA_IDs,
                           census_vectors = cc.data::census_vectors,
                           census_years = cc.data::census_years,
                           scales_to_interpolate = {
                             names(scales_variables_modules$scales)[
                               !names(scales_variables_modules$scales) %in% c(
                                 "building", "street", "DB", "grd30", "grd60", "grd120",
                                 "grd300")
                             ]
                           },
                           scales_sequences,
                           crs,
                           DB_table,
                           housing_module = TRUE,
                           age_module = TRUE,
                           householdsize_module = TRUE,
                           citizenship_module = TRUE,
                           dwellingchar_module = TRUE,
                           education_module = TRUE,
                           language_module = TRUE,
                           overwrite = FALSE,
                           inst_prefix,
                           large_tables_db = NULL) {

  # Declare all variables from the census -----------------------------------

  unique_var <- cc.data::census_add_parent_vectors(census_vectors)

  vars <-
    sapply(unique_var,
           \(x) paste(x, census_years, sep = "_"),
           simplify = FALSE, USE.NAMES = FALSE
    ) |> unlist()

  # Only keep vars for which we know there is data
  no_data <- sapply(cc.data::census_vectors_details$vec, \(x) all(is.na(unlist(x))))
  vars <- vars[!vars %in% cc.data::census_vectors_details$var_code[no_data]]

  time_regex <- "_\\d{4}$"


  # Which scales should be recalculated -------------------------------------

  scales_to_interpolate_exc <- exclude_processed_scales(unique_vars = unique_var,
                                                        scales = scales_to_interpolate,
                                                        overwrite = overwrite,
                                                        inst_prefix = inst_prefix)


  # Build census data for all possible scales -------------------------------

  census_dat <- build_census_data(
    scales_consolidated = scales_variables_modules$scales,
    region_DA_IDs = region_DA_IDs,
    census_vectors = census_vectors,
    census_years = census_years,
    crs = crs,
    DB_table = DB_table,
    scales_to_interpolate = scales_to_interpolate_exc
  )


  # Data tibble -------------------------------------------------------------

  data_construct(scales_data = census_dat$scales,
                 unique_var = unique_var,
                 time_regex = time_regex,
                 inst_prefix = inst_prefix,
                 large_tables_db = large_tables_db)


  # Add population and households of dissemination blocks to the db --------

  if (!all(c("DB_private_households", "DB_c_population" ) %in%
           cc.buildr::db_list_tables_of_scale(inst_prefix, "DB")) | overwrite) {

    recent_DB <- DB_table[[which.max(names(DB_table))]]
    previous_DB <- DB_table[-which.max(names(DB_table))]
    DB_table_interpolated <- DB_table

    for (i in names(previous_DB)) {
      DB_table_interpolated[[i]] <-
        interpolate_from_area(to = recent_DB, from = DB_table_interpolated[[i]],
                              additive_vars = c("dwellings", "population"),
                              round_additive = TRUE, crs = 32618)
    }

    all_cols <- mapply(\(year, df) {
      df <- sf::st_drop_geometry(df)
      names(df)[names(df) == "dwellings"] <-
        sprintf("private_households_%s", year)
      names(df)[names(df) == "population"] <-
        sprintf("c_population_%s", year)
      df[c(sprintf("private_households_%s", year),
           sprintf("c_population_%s", year))]
    }, names(DB_table_interpolated), DB_table_interpolated, SIMPLIFY = FALSE)

    data_binded <- cbind(DB_table_interpolated$`2021`["ID"], Reduce(cbind, all_cols)) |>
      tibble::as_tibble()

    data_construct(scales_data = list(DB = data_binded),
                   unique_var = c("private_households", "c_population"),
                   time_regex = time_regex,
                   inst_prefix = inst_prefix)

  }


  # Variables table ---------------------------------------------------------

  # Vectorized check for presence of every scale in cc.data::census_scales
  interpolated_from_vector <- ifelse(scales_to_interpolate %in% cc.data::census_scales, FALSE, "DA")

  # Combining into a data frame (tibble)
  interpolated_ref <- tibble::tibble(scale = scales_to_interpolate,
                                     interpolated_from = interpolated_from_vector)

  variables <-
    lapply(unique_var, \(u_var) {

      # THIS IS NOT TRUE. WHICH VARIABLE ISN'T AVAILABLE AT ALL DATES?
      dates <- vars[grepl(sprintf("%s%s", u_var, time_regex), vars)]
      dates <- curbcut::s_extract_all(time_regex, dates)
      dates <- gsub("^_", "", dates)

      # Include in place explorer
      pe_include <- if (u_var %in% cc.data::census_vectors_table$parent_vec) FALSE else TRUE
      # Only include larger brackets of age
      if (pe_include) pe_include <- !grepl("^age_", u_var) || u_var %in% c("age_0_14", "age_15_64", "age_65_plus")

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
        exp_q5 = cc.data::census_vectors_table$exp_q5[
          cc.data::census_vectors_table$var_code == u_var
        ],
        parent_vec = cc.data::census_vectors_table$parent_vec[
          cc.data::census_vectors_table$var_code == u_var
        ],
        classification = "sociodemo",
        theme = cc.data::census_vectors_table$theme[
          cc.data::census_vectors_table$var_code == u_var
        ],
        private = FALSE,
        pe_include = pe_include,
        dates = dates,
        avail_scale = scales_to_interpolate,
        source = "Canadian census",
        interpolated = interpolated_ref,
        rankings_chr = cc.data::census_vectors_table$rankings_chr[
          cc.data::census_vectors_table$var_code == u_var
        ][[1]],
        schema = list(time = time_regex)
      )
      out[out$var_code == u_var, ]
    })

  variables <- Reduce(rbind, variables, init = scales_variables_modules$variables)

  # So it works better with the explore panel
  variables$explanation_nodet[variables$var_code == "inc_median_income"] <-
    gsub(
      "income$", "incomes",
      variables$explanation_nodet[variables$var_code == "inc_median_income"]
    )
  variables$explanation_nodet[variables$var_code == "inc_limat"] <-
    gsub(
      "prevalence of low income", "prevalence of low incomes",
      variables$explanation_nodet[variables$var_code == "inc_limat"]
    )

  # Change the classification for dwelling characteristics
  variables$classification[
    variables$theme %in% c("Building typology", "Dwelling size",
                           "Housing period of construction")] <- "physical"


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = scales_to_interpolate)


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
            "<p>Housing is at the centre of our lives. Our ability to find affordable,",
            " adequate, and healthy accommodations profoundly affects our life chan",
            "ces."
          ),
          title_text_extra = paste0(
            "<p>The datasets visualized on this page come from the Canadian Census fro",
            "m 1996 to the present. There are a few efforts in place to better the ",
            "housing landscape from the federal and municipal governments. In Canad",
            "a, the National Housing Strategy aims to address housing needs and hou",
            "selessness through modernization, new construction, and innovation and",
            " research. Within the City of Montreal, important housing initiatives ",
            "include the Diverse Metropolis by-law and the 12,000-housing unit stra",
            "tegy. For more information on these initiatives visit:<ul><li><a href=",
            "'https://www.cmhc-schl.gc.ca/en/nhs/', target = '_blank'>CMHC. (n.d.).",
            " National Housing Strategy</a><li><a href='https://montreal.ca/article",
            "s/metropole-mixte-les-grandes-lignes-du-reglement-7816', target = '_bl",
            "ank'>Ville de Montréal. (4 octobre 2021). Métropole Mixte: Les grandes",
            " lignes du règlement.</a>"
          ),
          metadata = TRUE,
          dataset_info = paste0(
            "<p>This page presents <a href = 'https://www.statcan",
            ".gc.ca/en/census/census-engagement/about'>housing data",
            " from the 1996 to the latest, Canadian Censuses</a></p>"
          ),
          var_left = unique_var[grepl("^housing_", unique_var)],
          dates = census_years,
          main_dropdown_title = NA,
          default_var = "housing_tenant",
          avail_scale_combinations = avail_scale_combinations
        )
    } else {
      scales_variables_modules$modules
    }

  modules <-
    if (citizenship_module) {
      modules |>
        add_module(
          id = "citizenship",
          theme = "Demographics",
          nav_title = "Citizenship and identity",
          title_text_title = "Citizenship and Immigration Status",
          title_text_main = paste0(
            "<p>Understanding the distribution of citizenship and immigration status ",
            "is crucial for comprehending the diversity of the population. "
          ),
          title_text_extra = paste0(
            "<p>The datasets visualized on this page come from the Canadian Census ",
            "from 1996 to the present. These datasets provide insights into trends related ",
            "to immigration and citizenship, and the changing demographics over time."
          ),
          metadata = TRUE,
          dataset_info = paste0(
            "<p>This page presents <a href='https://www.statcan.gc.ca/en/census/census-engagement/about'>",
            "data related to citizenship and immigration status from the 1996 to the latest ",
            "Canadian Censuses</a></p>"
          ),
          var_left = unique_var[grepl("^citizenship_", unique_var)],
          dates = census_years,
          main_dropdown_title = NA,
          default_var = "citizenship_cit",
          avail_scale_combinations = avail_scale_combinations
        )
    } else {
      modules
    }


  # Dwelling characteristics modules table  ---------------------------------

  if (dwellingchar_module) {
    dweeling_char <- variables$var_code[
      variables$theme %in% c("Building typology", "Dwelling size",
                             "Housing period of construction")]

    for (var in dweeling_char) {
      theme <- variables$theme[variables$var_code == var]
      variables$group_name[variables$var_code == var] <- theme

      diff <- list(variables$var_title[variables$var_code == var])
      names(diff) <- theme

      variables$group_diff[
        variables$var_code == var
      ] <- list(diff)
    }

    modules <-
      add_module(
        modules,
        id = "dwelling",
        theme = "Housing",
        nav_title = "Dwelling characteristics",
        title_text_title = "Dwelling characteristics",
        title_text_main = paste0(
          "<p>Understanding the characteristics of dwellings, such as building typology, ",
          "number of bedrooms, and period of construction, is essential for comprehending ",
          "housing availability, suitability, and trends over time. These characteristics ",
          "provide insights into the types of homes people live in, the adequacy of space ",
          "for families, and the historical development of residential areas."
        ),
        title_text_extra = paste0(
          "<p>The datasets visualized on this page come from the Canadian Census from 1996 ",
          "to the present, allowing for an analysis of how dwelling characteristics have ",
          "evolved over time. Key features include the diversity in building types, the ",
          "availability of various sizes of housing (e.g., number of bedrooms), and the ",
          "age of the building stock, which all play a crucial role in meeting housing needs."
        ),
        metadata = TRUE,
        dataset_info = paste0(
          "<p>This page presents <a href='https://www.statcan.gc.ca/en/census/census-engagement/about'>",
          "data related to dwelling characteristics from the 1996 to the latest Canadian Censuses</a>. ",
          "The variables included allow for detailed insights into building typology, ",
          "dwelling sizes, and construction periods.</p>"
        ),
        var_left = variables[variables$theme %in% c("Building typology", "Dwelling size",
                                                    "Housing period of construction"),
                             c("var_code", "group_name", "group_diff")
        ],
        main_dropdown_title = "Dwelling characteristic",
        dates = census_years,
        default_var = "buildingage_1960constr",
        avail_scale_combinations = avail_scale_combinations
      )
  }

  # Language modules table  --------------------------------- -----------

  if (language_module) {
    language_vecs <- variables$var_code[
      variables$theme %in% c("Language")]

    for (var in language_vecs) {
      home <- grepl("lang_home_", var)
      theme <- if (home) "Language most often spoken at home" else "Knowledge of official languages"
      variables$group_name[variables$var_code == var] <- theme

      diff <- list(variables$var_title[variables$var_code == var])
      names(diff) <- theme

      variables$group_diff[
        variables$var_code == var
      ] <- list(diff)
    }

    modules <-
      add_module(
        modules,
        id = "language",
        theme = "Demographics",
        nav_title = "Language characteristics",
        title_text_title = "Language characteristics",
        title_text_main = paste0(
          "<p>Understanding language characteristics, including languages spoken ",
          "at home and knowledge of official languages, helps in comprehending ",
          "cultural diversity and communication needs within communities. It ",
          "provides insights into the linguistic landscape, language preferences, ",
          "and the level of multilingualism across different regions."
        ),
        title_text_extra = paste0(
          "<p>The datasets visualized on this page come from the Canadian Census ",
          "from 1996 to the present, allowing for an analysis of how language ",
          "usage has evolved over time. Key features include the prevalence of ",
          "different languages spoken at home and the knowledge of Canada's ",
          "official languages (English and French), as defined by Statistics ",
          "Canada. The definitions and classifications used here are those ",
          "provided by Statistics Canada, which include both English and French ",
          "as official languages."
        ),
        metadata = TRUE,
        dataset_info = paste0(
          "<p>This page presents <a href='https://www.statcan.gc.ca/en/census/census-engagement/about'>",
          "data related to languages spoken at home and knowledge of official languages from the 1996 to the latest Canadian Censuses</a>. ",
          "The variables included provide detailed insights into linguistic diversity and multilingual proficiency in Canada, according to ",
          "the definitions used by Statistics Canada.</p>"
        ),
        var_left = variables[variables$theme %in% c("Language"),
                             c("var_code", "group_name", "group_diff")
        ],
        main_dropdown_title = "Language characteristic",
        dates = census_years,
        default_var = "lang_french_eng",
        avail_scale_combinations = avail_scale_combinations
      )
  }

  # Age page and data formatting --------------------------------------------

  svm <-
    list(
      scales = scales_variables_modules$scales,
      variables = variables,
      modules = modules
    )
  if (age_module) {
    svm <- ba_age(scales_variables_modules = svm, scales_sequences = scales_sequences,
                  scales_to_interpolate = scales_to_interpolate,
                  overwrite = overwrite, inst_prefix = inst_prefix,
                  large_tables_db = large_tables_db)
  }
  if (householdsize_module) {
    svm <- ba_householdsize(scales_variables_modules = svm,
                            scales_sequences = scales_sequences,
                            scales_to_interpolate = scales_to_interpolate,
                            overwrite = overwrite, inst_prefix = inst_prefix,
                            large_tables_db = large_tables_db)
  }
  if (education_module) {
    svm <- ba_education(scales_variables_modules = svm,
                        scales_sequences = scales_sequences,
                        scales_to_interpolate = scales_to_interpolate,
                        overwrite = overwrite, inst_prefix = inst_prefix,
                        large_tables_db = large_tables_db)
  }


  # Return ------------------------------------------------------------------

  return(svm)
}
