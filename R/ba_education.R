#' Build and append education from census data
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param scales_to_interpolate <`character vector`> WILL NOT be used for calculations.
#' Simply used to inform the variables table. ALL scales holding census `age` data
#' will be used for calculation of the age page data.
#' @param overwrite <`logical`> Should the data already precessed and stored be
#' overwriten?
#' @param inst_prefix <`character`> The prefix of the instance, e.g. `'mtl'` which
#' is the database schema in which the data is saved.
#'
#' @return A list of length 4, similar to the one fed to
#' `scales_variables_modules` with education data added, their addition
#' in the variables table and the module table.
#' @export
ba_education <- function(scales_variables_modules, scales_sequences,
                         scales_to_interpolate, overwrite = FALSE,
                         inst_prefix, large_tables_db) {
  # Declare all variables from the census -----------------------------------

  time_regex <- "_\\d{4}$"

  edu_dat <- cc.data::census_vectors_education$var_code
  edu_dat <- edu_dat[grepl("edu_", edu_dat)]

  edu_dat_n <- seq_along(edu_dat)
  names(edu_dat_n) <- edu_dat

  # Function to generate all sequences
  generate_sequences <- function(data) {
    all_sequences <- list()
    sequence_id <- 1

    for (i in 1:length(data)) {
      for (j in i:length(data)) {
        all_sequences[[sequence_id]] <- data[i:j]
        sequence_id <- sequence_id + 1
      }
    }

    return(all_sequences)
  }

  # Generate all sequences
  vars <- generate_sequences(edu_dat)

  names(vars) <- sapply(vars, \(x) {
    if (length(x) == 1) return(x)
    end <- gsub("edu_", "", x[[length(x)]])

    sprintf("%s_to_%s", x[[1]], end)
  })


  # Build census data for all possible scales -------------------------------

  # Add all age data to the scales
  svm_scales <- scales_variables_modules$scales

  # Exclude scales that already have data
  unique_var <-  names(vars)
  unique_var <- c(paste0(unique_var, "_pct"), paste0(unique_var, "_count"))

  svm_scales <- svm_scales[exclude_processed_scales(unique_var, overwrite = overwrite,
                                                    names(svm_scales), inst_prefix = inst_prefix)]

  # Build the scales as they have 'age'
  svm_scales <- mapply(\(scale_name, scale_df) {

    all_files <- list.files(paste0("data/", scale_name, "/"), full.names = TRUE)
    education_files <- all_files[grepl(sprintf("%s/edu_.", scale_name), all_files)]

    if (length(education_files) == 0) return(scale_df)

    # Add c_population
    education_files <- c(education_files, paste0("data/", scale_name, "/population_15plus.qs"))

    education_data <- lapply(education_files, qs::qread)
    education_data <- Reduce(\(x, y) merge(x, y, by = "ID"), education_data)

    merge(scale_df, education_data, by = "ID")

  }, names(svm_scales), svm_scales)

  final_dat <- lapply(svm_scales, \(scale) {
    if (!"edu_no_degree_2021" %in% names(scale)) return(scale)

    # Iterate over all years
    for (year in cc.data::census_years) {
      for (new_var_name in names(vars)) {
        if (new_var_name == "edu_no_degree_to_bachelor_above") {
          # In percentage
          scale[[paste(new_var_name, "pct", year, sep = "_")]] <- 1

          # In count value
          scale[[paste(new_var_name, "count", year, sep = "_")]] <-
            scale[[paste("population_15plus", year, sep = "_")]]
          next
        }


        education_var_codes <- vars[[new_var_name]]
        pct_val <- rowSums(sf::st_drop_geometry(scale)[paste(education_var_codes, year, sep = "_")])

        # in percentage
        scale[[paste(new_var_name, "pct", year, sep = "_")]] <- pmin(1, pct_val)
        # in count value
        scale[[paste(new_var_name, "count", year, sep = "_")]] <-
          pct_val * scale[[paste("population_15plus", year, sep = "_")]]
      }
    }

    return(scale)

  })


  # Data tibble -------------------------------------------------------------

  unique_var <-  names(vars)
  unique_var <- c(paste0(unique_var, "_pct"), paste0(unique_var, "_count"))

  data_construct(scales_data = final_dat,
                 unique_var = unique_var,
                 time_regex = time_regex,
                 inst_prefix = inst_prefix,
                 large_tables_db = large_tables_db)


  # Variables table ---------------------------------------------------------

  edu_dat <- gsub("edu_", "", edu_dat)
  vartitles <- cc.data::census_vectors_education[1:5, c("var_code", "var_title", "explanation", "exp_q5")]
  vartitles$var_code <- gsub("edu_", "", vartitles$var_code)
  vartitles$var_title <- gsub(" \\(\\%\\)$", "", vartitles$var_title)
  vartitles$var_short <- c("No degree", "Secondary", "College", "Uni. below bachelor",
                           "Bachelor+")
  vartitles$explanation <- gsub("the percentage of the population aged 15 and over ",
                                "", vartitles$explanation)

  variables <-
    lapply(unique_var, \(u_var) {

      pct <- grepl("_pct$", u_var)
      var <- gsub("_pct|_count", "", u_var)

      v <- gsub("edu_", "", u_var)
      vals <- stringr::str_extract_all(v, paste0(edu_dat, collapse = "|"))[[1]]

      title <- (\(x) {
        out <- if (length(vals) == 1) {
          (\(y) {
            vartitles$var_title[vartitles$var_code == vals]
          })()
        } else {
          start <- vartitles$var_title[vartitles$var_code == vals[[1]]]
          end <- vartitles$var_title[vartitles$var_code == vals[[2]]]
          if (vals[[2]] == "bachelor_above") return(sprintf("%s or any higher level of education", start))

          sprintf("%s, up to to %s", start, end)
        }

        if (pct) out <- paste(out, "(%)")
        out})()

      short <- (\(x) {
        if (length(vals) == 1) return(vartitles$var_short[vartitles$var_code == vals])
        start <- vartitles$var_short[vartitles$var_code == vals[[1]]]
        end <- vartitles$var_short[vartitles$var_code == vals[[2]]]

        if (vals[[2]] == "bachelor_above") return(sprintf("%s+", start))

        sprintf("%s - %s", start, end)
      })()

      explanation <- (\(x) {
        beg <- if (pct) "percentage" else "number"
        beg <- sprintf("the %s of individuals aged 15 and over", beg)
        if (length(vals) == 1) {
          return(sprintf("%s %s", beg, vartitles$explanation[vartitles$var_code == vals]))
        }
        start <- vartitles$explanation[vartitles$var_code == vals[[1]]]
        end <- vartitles$explanation[vartitles$var_code == vals[[2]]]
        if (vals[[2]] == "bachelor_above") return(sprintf("%s %s or any higher level of education", beg, start))

        sprintf("%s %s, up to a %s", beg, start, gsub("holding ", "", end))
      })()

      exp_q5 <- (\(x) {
        if (length(vals) == 1) {
          return(vartitles$exp_q5[vartitles$var_code == vals])
        }
        start <- vartitles$exp_q5[vartitles$var_code == vals[[1]]]
        end <- vartitles$exp_q5[vartitles$var_code == vals[[2]]]
        if (vals[[2]] == "bachelor_above") return(sprintf("%s or any higher level of education", start))

        sprintf("%s, up to %s", start, gsub("hold ", "", end))
      })()

      out <- add_variable(
        variables = scales_variables_modules$variables,
        var_code = u_var,
        type = if (pct) "pct" else "count",
        var_title = title,
        var_short = short,
        explanation = explanation,
        exp_q5 = exp_q5,
        parent_vec = "population_15plus",
        classification = "sociodemo",
        theme = "Education",
        private = FALSE,
        pe_include = FALSE,
        dates = cc.data::census_years,
        avail_scale = scales_to_interpolate,
        source = "Canadian census",
        interpolated = scales_variables_modules$variables$interpolated[
          scales_variables_modules$variables$var_code == "edu_no_degree"][[1]],
        allow_title_duplicate = TRUE,
        schema = list(time = time_regex)
      )
      out[out$var_code == u_var, ]
    })

  variables <- Reduce(rbind, variables, init = scales_variables_modules$variables)


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = scales_to_interpolate)


  # Modules table -----------------------------------------------------------

  varlefts <- grepl("^edu_", variables$var_code) & grepl("_pct$|_count$", variables$var_code)

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "education",
      theme = "Demographics",
      nav_title = "Education levels",
      title_text_title = "Education levels",
      title_text_main = paste0(
        "<p>Understanding how education levels are distributed helps us better ",
        "plan for effective policies, ensure equitable resource distribution, ",
        "and tailor community programs to meet real needs."
      ),
      title_text_extra = paste0(
        "<p>The education data showcased here provide insights into trends over ",
        "time. By examining data from the Canadian Census spanning 1996 to the ",
        "present, we can see how educational attainment has shifted and evolved ",
        "across different regions and communities."
      ),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>This module presents <a href = 'https://www.statcan.gc.ca/en/census/cen",
        "sus-engagement/about'>education distribution data from the 1996 to the latest C",
        "anadian Censuses</a></p>"
      ),
      var_left = variables$var_code[varlefts],
      dates = cc.data::census_years,
      main_dropdown_title = NA,
      add_advanced_controls = "Data representation",
      default_var = "edu_secondary_to_nonuni_pct",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))
}
