#' Build and append age from census data
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#'
#' @return A list of length 4, similar to the one fed to
#' `scales_variables_modules` with age data added, their addition
#' in the variables table and the module table.
#' @export
ba_age <- function(scales_variables_modules, scales_sequences) {
  # Declare all variables from the census -----------------------------------

  time_regex <- "_\\d{4}$"

  age_dat <- cc.data::census_vectors_age$var_code
  age_dat <- age_dat[!age_dat %in% c("age_0_14", "age_15_64", "age_65_plus")]

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
  vars <- generate_sequences(age_dat)

  names(vars) <- sapply(vars, \(x) {
    if (length(x) == 1 && x == "age_0_4") return("age_agg_0_4")
    if (length(x) == 1 && x == "age_85") return("age_agg_85plus")
    start <- stringr::str_extract(x[[1]], "(?<=age_).*(?=_)")

    last <- x[[length(x)]]
    end <- if (last == "age_85") "85plus" else stringr::str_extract(last, "(?<=age_\\d{1,2}_).*")

    sprintf("age_agg_%s_%s", start, end)
  })


  # Build census data for all possible scales -------------------------------

  final_dat <- lapply(scales_variables_modules$scales, \(scale) {
    if (!"age_0_14_2021" %in% names(scale)) return(scale)

    # Iterate over all years
    for (year in cc.data::census_years) {
      for (new_var_name in names(vars)) {
        if (new_var_name == "age_agg_0_85plus") {
          scale[[paste(new_var_name, year, sep = "_")]] <- 1
          next
        }


        age_var_codes <- vars[[new_var_name]]
        scale[[paste(new_var_name, year, sep = "_")]] <-
          rowSums(sf::st_drop_geometry(scale)[paste(age_var_codes, year, sep = "_")])
      }
    }

    return(scale)

  })

  avail_scales <- sapply(scales_variables_modules$scales, \(scale) {
    if (!"age_0_14_2021" %in% names(scale)) F else T
  })
  avail_scales <- names(avail_scales)[avail_scales]


  # Data tibble -------------------------------------------------------------

  unique_var <-  names(vars)

  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = final_dat,
                         unique_var = unique_var,
                         time_regex = time_regex)


  # Variables table ---------------------------------------------------------

  variables <-
    lapply(unique_var, \(u_var) {

      title <- (\(x) {
        if (u_var == "age_agg_85plus") return("Aged over 85 (%)")
        start <- stringr::str_extract(u_var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(u_var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "over 85"


        sprintf("Aged between %s and %s (%%)", start, end)
      })()

      short <- (\(x) {
        if (u_var == "age_agg_85plus") return("85+ yo")
        start <- stringr::str_extract(u_var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(u_var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "85+"


        sprintf("%s-%s yo", start, end)
      })()

      explanation <- (\(x) {
        if (u_var == "age_agg_85plus")
          return("the percentage of the population aged over 85 years old")
        start <- stringr::str_extract(u_var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(u_var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "over 85"


        sprintf("the percentage of the population aged between %s and %s years old", start, end)
      })()

      exp_q5 <- (\(x) {
        if (u_var == "age_agg_85plus")
          return("are aged over 85 years old")
        start <- stringr::str_extract(u_var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(u_var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "over 85"


        sprintf("are aged between %s and %s years old", start, end)
      })()

      out <- add_variable(
        variables = scales_variables_modules$variables,
        var_code = u_var,
        type = "pct",
        var_title = title,
        var_short = short,
        explanation = explanation,
        exp_q5 = exp_q5,
        parent_vec = scales_variables_modules$variables$parent_vec[
          scales_variables_modules$variables$var_code == "age_0_14"],
        theme = "Age",
        private = FALSE,
        pe_include = FALSE,
        dates = cc.data::census_years,
        avail_scale = avail_scales,
        source = "Canadian census",
        interpolated = scales_variables_modules$variables$interpolated[
          scales_variables_modules$variables$var_code == "age_0_14"][[1]],
        allow_title_duplicate = TRUE
      )
      out[out$var_code == u_var, ]
    })

  variables <- Reduce(rbind, variables, init = scales_variables_modules$variables)


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = avail_scales)


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "age",
      theme = "Demographic",
      nav_title = "Age demographics",
      title_text_title = "Age distribution",
      title_text_main = paste0(
        "<p>Understanding the age distribution of a population is crucial for policy ",
        "making and resource allocation. It helps in planning for education, health",
        "care, and social services."
      ),
      title_text_extra = paste0(
        "<p>The age distribution data visualized here is sourced from the Canadian C",
        "ensus, ranging from 1996 to the present. These datasets provide insights in",
        "to demographic trends and shifts over time. Key initiatives to address age-",
        "related challenges include healthcare reforms and educational adjustments."
      ),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>This module presents <a href = 'https://www.statcan.gc.ca/en/census/cen",
        "sus-engagement/about'>age distribution data from the 1996 to the latest C",
        "anadian Censuses</a></p>"
      ),
      var_left = unique_var,
      dates = cc.data::census_years,
      main_dropdown_title = NA,
      var_right = variables$var_code[variables$source == "Canadian census" &
                                       variables$theme != "Age" &
                                       !is.na(variables$parent_vec)],
      default_var = "age_agg_0_14",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = final_dat,
    variables = variables,
    modules = modules,
    data = data
  ))
}
