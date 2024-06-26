#' Build and append age from census data
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
#' `scales_variables_modules` with age data added, their addition
#' in the variables table and the module table.
#' @export
ba_age <- function(scales_variables_modules, scales_sequences, scales_to_interpolate,
                   overwrite = FALSE, inst_prefix) {
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
    age_files <- all_files[grepl(sprintf("%s/age_\\d", scale_name), all_files)]

    if (length(age_files) == 0) return(scale_df)

    # Add c_population
    age_files <- c(age_files, paste0("data/", scale_name, "/c_population.qs"))

    age_data <- lapply(age_files, qs::qread)
    age_data <- Reduce(\(x, y) merge(x, y, by = "ID"), age_data)

    merge(scale_df, age_data, by = "ID")

  }, names(svm_scales), svm_scales)

  final_dat <- lapply(svm_scales, \(scale) {
    if (!"age_0_14_2021" %in% names(scale)) return(scale)

    # Iterate over all years
    for (year in cc.data::census_years) {
      for (new_var_name in names(vars)) {
        if (new_var_name == "age_agg_0_85plus") {
          # In percentage
          scale[[paste(new_var_name, "pct", year, sep = "_")]] <- 1

          # In count value
          scale[[paste(new_var_name, "count", year, sep = "_")]] <-
            scale[[paste("c_population", year, sep = "_")]]
          next
        }


        age_var_codes <- vars[[new_var_name]]
        pct_val <- rowSums(sf::st_drop_geometry(scale)[paste(age_var_codes, year, sep = "_")])

        # in percentage
        scale[[paste(new_var_name, "pct", year, sep = "_")]] <- pct_val
        # in count value
        scale[[paste(new_var_name, "count", year, sep = "_")]] <-
          pct_val * scale[[paste("c_population", year, sep = "_")]]
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
                 inst_prefix = inst_prefix)


  # Variables table ---------------------------------------------------------

  variables <-
    lapply(unique_var, \(u_var) {

      pct <- grepl("_pct$", u_var)
      var <- gsub("_pct|_count", "", u_var)

      title <- (\(x) {
        if (grepl("age_agg_85plus", var)) {
          out <- "Aged over 85"
          if (pct) out <- paste(out, "(%)")
          return(out)
        }
        start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "over 85"


        out <- sprintf("Aged between %s and %s", start, end)
        if (pct) out <- paste(out, "(%)")
        out
      })()

      short <- (\(x) {
        if (grepl("age_agg_85plus", var)) {
          out <- "85+ yo"
          if (pct) out <- paste(out, "(%)")
          return(out)
        }
        start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "85+"


        sprintf("%s-%s yo", start, end)
      })()

      explanation <- (\(x) {
        if (grepl("age_agg_85plus", var)) {
          out <- "aged over 85 years old"

          beg <- if (pct) "percentage of the population" else "number of individuals"
          return(sprintf("the %s %s", beg, out))
        }
        start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "over 85"

        beg <- if (pct) "percentage of the population" else "number of individuals"
        sprintf("the %s aged between %s and %s years old", beg, start, end)
      })()

      exp_q5 <- (\(x) {
        if (grepl("age_agg_85plus", var))
          return("are aged over 85 years old")
        start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
        if (end == "85plus") end <- "over 85"


        sprintf("are aged between %s and %s years old", start, end)
      })()

      group_name <- title
      group_diff <- list(
        "Data representation" = (\(x) {
          if (!pct) "Number" else "Percentage"
        })())

      out <- add_variable(
        variables = scales_variables_modules$variables,
        var_code = u_var,
        type = if (pct) "pct" else "count",
        var_title = title,
        var_short = short,
        explanation = explanation,
        exp_q5 = exp_q5,
        parent_vec = "c_population",
        classification = "sociodemo",
        theme = "Age",
        private = FALSE,
        pe_include = FALSE,
        dates = cc.data::census_years,
        avail_scale = scales_to_interpolate,
        source = "Canadian census",
        interpolated = scales_variables_modules$variables$interpolated[
          scales_variables_modules$variables$var_code == "age_0_14"][[1]],
        allow_title_duplicate = TRUE,
        group_name = group_name,
        group_diff = group_diff
      )
      out[out$var_code == u_var, ]
    })

  variables <- Reduce(rbind, variables, init = scales_variables_modules$variables)


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = scales_to_interpolate)


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "age",
      theme = "Demographics",
      nav_title = "Age demographics",
      title_text_title = "Age distribution",
      title_text_main = paste0(
        "<p>Understanding the age distribution of a population is crucial for policy ",
        "making and resource allocation. It helps in planning for education, health",
        "care, and social services."
      ),
      title_text_extra = paste0(
        "<p>The age distribution data visualized on this page come from the Canadian C",
        "ensus from 1996 to the present. These datasets provide insights into ",
        "demographic trends and shifts over time. Key initiatives to address age-",
        "related challenges include healthcare reforms and educational reforms."
      ),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>This module presents <a href = 'https://www.statcan.gc.ca/en/census/cen",
        "sus-engagement/about'>age distribution data from the 1996 to the latest C",
        "anadian Censuses</a></p>"
      ),
      var_left = variables[grepl("^age_agg_", variables$var_code),
                           c("var_code", "group_name", "group_diff")],
      dates = cc.data::census_years,
      main_dropdown_title = NA,
      default_var = "age_agg_0_14_pct",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))
}
