#' Build and append household sizes from census data
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
ba_householdsize <- function(scales_variables_modules, scales_sequences,
                             scales_to_interpolate, overwrite = FALSE,
                             inst_prefix, large_tables_db) {
  # Declare all variables from the census -----------------------------------

  time_regex <- "_\\d{4}$"

  hou_dat <- cc.data::census_vectors_householdsize$var_code
  hou_dat <- hou_dat[grepl("household_size_", hou_dat)]

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
  vars <- generate_sequences(hou_dat)

  names(vars) <- sapply(vars, \(x) {
    if (length(x) == 1) return(x)
    start <- gsub("household_size_", "", x[[1]])
    end <- gsub("household_size_", "", x[[length(x)]])

    sprintf("household_size_agg_%s_%s", start, end)
  })

  vars <- vars[lengths(vars) > 1]


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
    householdsize_files <- all_files[grepl(sprintf("%s/household_size_\\d", scale_name), all_files)]

    if (length(householdsize_files) == 0) return(scale_df)

    # Add c_population
    householdsize_files <- c(householdsize_files, paste0("data/", scale_name, "/private_households.qs"))

    householdsize_data <- lapply(householdsize_files, qs::qread)
    householdsize_data <- Reduce(\(x, y) merge(x, y, by = "ID"), householdsize_data)

    merge(scale_df, householdsize_data, by = "ID")

  }, names(svm_scales), svm_scales)

  final_dat <- lapply(svm_scales, \(scale) {
    if (!"household_size_1_2021" %in% names(scale)) return(scale)

    # Iterate over all years
    for (year in cc.data::census_years) {
      for (new_var_name in names(vars)) {
        if (new_var_name == "householdsize_agg_1_4") {
          # In percentage
          scale[[paste(new_var_name, "pct", year, sep = "_")]] <- 1

          # In count value
          scale[[paste(new_var_name, "count", year, sep = "_")]] <-
            scale[[paste("private_households", year, sep = "_")]]
          next
        }


        householdsize_var_codes <- vars[[new_var_name]]
        pct_val <- rowSums(sf::st_drop_geometry(scale)[paste(householdsize_var_codes, year, sep = "_")])

        # in percentage
        scale[[paste(new_var_name, "pct", year, sep = "_")]] <- pct_val
        # in count value
        scale[[paste(new_var_name, "count", year, sep = "_")]] <-
          pct_val * scale[[paste("private_households", year, sep = "_")]]
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

  variables <-
    lapply(unique_var, \(u_var) {

      pct <- grepl("_pct$", u_var)
      var <- gsub("_pct|_count", "", u_var)

      title <- (\(x) {
        start <- stringr::str_extract(var, "(?<=household_size_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=household_size_agg_\\d{1,2}_).*")
        if (end == "4") end <- "4 and above"


        out <- sprintf("Household size between %s and %s", start, end)
        if (pct) out <- paste(out, "(%)")
        out
      })()

      short <- (\(x) {
        start <- stringr::str_extract(var, "(?<=household_size_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=household_size_agg_\\d{1,2}_).*")
        if (end == "4") end <- "4+"


        sprintf("%s-%s", start, end)
      })()

      explanation <- (\(x) {
        start <- stringr::str_extract(var, "(?<=household_size_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=household_size_agg_\\d{1,2}_).*")
        if (end == "4") end <- "4 or more"

        beg <- if (pct) "percentage of households" else "number of households"
        sprintf("the %s that are occupied by %s to %s people", beg, start, end)
      })()

      exp_q5 <- (\(x) {
        start <- stringr::str_extract(var, "(?<=household_size_agg_).*(?=_)")
        end <- stringr::str_extract(var, "(?<=household_size_agg_\\d{1,2}_).*")
        if (end == "4") end <- "4 or more"


        sprintf("are occupied by %s to %s people", start, end)
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
        parent_vec = "private_households",
        classification = "sociodemo",
        theme = "Household",
        private = FALSE,
        pe_include = FALSE,
        dates = cc.data::census_years,
        avail_scale = scales_to_interpolate,
        source = "Canadian census",
        interpolated = scales_variables_modules$variables$interpolated[
          scales_variables_modules$variables$var_code == "household_size_1"][[1]],
        allow_title_duplicate = TRUE,
        group_name = group_name,
        group_diff = group_diff,
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

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "householdsize",
      theme = "Demographics",
      nav_title = "Household size",
      title_text_title = "Household size distribution",
      title_text_main = paste0(
        "<p>Understanding the distribution of household sizes is crucial for ",
        "policy making and resource allocation. It helps in planning for housing, ",
        "community services, and infrastructure development."
      ),
      title_text_extra = paste0(
        "<p>The household size distribution data visualized on this page come from the Canadian C",
        "ensus from 1996 to the present. These datasets provide insights into ",
        "demographic trends and shifts over time."
      ),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>This module presents <a href = 'https://www.statcan.gc.ca/en/census/cen",
        "sus-engagement/about'>household size distribution data from the 1996 to the latest C",
        "anadian Censuses</a></p>"
      ),
      var_left = variables[grepl("^household_size_agg_", variables$var_code),
                           c("var_code", "group_name", "group_diff")],
      dates = cc.data::census_years,
      main_dropdown_title = NA,
      default_var = "household_size_2_3",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))
}
