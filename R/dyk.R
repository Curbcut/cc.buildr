### DYK FUNCTIONS ##############################################################

#' Prepare a Variable Table for DYK Generation
#'
#' This function creates a table of variable combinations which serves as the
#' input to the "Did you know" generation process.
#'
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#' @param all_tables <`list`> A list, usually `all_tables`, containing the
#' scales present in each region.
#' @param n <`integer`> Optionally, an integer scalar specifying the number of
#' rows in `svm$modules` to be used for creating the DYK variable table. The
#' default (`NULL`) will use all rows; other values may be useful for testing.
#'
#' @return A data frame with six columns (`module`, `region`, `scale`, `date`,
#' `var_left` and `var_right`), each of which is a character vector.
#' @export
dyk_prep <- function(svm, all_tables, n = NULL) {

  modules <- if (missing(n) || is.null(n)) svm$modules else svm$modules[1:n,]

  vars_dyk <-
    modules |>
    dplyr::select(module = id, region = regions, var_left, var_right,
                  date = dates) |>
    tidyr::unnest(region) |>
    dplyr::left_join(dplyr::tibble(region = names(all_tables),
                                   scale = all_tables), by = "region") |>
    dplyr::relocate(scale, .after = region) |>
    tidyr::unnest(scale) |>
    dplyr::mutate(date = lapply(date, as.character)) |>
    tidyr::unnest(date) |>
    # For now, filter away modules with huge numbers of variables; eventually we
    # should designate one variable per group to get a DYK for the whole group,
    # which would require a "group_default" logical field. TKTK
    dplyr::filter(!sapply(var_left, tibble::is_tibble)) |>
    tidyr::unnest(var_left) |>
    tidyr::unnest(var_right)

  var_left_check <-
    svm$variables |>
    dplyr::filter(var_code %in% vars_dyk$var_left) |>
    dplyr::select(var_left = var_code, df = avail_df, date = dates) |>
    tidyr::unnest(date) |>
    tidyr::unnest(df) |>
    tidyr::separate_wider_delim(df, delim = "_", names = c("region", "scale"))

  var_right_check <-
    svm$variables |>
    dplyr::filter(var_code %in% vars_dyk$var_right) |>
    dplyr::select(var_right = var_code, df = avail_df, date = dates) |>
    tidyr::unnest(date) |>
    tidyr::unnest(df) |>
    tidyr::separate_wider_delim(df, delim = "_", names = c("region", "scale"))

  vars_dyk <-
    vars_dyk |>
    dplyr::semi_join(var_left_check,
                     by = c("var_left", "region", "scale", "date")) |>
    dplyr::semi_join(var_right_check,
                     by = c("var_right", "region", "scale", "date")) |>
    # Manually remove grid region. TKTK maybe eventually this should be
    # data-driven instead of manually specified?
    dplyr::filter(region != "grid")

  vars_dyk <-
    vars_dyk |>
    dplyr::mutate(var_right = " ") |>
    dplyr::distinct() |>
    dplyr::bind_rows(vars_dyk) |>
    dplyr::arrange(module, region, scale, var_left, var_right, date)

  return(vars_dyk)

}

# Univariate --------------------------------------------------------------

#' Generate Highest/Lowest Value DYKs
#'
#' This function creates "Did you know" text strings highlighting the highest
#' and lowest values from a combination of variable, region, scale and date.
#' The output is a data frame containing a `highest` and `lowest` column.
#'
#' @param var_left <character> A string representing the name of the variable
#' for which the DYK should be calculated.
#' @param region <`character`> A string representing the name of the region
#' for which the DYK should be calculated.
#' @param scale <`character`> A string representing the name of the scale
#' for which the DYK should be calculated.
#' @param date <`character`> A string representing the name of the date
#' for which the DYK should be calculated.
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#'
#' @return A data frame with two columns (`highest` and `lowest`), each of
#' which contains a character vector of DYK outputs.
#' @export
dyk_uni_highest_lowest <- function(var_left, region, scale, date, svm) {

  # Get class
  vars <- mapply(curbcut::vars_build,
                 var_left = paste(var_left, date, sep = "_"),
                 df = paste(region, scale, sep = "_"),
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # Initial region mention
  region_start <- mapply(\(x, y) curbcut:::explore_context(
    region = x, select_id = NA, df = paste(x, y, sep = "_"), switch_DA = FALSE),
    x = region, y = scale, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  region_start <- sapply(region_start, \(x) curbcut::s_sentence(x$p_start))

  # Scale name
  scale_name <- scales_dictionary$sing[sapply(
    scale, \(x) which(scales_dictionary$scale == x), USE.NAMES = FALSE)]

  # Highest value
  highest_val <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    max(tb[[paste(var_left, date, sep = "_")]], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Highest name
  highest_name <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    tb$name[which.max(tb[[paste(var_left, date, sep = "_")]])]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second highest value
  second_highest_val <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    max_val <- which.max(tb[[paste(var_left, date, sep = "_")]])
    max(tb[[paste(var_left, date, sep = "_")]][-max_val], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second highest name
  second_highest_name <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    val_vec <- tb[[paste(var_left, date, sep = "_")]]
    # Remove top value
    val_vec[which.max(val_vec)] <- -Inf
    tb$name[which.max(val_vec)]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Lowest value
  lowest_val <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    min(tb[[paste(var_left, date, sep = "_")]], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Lowest name
  lowest_name <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    tb$name[which.min(tb[[paste(var_left, date, sep = "_")]])]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second lowest value
  second_lowest_val <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    min_val <- which.min(tb[[paste(var_left, date, sep = "_")]])
    min(tb[[paste(var_left, date, sep = "_")]][-min_val], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second lowest name
  second_lowest_name <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    val_vec <- tb[[paste(var_left, date, sep = "_")]]
    # Remove bottom value
    val_vec[which.min(val_vec)] <- Inf
    tb$name[which.min(val_vec)]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Variable explanation
  var_exp <- svm$variables$explanation_nodet[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]

  # Convert values
  highest_val <- mapply(curbcut::convert_unit,
                        var = lapply(vars, \(x) x$var_left),
                        x = highest_val,
                        MoreArgs = list(decimal = 1, compact = FALSE),
                        SIMPLIFY = TRUE, USE.NAMES = FALSE)

  second_highest_val <- mapply(curbcut::convert_unit,
                               var = lapply(vars, \(x) x$var_left),
                               x = second_highest_val,
                               MoreArgs = list(decimal = 1, compact = FALSE),
                               SIMPLIFY = TRUE, USE.NAMES = FALSE)

  lowest_val <- mapply(curbcut::convert_unit,
                       var = lapply(vars, \(x) x$var_left),
                       x = lowest_val,
                       MoreArgs = list(decimal = 1, compact = FALSE),
                       SIMPLIFY = TRUE, USE.NAMES = FALSE)

  second_lowest_val <- mapply(curbcut::convert_unit,
                              var = lapply(vars, \(x) x$var_left),
                              x = second_lowest_val,
                              MoreArgs = list(decimal = 1, compact = FALSE),
                              SIMPLIFY = TRUE, USE.NAMES = FALSE)

  # Assemble output
  highest <- paste0(
    region_start, ", ", highest_name, " is the ", scale_name,
    " with the highest ", var_exp, " (", highest_val, "), followed by ",
    second_highest_name, " (", second_highest_val, ").")

  lowest <- paste0(
    region_start, ", ", lowest_name, " is the ", scale_name,
    " with the lowest ", var_exp, " (", lowest_val, "), followed by ",
    second_lowest_name, " (", second_lowest_val, ").")

  tibble::tibble(highest = highest, lowest = lowest)

}


#' Generate Change-over-Time DYKs
#'
#' This function creates "Did you know" text strings highlighting the change
#' over time from a combination of variable, region and scale. The output is a
#' data frame containing a `change_text` and `change_val` column.
#'
#' @param var_left <character> A string representing the name of the variable
#' for which the DYK should be calculated.
#' @param region <`character`> A string representing the name of the region
#' for which the DYK should be calculated.
#' @param scale <`character`> A string representing the name of the scale
#' for which the DYK should be calculated.
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#'
#' @return A data frame with two columns (`change_text` and `change_val`),
#' which contain a character vector of DYK outputs and a numeric vector of the
#' values respectively.
#' @export
dyk_uni_change <- function(var_left, region, scale, svm) {

  # Get class
  vars <- mapply(curbcut::vars_build,
                 var_left = var_left,
                 df = paste(region, scale, sep = "_"),
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # Get region values
  values <-
    map2(var_left, region, \(x, y) {
      svm$variables |>
        filter(var_code == x) |>
        pull(region_values) |>
        pluck(1) |>
        filter(region == y)})

  first_val <- sapply(values, \(x) x$val[length(x$val)])
  last_val <- sapply(values, \(x) x$val[1])
  change <- mapply(\(x, y) (y - x) / x, x = first_val, y = last_val)
  first_date <- sapply(values, \(x) x$year[length(x$year)])
  last_date <- sapply(values, \(x) x$year[1])

  # Initial region mention
  region_start <- mapply(\(x, y) curbcut:::explore_context(
    region = x, select_id = NA, df = paste(x, y, sep = "_"), switch_DA = FALSE),
    x = region, y = scale, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  region_start <- sapply(region_start, \(x) curbcut::s_sentence(x$p_start))

  # Variable explanation
  var_exp <- svm$variables$explanation[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]

  # Increasing/decreasing
  inc_dec <- dplyr::case_when(
    change >= 0.2 ~ "increasing rapidly",
    change >= 0 ~ "increasing",
    change < -0.2 ~ "decreasing rapidly",
    change < 0 ~ "decreasing")

  # Convert values
  first_val <- mapply(curbcut::convert_unit,
                      var = lapply(vars, \(x) x$var_left),
                      x = first_val,
                      MoreArgs = list(decimal = 1, compact = FALSE),
                      SIMPLIFY = TRUE, USE.NAMES = FALSE)

  last_val <- mapply(curbcut::convert_unit,
                     var = lapply(vars, \(x) x$var_left),
                     x = last_val,
                     MoreArgs = list(decimal = 1, compact = FALSE),
                     SIMPLIFY = TRUE, USE.NAMES = FALSE)

  change_txt <- sapply(
    change, \(x) curbcut:::convert_unit.pct(x = x, decimal = 1))

  # Assemble output
  change_vec <- paste0(
    region_start, ", ", var_exp, " has been ", inc_dec, ", from ",
    first_val, " in ", first_date, " to ", last_val, " in ", last_date,
    ". This is a ", change_txt, " overall change.")

  tibble::tibble(change_text = change_vec, change_val = change)

}


#' Generate Bivariate Comparison DYKs
#'
#' This function creates "Did you know" text strings highlighting bivariate
#' comparisons from a combination of variables, region, scale and date.
#' The output is a data frame containing a `highest` and `lowest` column.
#'
#' @param var_left <character> A string representing the name of the first
#' variable for which the DYK should be calculated.
#' @param var_right <character> A string representing the name of the second
#' variable for which the DYK should be calculated.
#' @param region <`character`> A string representing the name of the region
#' for which the DYK should be calculated.
#' @param scale <`character`> A string representing the name of the scale
#' for which the DYK should be calculated.
#' @param date <`character`> A string representing the name of the date
#' for which the DYK should be calculated.
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#'
#' @return A data frame with two columns (`compare_text` and `compare_val`),
#' which contain a character vector of DYK outputs and a numeric vector of the
#' values respectively.
#' @export
dyk_uni_compare <- function(var_left, var_right, region, scale, date, svm) {

  # Get class
  vars <- mapply(curbcut::vars_build,
                 var_left = paste(var_left, date, sep = "_"),
                 var_right = paste(var_right, date, sep = "_"),
                 df = paste(region, scale, sep = "_"),
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # Initial region mention
  region_start <- mapply(\(x, y) curbcut:::explore_context(
    region = x, select_id = NA, df = paste(x, y, sep = "_"), switch_DA = FALSE),
    x = region, y = scale, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  region_start <- sapply(region_start, \(x) curbcut::s_sentence(x$p_start))

  # Scale name
  scale_name <- scales_dictionary$plur[sapply(
    scale, \(x) which(scales_dictionary$scale == x), USE.NAMES = FALSE)]

  # Values
  val_1 <- mapply(\(var_left, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    tb[[paste(var_left, date, sep = "_")]]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  val_2 <- mapply(\(var_right, region, scale, date) {
    tb <- svm$scales[[region]][[scale]]
    tb[[paste(var_right, date, sep = "_")]]
  }, var_right, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = FALSE)

  # Correlation
  corr <- mapply(cor, x = val_1, y = val_2,
                 MoreArgs = list(use = "na.or.complete"), USE.NAMES = FALSE)
  positive <- corr > 0

  # Frequency qualifier
  freq <- dplyr::case_when(
    abs(corr) > 0.7 ~ "almost always have",
    abs(corr) > 0.3 ~ "tend to have",
    abs(corr) > 0.1 ~ "often have",
    .default = "are equally likely to have and not to have"
  )

  # High/low
  high_low_1 <- sapply(vars, \(x) curbcut::explore_text_bivar_adjective(
    x$var_left, TRUE, TRUE, FALSE))
  high_low_2 <- mapply(\(x, y) {
    if (is.na(y)) return(NA_character_)
    curbcut::explore_text_bivar_adjective(
      x$var_right, FALSE, y, FALSE)}, vars, positive)

  # Variable explanations
  var_exp_1 <- svm$variables$explanation_nodet[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]
  var_exp_2 <- svm$variables$explanation_nodet[sapply(
    var_right, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]

  # Assemble output
  compare_vec <- paste0(
    region_start, ", ", scale_name, " with ", high_low_1, " ", var_exp_1, " ",
    freq, " ", high_low_2, " ", var_exp_2, ".")

  tibble::tibble(compare_text = compare_vec, compare_val = corr)

}
