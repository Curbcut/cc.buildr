#' Build and append social mixity from census data
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
ba_socialmixity <- function(scales_variables_modules, scales_sequences,
                            scales_to_interpolate, overwrite = FALSE,
                            inst_prefix, large_tables_db) {
  # Get list of data variables ----------------------------------------------

  time_regex <-  "_\\d{4}$"
  data <- cc.data::db_read_data("socialmixity",
                                column_to_select = "DA_ID",
                                IDs = region_DA_IDs, crs = crs
  )

  names(data)[names(data) != "DA_ID"] <-
    paste0("socialmixity_", names(data)[names(data) != "DA_ID"])
  cols <- names(data)[names(data) != "DA_ID"]


  # Interpolate data to all possible scales ---------------------------------

  data_interpolated <-
    interpolate_from_census_geo(
      data = data,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "population",
      crs = crs,
      average_vars = cols,
      overwrite = overwrite,
      time_regex = time_regex,
      inst_prefix = inst_prefix
    )


  # Declare the types of the variables in a named list ----------------------

  types <- sapply(cols, \(x) list("ind"))


  # Data tibble -------------------------------------------------------------

  unique_var <- gsub(time_regex, "", cols)
  data_construct(scales_data = data_interpolated$scales,
                 unique_var = unique_var,
                 time_regex = time_regex,
                 inst_prefix = inst_prefix)


  # Variables table ---------------------------------------------------------

  ## Dates at which the data is available
  dates <- curbcut::s_extract(time_regex, var)
  dates <- gsub("^_", "", dates)
  unique_var <- unique_var[unique_var != "composite"]

  variables <-
    scales_variables_modules$variables |>
    add_variable(
      var_code = "socialmixity_composite",
      type = "ind",
      var_title = "Social mixity composite index",
      var_short = "Social mixity",
      explanation = "the composite index of social mixity",
      exp_q5 = "are living in areas with _X_ social mixity",
      parent_vec = "population",
      theme = "Social mixity",
      classification = "sociodemo",
      private = FALSE,
      pe_include = TRUE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      source = "Curbcut",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex),
      rank_name = c("Very low", "Low", "Moderate", "High", "Very high"),
      rank_name_short = c("V. low", "Low", "Mod.", "High", "V. high")
    ) |>
    add_variable(
      var_code = "socialmixity_sociopro",
      type = "ind",
      var_title = "Socioprofessional diversity index",
      var_short = "Sociopro",
      explanation = "the diversity of socioprofessional occupations",
      exp_q5 = "are living in areas with _X_ diversity of socioprofessional occupations",
      parent_vec = "population",
      theme = "Social mixity",
      classification = "sociodemo",
      private = FALSE,
      pe_include = TRUE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      source = "Curbcut",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex),
      rank_name = c("Very low", "Low", "Moderate", "High", "Very high"),
      rank_name_short = c("V. low", "Low", "Mod.", "High", "V. high")
    ) |>
    add_variable(
      var_code = "socialmixity_ethn_cult",
      type = "ind",
      var_title = "Ethno-cultural diversity index",
      var_short = "Ethno-cultural",
      explanation = "the ethno-cultural diversity of the population",
      exp_q5 = "are living in areas with _X_ ethno-cultural diversity",
      parent_vec = "population",
      theme = "Social mixity",
      classification = "sociodemo",
      private = FALSE,
      pe_include = TRUE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      source = "Curbcut",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex),
      rank_name = c("Very low", "Low", "Moderate", "High", "Very high"),
      rank_name_short = c("V. low", "Low", "Mod.", "High", "V. high")
    ) |>
    add_variable(
      var_code = "socialmixity_age",
      type = "ind",
      var_title = "Age diversity index",
      var_short = "Age",
      explanation = "the diversity in age groups of the population",
      exp_q5 = "are living in areas with _X_ age groups diversity",
      parent_vec = "population",
      theme = "Social mixity",
      classification = "sociodemo",
      private = FALSE,
      pe_include = TRUE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      source = "Curbcut",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex),
      rank_name = c("Very low", "Low", "Moderate", "High", "Very high"),
      rank_name_short = c("V. low", "Low", "Mod.", "High", "V. high")
    ) |>
    add_variable(
      var_code = "socialmixity_income",
      type = "ind",
      var_title = "Income diversity index",
      var_short = "Income",
      explanation = "the diversity in income groups of the population",
      exp_q5 = "are living in areas with _X_ income groups diversity",
      parent_vec = "population",
      theme = "Social mixity",
      classification = "sociodemo",
      private = FALSE,
      pe_include = TRUE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      source = "Curbcut",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex),
      rank_name = c("Very low", "Low", "Moderate", "High", "Very high"),
      rank_name_short = c("V. low", "Low", "Mod.", "High", "V. high")
    ) |>
    add_variable(
      var_code = "socialmixity_education",
      type = "ind",
      var_title = "Educational attainment diversity index",
      var_short = "Education",
      explanation = "the diversity of educational attainment in the population",
      exp_q5 = "are living in areas with _X_ educational attainment diversity",
      parent_vec = "population",
      theme = "Social mixity",
      classification = "sociodemo",
      private = FALSE,
      pe_include = TRUE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      source = "Curbcut",
      interpolated = data_interpolated$interpolated_ref,
      schema = list(time = time_regex),
      rank_name = c("Very low", "Low", "Moderate", "High", "Very high"),
      rank_name_short = c("V. low", "Low", "Mod.", "High", "V. high")
    )


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "socialmixity",
      theme = "Demographics",
      nav_title = "Social mixity",
      title_text_title = "Social mixity index",
      title_text_main = paste0(
        "The social mixity composite index provides an overall measure of socia",
        "l diversity across dissemination areas based on six key dimensio",
        "ns: socioprofessional diversity, ethno-cultural diversity, age diversi",
        "ty, income diversity, and educational attainment diversity. This index",
        " combines these factors to help identify areas that are socioeconomica",
        "lly diverse, compared to others, in an accessible and holistic manner."
      ),
      title_text_extra = paste0(
        "In addition to the composite index, the page provides ",
        "individual indices for each of the five components that constitute soci",
        "al mixity: socioprofessional status, ethno-cultural background, age di",
        "stribution, income levels, and education levels. Together, these indic",
        "es offer insights into which neighborhoods are more or less socioecono",
        "mically mixed."
      ),
      metadata = TRUE,
      dataset_info = paste0(
        "    <p>The data used on this page are derived from the 2021 Canadian C",
        "ensus, aggregated at the Dissemination Area (DA) level. The Composite ",
        "Social Mixity Index and the individual indices provide a comparative v",
        "iew of social diversity across Canada. This data includes multiple dim",
        "ensions, such as educational attainment, income brackets, socioprofess",
        "ional categories, age groups, and ethno-cultural background. Each inde",
        "x is normalized and aggregated to allow users to make meaningful compa",
        "risons between areas.</p>

    <ul>
        <li>
            <strong>T",
    "ravel Time Matrices:</strong> The indices are built using Curbcut’s tr",
    "avel time matrices which define accessibility within a 15-minute walk ",
    "between DAs. This means that each DA is analyzed together with the nei",
    "ghboring DAs that can be reached within 15 minutes on foot. This appro",
    "ach ensures that social diversity is considered not only at the indivi",
    "dual DA level but also in the context of its surrounding areas, promot",
    "ing a deeper understanding of neighborhood-level social mixity.
      ",
    "  </li>
        <li>
            <strong>Shannon Entropy Method:</stro",
    "ng> To evaluate social mixity within each DA, we use the Shannon entro",
    "py method. Shannon entropy takes into account both the richness of cat",
    "egories (the number of different groups) and their uniformity (how ind",
    "ividuals are distributed across these groups). For each of the dimensi",
    "ons mentioned above (socioprofessional, age, income, education, and et",
    "hno-cultural diversity), this method assigns a higher value when the p",
    "roportions of categories, such as education levels, are more evenly di",
    "stributed. This means that a DA with a more balanced distribution of i",
    "ndividuals across different categories will have a higher entropy valu",
    "e, indicating greater social mixity.
        </li>
        <li>
      ",
    "      <strong>Normalization:</strong> In order to ensure that no singl",
    "e dimension dominates the composite measure, we normalize the values f",
    "or each dimension. This guarantees that each dimension contributes equ",
    "ally to the overall measure of social mixity. After normalization, we ",
    "sum the normalized entropy scores to produce a composite index of soci",
    "al mixity for each area. However, even though each dimension is normal",
    "ized, a particular dimension may still have a significant impact on th",
    "e composite index if it exhibits exceptionally high diversity.
       ",
    " </li>
        <li>
            <strong>Composite Social Mixity Index:",
    "</strong> The Composite Social Mixity Index is derived by summing the ",
    "normalized entropy scores of all five components, providing a holistic",
    " view of the overall social mixity.
        </li>
    </ul>"
      ),
    var_left = grep("^socialmixity_", variables$var_code, value = TRUE),
    dates = "2021",
    main_dropdown_title = "Social mixity index",
    default_var = "socialmixity_composite",
    avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = modules
  ))
}
