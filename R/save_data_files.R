#' Save all buildings-street-like dataset in a SQLite
#'
#' The function is used for any scales that ressembles to buildings or street, in
#' that they are very large in the amount of observations and most of the time,
#' the data attached to it on Curbcut will be the one of the dissemination areas.
#'
#' @param scale_chr <`character`> The name of the scale, e.g. "building".
#' @param path <`character`> Path where to save the `.sqlite`. Defaults to
#' `data/'scale_chr'.sqlite`
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#' @param keep_cols <`character vector`> Column names to keep in the sqlite
#' database.
#'
#' @return Returns an error or nothing if ran successfully. All existing `scale_chr`
#' data.frame in the fed `all_scales` are saved in the created `.sqlite`.
#' @export
save_bslike_sqlite <- function(scale_chr, path = sprintf("data/%s.sqlite", scale_chr),
                               all_scales, keep_cols = c("ID", "name", "name_2", "DA_ID")) {
  # Save all scales in the same database
  if (file.exists(path)) unlink(path)
  scale_sql <- DBI::dbConnect(RSQLite::SQLite(), path)

  # Save the scale
  df <- all_scales[[scale_chr]]
  df <- df[keep_cols]
  df <- sf::st_drop_geometry(df)

  if (scale_chr %in% DBI::dbListTables(scale_sql)) {
    DBI::dbRemoveTable(scale_sql, scale_chr)
  }

  DBI::dbWriteTable(scale_sql, "pre_pk_scale", df, overwrite = TRUE)

  create_table <- paste0("CREATE TABLE ", scale_chr)
  cols <- paste0(names(df), " VARCHAR,")
  cols <- paste0(cols, collapse = " ")
  end_cols <- paste0("CONSTRAINT ", scale_chr, "_pk PRIMARY KEY (ID)")
  collapsed <- paste0(create_table, " (", cols, " ", end_cols, ")")


  DBI::dbExecute(scale_sql, collapsed)
  DBI::dbExecute(
    scale_sql,
    paste0(
      "INSERT INTO ", scale_chr,
      " SELECT * FROM pre_pk_scale"
    )
  )
  DBI::dbExecute(scale_sql, "DROP TABLE pre_pk_scale")

  # Disconnect from the database
  DBI::dbDisconnect(scale_sql)

  # Return nothing
  return(invisible(NULL))
}

#' Save all buildings dataset in a SQLite
#'
#' This function uses \code{\link{save_bslike_sqlite}}.
#'
#' @param scale_chr <`character`> The name of the scale: "building
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#'
#' @return Returns an error or nothing if ran successfully. All existing `building`
#' data.frame in the fed `all_scales` are saved in the created `.sqlite`.
#' @export
save_buildings_sqlite <- function(scale_chr = "building", all_scales) {
  save_bslike_sqlite(scale_chr = scale_chr, all_scales = all_scales)
}

#' Save all streets dataset in a SQLite
#'
#' This function uses \code{\link{save_bslike_sqlite}}.
#'
#' @param scale_chr <`character`> The name of the scale: street
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#'
#' @return Returns an error or nothing if ran successfully. All existing `streets`
#' data.frame in the fed `all_scales` are saved in the created `.sqlite`.
#' @export
save_streets_sqlite <- function(scale_chr = "street", all_scales) {
  save_bslike_sqlite(scale_chr = scale_chr, all_scales = all_scales)
}

#' Save all scale tables in QS format
#'
#' This function saves all scale tables in QS format in the specified data folder.
#' It first drops the geometry of other scales and creates a list of tables for all
#' scales, then creates the folders for each region and scale, and finally saves the
#' tables in the database.
#'
#' @param data_folder <`character`> Where the `.qs` files should be
#' written to. Defaults to `data/`.
#' @param svm_data <`named list`> A named list of data.frame
#' containing all scales with a named list of their data. Usually
#' `scales_variables_modules$data`.
#'
#' @return An invisible NULL value.
#' @export
save_all_scales_qs <- function(data_folder = "data/", svm) {
  svm_data <- svm$data

  mapply(\(scale_name, data_list) {
    # Construct the folder path for the scale
    folder <- sprintf("%s%s/", data_folder, scale_name)

    # If the folder doesn't exist, create it
    if (!dir.exists(folder)) dir.create(folder)

    mapply(\(data_name, data) {
      if (is.null(data)) return()

      # Construct the file path for the table
      file <- sprintf("%s%s.qs", folder, data_name)

      # Save the table
      qs::qsave(data, file = file)

    }, names(data_list), data_list)

    # Population and households parent vectors are necessary even for scales
    # without census data
    if (!"c_population" %in% names(data_list)) {
      data_name <- "c_population"
      dat <- svm$scales[[scale_name]]
      if (!"population" %in% names(dat)) return()
      dat <- dat[c("ID", "population")]
      census_year <- cc.data::census_years
      census_year <- census_year[length(census_year)]
      names(dat)[2] <- sprintf("%s_%s", data_name, census_year)
      dat <- sf::st_drop_geometry(dat)

      # Add the schema regexes
      attr(dat, "schema") <- list(time = "_\\d{4}$")

      # Construct the file path for the table
      file <- sprintf("%s%s.qs", folder, data_name)

      # Save the table
      qs::qsave(dat, file = file)
    }
    if (!"private_households" %in% names(data_list)) {
      data_name <- "private_households"
      dat <- svm$scales[[scale_name]]
      if (!"households" %in% names(dat)) return()
      dat <- dat[c("ID", "households")]
      census_year <- cc.data::census_years
      census_year <- census_year[length(census_year)]
      names(dat)[2] <- sprintf("%s_%s", data_name, census_year)
      dat <- sf::st_drop_geometry(dat)

      # Add the schema regexes
      attr(dat, "schema") <- list(time = "_\\d{4}$")

      # Construct the file path for the table
      file <- sprintf("%s%s.qs", folder, data_name)

      # Save the table
      qs::qsave(dat, file = file)
    }

    # Keep a 'dictionary' of all available files
    all_files <- list.files(folder)
    all_files <- gsub(".qs$", "", all_files)
    qs::qsave(all_files, sprintf("%s%s_files.qs", data_folder, scale_name))

  }, names(svm_data), svm_data)

  return(invisible(NULL))
}

#' Save short tables as .qs
#'
#' @param data_folder <`character`> Where the `.qsm` files should be
#' written to. Defaults to `data/`.
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales, normally
#' `scales_variables_modules$scales`.
#' @param skip_scales <`character vector`> Scales to skip (not to keep as
#' a short table). These scales should be saved as a sqlite database instead, if
#' they are too large to be kept on memory. Defaults to an empty vector, no
#' scales are skipped.
#'
#' @return Returns an error or nothing if ran successfully. Every `scale` is
#' its own `.qs` containing a  trimed down  version of its data. Only
#' the columns from `ID` to `households` are kept, with centroid. NO data columns.
#' As light as possible so they live in the global environment.
#' @export
save_short_tables_qs <- function(data_folder = "data/", all_scales,
                                 skip_scales = c("building", "street")) {

  # Remove the scales to skip
  scales <- all_scales[!names(all_scales) %in% skip_scales]

  # For each scale, drop the geometry and save the table
  mapply(\(scale_name, scale_df) {

    d <- sf::st_drop_geometry(scale_df)
    subs <- grepl("ID$|^name$|^name_2$|^population$|^households$|^centroid$|^area$", names(d))
    d <- d[, subs]

    qs::qsave(d, file = paste0(data_folder, scale_name, ".qs"))
  }, names(scales), scales)

  return(invisible(NULL))
}

#' Save, for each scale, a table of ID and geometry used for export
#'
#' @param data_folder <`character`> Where the `.qsm` files should be
#' written to. Defaults to `data/`.
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#'
#' @return Returns an error or nothing if ran succesfully. Every scale is saved
#' in their most minimal version. Only used for when a user wants to do a
#' geometry export.
#' @export
save_geometry_export <- function(data_folder = "data/", all_scales) {
  if (!file.exists(paste0(data_folder, "geometry_export/"))) {
    dir.create(paste0(data_folder, "geometry_export/"))
  }

  # For each scale, drop the geometry and save the table
  mapply(\(scale_name, scale_df) {

    file_link <- paste0(data_folder, "geometry_export/", scale_name, ".qs")

    subs <- grepl("ID$", names(scale_df))
    d <- scale_df[, subs]

    qs::qsave(d, file = file_link)
  }, names(all_scales), all_scales)

  return(invisible(NULL))
}
