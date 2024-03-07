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
#' @param scales_dictionary <`named list`> Dictionary of scales
#' @param data_folder <`character`> Where the `.qs` files should be
#' written to. Defaults to `data/`.
#'
#' @return An invisible NULL value.
#' @export
save_all_scales_qs <- function(scales_dictionary, data_folder = "data/") {

  lapply(scales_dictionary$scale, \(scale_name) {
    # Construct the folder path for the scale
    folder <- sprintf("%s%s/", data_folder, scale_name)

    # If the folder doesn't exist, create it
    if (!dir.exists(folder)) dir.create(folder)

    all_files <- list.files(folder)
    all_files <- gsub(".qs$", "", all_files)

    # Population and households parent vectors are necessary even for scales
    # without census data
    if (!"c_population" %in% all_files) {
      data_name <- "c_population"
      dat_file <- sprintf("%s%s.qs", data_folder, scale_name)
      if (file.exists(dat_file)) {
        dat <- qs::qread(dat_file)
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

    }
    if (!"private_households" %in% all_files) {
      data_name <- "private_households"
      dat_file <- sprintf("%s%s.qs", data_folder, scale_name)
      if (file.exists(dat_file)) {
        dat <- qs::qread(dat_file)
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
    }

    # Keep a 'dictionary' of all available files
    all_files <- list.files(folder)
    all_files <- gsub(".qs$", "", all_files)
    qs::qsave(all_files, sprintf("%s%s_files.qs", data_folder, scale_name))

  })

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
                                 skip_scales = c(
                                   "building", "street", "grd30", "grd60", "grd120",
                                   "grd300")) {

  # Remove the scales to skip
  scales <- all_scales[!names(all_scales) %in% skip_scales]

  # Create the scales_png folder
  suppressWarnings(dir.create(sprintf("%sscales_png", data_folder)))

  # For each scale, drop the geometry and save the table
  mapply(\(scale_name, scale_df) {

    d <- sf::st_drop_geometry(scale_df)
    subs <- grepl("ID$|^name$|^name_2$|^population$|^households$|^centroid$|^area$", names(d))
    d <- d[, subs]

    qs::qsave(d, file = paste0(data_folder, scale_name, ".qs"))

    # # Save plot PNG
    # plot <- ggplot2::ggplot(scale_df["geometry"]) +
    #   ggplot2::geom_sf(fill = "#98A8CB", color = "white") +
    #   ggplot2::theme_void()
    # ggplot2::ggsave(sprintf("%sscales_png/%s_plot.png", data_folder, scale_name),
    #                 plot = plot, width = 4.86, height = 4.86)
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

    subs <- grepl("ID$|name$|area$", names(scale_df))
    d <- scale_df[, subs]

    qs::qsave(d, file = file_link)
  }, names(all_scales), all_scales)

  return(invisible(NULL))
}

#' Unload specified scales from the scales_variables_modules
#'
#' This function removes specified scale variables from a list of scales.
#' It is useful for managing memory if a scale is not to be re-used.
#'
#' @param scales <`list`> A list containing scales and
#' their associated variables and modules.
#' @param unload <`character vector`> A character vector of scale names to
#' be unloaded from the list.
#'
#' @return <`list`> Returns the modified list of scales variables modules,
#' excluding the scales specified in `unload`.
#' @export
unload_scales <- function(scales, unload) {
  if (any(!unload %in% names(scales))) {
    stop("One or more of the specified scales to unload are not present in the list.")
  }

  scales[!names(scales) %in% unload]

}

#' Exclude scales with processed data already saved
#'
#' This function excludes scales for which data has already been processed and stored.
#' If 'overwrite' is TRUE, no exclusion is performed and all scales are returned.
#'
#' @param unique_vars <`character vector`> A vector of unique variable names.
#' @param scales <`character vector OR named list`> A vector of scale names or a named
#' list of scales to be checked.
#' @param overwrite <`logical`> If TRUE, no scales are excluded and all are returned.
#' @param data_folder <`character`> The folder where data files are stored.
#' Default is "data/".
#'
#' @return <`character vector`> Scales for which data files do not exist
#' or all scales if 'overwrite' is TRUE.
#' @export
exclude_processed_scales <- function(unique_vars, scales, overwrite = FALSE, data_folder = "data/") {
  if (overwrite) return(scales)

  # We want the function to work both for the named list of scales, or for a
  # character vector of scales.
  if (is.list(scales)) {
    scales_name <- names(scales)
  } else {
    scales_name <- scales
  }

  all_files <- list.files(data_folder, recursive = TRUE)

  # Iterate over scales_name to know which ones already have data stored
  keep_index <- sapply(scales_name, \(sc) {

    # If it's a sqlite
    sqlite_path <- sprintf("%s%s.sqlite", data_folder, sc)
    if (sqlite_path %in% all_files) {
      conn <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
      table <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_master")$name
      DBI::dbDisconnect(conn)
      return(!all(unique_vars %in% table))
    }

    data_files <- paste0(sc, "/", unique_vars, ".qs")

    # Keep the index if there are data that isn't already stored
    !all(data_files %in% all_files)
  })

  # Remove scales that already have all the data
  scales[keep_index]
}
