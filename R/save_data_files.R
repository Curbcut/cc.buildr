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

  # Iterate and save every scales dataset
  map_over_scales(
    all_scales = all_scales,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
      scale_df = scale_df) {
      if (scale_name != scale_chr) {
        return()
      }
      geo_scale <- paste0(geo, "_", scale_chr)
      df <- sf::st_drop_geometry(scale_df)
      df <- df[keep_cols]

      if (geo_scale %in% DBI::dbListTables(scale_sql)) {
        DBI::dbRemoveTable(scale_sql, geo_scale)
      }

      DBI::dbWriteTable(scale_sql, "pre_pk_scale", df)

      create_table <- paste0("CREATE TABLE ", geo_scale)
      cols <- paste0(names(df), " VARCHAR,")
      cols <- paste0(cols, collapse = " ")
      end_cols <- paste0("CONSTRAINT ", scale_name, "_pk PRIMARY KEY (ID)")
      collapsed <- paste0(create_table, " (", cols, " ", end_cols, ")")


      DBI::dbExecute(scale_sql, collapsed)
      DBI::dbExecute(
        scale_sql,
        paste0(
          "INSERT INTO ", geo_scale,
          " SELECT * FROM pre_pk_scale"
        )
      )
      DBI::dbExecute(scale_sql, "DROP TABLE pre_pk_scale")
    }
  )

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
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#' @param variables <`data.frame`> The `variables` data.frame, normally
#' `scales_variables_modules$variables`.
#'
#' @return An invisible NULL value.
#' @export
save_all_scales_qs <- function(data_folder = "data/", all_scales, variables) {
  # Drop geometry of other scales
  all_scales_no_geo <-
    map_over_scales(
      all_scales = all_scales,
      fun = \(geo = geo, scales = scales, scale_name = scale_name,
        scale_df = scale_df) {
        sf::st_drop_geometry(scale_df)
      }
    )
  all_scales_no_geo <- lapply(all_scales_no_geo, \(x) x[!sapply(x, is.null)])


  # For all scales, list the tables that will be saved
  qs_table_list <-
    map_over_scales(
      all_scales = all_scales_no_geo,
      fun = \(geo = geo, scales = scales, scale_name = scale_name,
        scale_df = scale_df) {
        var_combinations <-
          lapply(variables$var_code, \(y) {
            vars <- names(scale_df)[grepl(y, names(scale_df))]
            vars <- stringr::str_subset(vars, "_q5|_q3", negate = TRUE)

            sapply(vars, \(x) {
              time_format <- "\\d{4}$"
              q3 <- paste0(
                gsub(time_format, "", x),
                if (grepl(time_format, x)) "q3_" else "_q3",
                stats::na.omit(stringr::str_extract(x, time_format))
              )
              q5 <- paste0(
                gsub(time_format, "", x),
                if (grepl(time_format, x)) "q5_" else "_q5",
                stats::na.omit(stringr::str_extract(x, time_format))
              )

              c(x, q3, q5)
            }, simplify = FALSE, USE.NAMES = TRUE)
          })
        var_combinations <- Reduce(c, var_combinations)

        lapply(var_combinations, \(x) scale_df[, c("ID", x)])
      }
    )

  # Create folders
  all_tables <- cc.buildr::reconstruct_all_tables(all_scales = all_scales)
  mapply(\(region, scales) {
    lapply(scales, \(scale_name) {
      folder_path <- paste0(data_folder, "/", region, "/", scale_name)
      if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE)
      }
    })
  }, names(all_tables), all_tables)

  # Save the scales in the database
  all_files <- list.files(data_folder, recursive = TRUE, full.names = TRUE)
  mapply(\(region, scales) {
    mapply(\(scale_name, tables) {
      mapply(\(table_name, table) {
        path <- sprintf("%s/%s/%s/%s.qs", data_folder, region, scale_name, table_name)

        # if (path %in% all_files) {
        #   old_table <- qs::qread(path)
        #   if (!identical(table, old_table)) qs::qsave(table, file = path)
        # } else {
        qs::qsave(table, file = path)
        # }
      }, names(tables), tables)
    }, names(scales), scales)
  }, names(qs_table_list), qs_table_list)

  return(invisible(NULL))
}

#' Save every scales in their own SQLite database
#'
#' @param data_folder <`character`> Where the `.sqlite` databases should be
#' written to. Defaults to `data/`.
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#' @param variables <`data.frame`> The `variables` data.frame, normally
#' `scales_variables_modules$variables`.
#'
#' @return Returns an error or nothing if ran successfully. Every existing region-geo
#' combination is a new SQLite db, and every variable is a table saved in each
#' of the db.
#' @export
save_all_scales_sqlite <- function(data_folder = "data/", all_scales, variables) {
  # Drop geometry of other scales
  all_scales_no_geo <-
    map_over_scales(
      all_scales = all_scales,
      fun = \(geo = geo, scales = scales, scale_name = scale_name,
        scale_df = scale_df) {
        sf::st_drop_geometry(scale_df)
      }
    )
  all_scales_no_geo <- lapply(all_scales_no_geo, \(x) x[!sapply(x, is.null)])


  # For all scales, list the tables that will be saved
  sql_table_list <-
    map_over_scales(
      all_scales = all_scales_no_geo,
      fun = \(geo = geo, scales = scales, scale_name = scale_name,
        scale_df = scale_df) {
        var_combinations <-
          lapply(variables$var_code, \(y) {
            vars <- names(scale_df)[grepl(y, names(scale_df))]
            vars <- stringr::str_subset(vars, "_q5|_q3", negate = TRUE)

            sapply(vars, \(x) {
              time_format <- "\\d{4}$"
              q3 <- paste0(
                gsub(time_format, "", x),
                if (grepl(time_format, x)) "q3_" else "_q3",
                stats::na.omit(stringr::str_extract(x, time_format))
              )
              q5 <- paste0(
                gsub(time_format, "", x),
                if (grepl(time_format, x)) "q5_" else "_q5",
                stats::na.omit(stringr::str_extract(x, time_format))
              )

              c(x, q3, q5)
            }, simplify = FALSE, USE.NAMES = TRUE)
          })
        var_combinations <- Reduce(c, var_combinations)

        lapply(var_combinations, \(x) scale_df[, c("ID", x)])
      }
    )

  # Save the scales in the database
  map_over_scales(
    all_scales = all_scales_no_geo,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
      scale_df = scale_df) {
      geo_scale <- paste(geo, scale_name, sep = "_")

      geo_scale_table_list <- sql_table_list[[geo]][[scale_name]]

      sqlite_path <- paste0(data_folder, geo_scale, ".sqlite")

      db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
      mapply(
        \(df, y)
        DBI::dbWriteTable(db, y, df, overwrite = TRUE),
        geo_scale_table_list, names(geo_scale_table_list)
      )
      DBI::dbDisconnect(db)
    }
  )

  # Add centroid
  map_over_scales(
    all_scales = all_scales_no_geo,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
      scale_df = scale_df) {
      geo_scale <- paste(geo, scale_name, sep = "_")
      with_geo <- all_scales[[geo]][[scale_name]][, "ID"]

      centroids <- lapply(with_geo$geometry, sf::st_centroid)
      lat <- sapply(centroids, `[[`, 1)
      lon <- sapply(centroids, `[[`, 2)

      df <- sf::st_drop_geometry(with_geo)

      df$lat <- lat
      df$lon <- lon

      sqlite_path <- paste0(data_folder, geo_scale, ".sqlite")

      db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
      DBI::dbWriteTable(db, "centroid", df, overwrite = TRUE)
      DBI::dbDisconnect(db)
    }
  )

  # Keep strings of all available tables in each db
  tables_in_sql <- map_over_scales(
    all_scales = sql_table_list,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
      scale_df = scale_df) {
      names(scale_df)
    }
  )
  tables_in_sql <- unlist(tables_in_sql, recursive = FALSE)
  names(tables_in_sql) <- gsub("\\.", "_", names(tables_in_sql))
  qs::qsave(tables_in_sql, file = paste0(data_folder, "tables_in_sql.qs"))

  return(invisible(NULL))
}

#' Save short tables as .qsm (regions regrouped)
#'
#' @param data_folder <`character`> Where the `.qsm` files should be
#' written to. Defaults to `data/`.
#' @param all_scales <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions, normally
#' `scales_variables_modules$scales`.
#' @param skip_scales <`character vector`> Scales to skip (not to keep as
#' a short table). These scales should be saved as a sqlite database instead, if
#' they are too large to be kept on memory. Defaults to an empty vector, no
#' scales are skipped.
#'
#' @return Returns an error or nothing if ran successfully. Every `region` is
#' its own `.qsm` file in which there are all the scales trimed down to only
#' the columns from `ID` to `households` (NO data columns).
#' @export
save_short_tables_qs <- function(data_folder = "data/", all_scales,
                                 skip_scales = c()) {
  mapply(\(scls, geo) {
    scls <- mapply(\(x, y) {
      d <- sf::st_drop_geometry(x)
      subs <- grepl("ID$|^name$|^name_2$|^population$|^households$|^centroid$", names(d))
      d[, subs]
    }, scls, names(scls), SIMPLIFY = FALSE)
    scls <- scls[!sapply(scls, is.null)]
    if (length(scls) == 0) {
      return(NULL)
    }
    scls <- scls[!names(scls) %in% skip_scales]
    names(scls) <- paste(geo, names(scls), sep = "_")

    for (i in seq_len(length(scls))) {
      assign(names(scls)[[i]], scls[[i]])
    }

    do.call(qs::qsavem, c(lapply(names(scls), rlang::sym),
      file = paste0(data_folder, geo, ".qsm")
    ))
  }, all_scales, names(all_scales))

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

  map_over_scales(
    all_scales = all_scales,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
      scale_df = scale_df) {
      geo_scale <- paste(geo, scale_name, sep = "_")
      out <- scale_df[, grepl("ID$", names(scale_df))]
      file_link <- paste0(data_folder, "geometry_export/", geo_scale, ".qs")
      qs::qsave(out, file = file_link)
    }
  )

  return(invisible(NULL))
}
