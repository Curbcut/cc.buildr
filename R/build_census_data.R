#' Build census data at all possible scales
#'
#' @param scales_consolidated A named list of sf data.frame
#' containing all scales listed with their regions. The output of
#' \code{\link[cc.buildr]{consolidate_scales}}.
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param census_vectors <`character vector`> Data variables that should be added
#' to the scales. By default, all: \code{\link[cc.data]{census_vectors}}. Look
#' at the \code{\link[cc.data]{census_vectors_table}} to view all
#' variables explained.
#' @param census_years <`character vector`> Years for which the census data
#' should be added to the scales. Defaults to \code{\link[cc.data]{census_years}}
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return Returns a list of length 3. The first is the possible scales for which
#' census data can be added in scales_consolidated. The second is a
#' data.frame of scales reference, to know for which scales the data is
#' available. It can directly be fed to the scales argument of add_variable.
#' The third is a data.frame of interpolation reference, to know for which
#' scales the data has been interpolated. It can directly be fed to the
#' interpolated argument of add_variable.
#' @export
build_census_data <- function(scales_consolidated, region_DA_IDs,
                              census_vectors, census_years, crs) {
  # Filter out scales smaller than DAs --------------------------------------

  higher_DA <- lapply(scales_consolidated, \(x) {
    if (!"DA" %in% names(x)) {
      return(x)
    }
    x[seq_len(which("DA" == names(x)))]
  })
  # Only keep the ID column
  higher_DA <-
    map_over_scales(
      all_scales = higher_DA,
      fun = \(scale_df = scale_df, ...) scale_df["ID"]
    )


  # Filter in only zones that are not in the census -------------------------

  missing <-
    sapply(higher_DA, \(x) {
      mapply(\(df, scale) {
        # Special cases for CT, where sometimes CSDs might fill in gaps
        if (scale == "CT") {
          csd_data <- cc.data::census_all_ids[["CSD"]]
          csd_data <- paste0("CSD_", csd_data)
        }

        all_possible_ids <-
          if (scale == "CT") {
            c(cc.data::census_all_ids[[scale]], csd_data)
          } else {
            cc.data::census_all_ids[[scale]]
          }

        missing_ids <- df$ID[!df$ID %in% all_possible_ids]
        df[df$ID %in% missing_ids, ]
      }, x, names(x), SIMPLIFY = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)


  # Regroup all missing zones per scales to interpolate only once -----------

  all_scales <- unique(unlist(sapply(scales_consolidated, names)))
  ready <-
    sapply(all_scales, \(scale) {
      miss <- sapply(missing, \(m) {
        m[[scale]]$ID
        dat <- m[[scale]]
        dat[dat$ID %in% m[[scale]]$ID, ]
      }, simplify = FALSE, USE.NAMES = TRUE)
      miss <- miss[unlist(sapply(miss, \(x) nrow(x) > 0 && !is.null(x)))]
      out <- unique(Reduce(rbind, miss))
      if (!is.null(out)) out
    }, simplify = FALSE, USE.NAMES = TRUE)
  ready <- ready[!sapply(ready, is.null)]


  # Interpolate -------------------------------------------------------------

  filled_vals <- cc.data::census_custom_boundaries(
    destination = ready,
    DA_IDs = region_DA_IDs,
    census_vectors = census_vectors,
    census_years = census_years,
    crs = crs
  )


  # Merge processed census data with interpolated data ----------------------

  pb <- progressr::progressor(steps = sum(sapply(higher_DA, length)))

  census_data_merged <- future.apply::future_sapply(higher_DA, \(x) {
    mapply(\(df, scale) {
      # Get the census data for that scale
      ids <- if (scale == "CT") {
        ids <- df$ID[!df$ID %in% filled_vals[[scale]]$ID]
        ids[!grepl("CSD_", ids)]
      } else {
        df$ID[!df$ID %in% filled_vals[[scale]]$ID]
      }

      if (length(ids) > 0 && scale %in% cc.data::census_scales) {
        census_data <- cc.data::db_read_data(
          table = paste0("processed_", scale),
          columns = sapply(
            census_years,
            \(x) paste(census_vectors, x, sep = "_")
          ),
          IDs = ids,
          crs = crs
        )
      } else {
        census_data <- tibble::tibble()
      }

      # Bind all the data
      all_data <- if (scale %in% cc.data::census_scales) {
        sf::st_drop_geometry(rbind(census_data, filled_vals[[scale]]))
      } else {
        sf::st_drop_geometry(filled_vals[[scale]])
      }

      out_df <- merge(df, all_data, by = "ID", all.x = TRUE)

      if (scale == "CT") {
        csd_ids <- df$ID[!df$ID %in% filled_vals[[scale]]$ID]
        csd_ids <- csd_ids[grepl("CSD_", csd_ids)]
        csd_ids <- gsub("CSD_", "", csd_ids)

        if (length(csd_ids) > 0) {
          csd_data <- cc.data::db_read_data(
            table = paste0("processed_", "CSD"),
            columns = sapply(
              census_years,
              \(x) paste(census_vectors, x, sep = "_")
            ),
            IDs = csd_ids,
            crs = crs
          ) |>
            sf::st_drop_geometry()

          ct_csds <- out_df[!out_df$ID %in% paste0("CSD_", csd_ids), ]
          csd_data$ID <- paste0("CSD_", csd_data$ID)
          csd_data <- merge(csd_data, out_df[, "ID"], by = "ID")

          out_df <- rbind(ct_csds, csd_data)
        } else {
          out_df
        }
      }

      pb()

      return(out_df)
    }, x, names(x), SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)


  # Merge to scales_consolidated --------------------------------------------

  final_scales <-
    map_over_scales(
      all_scales = scales_consolidated,
      \(geo = geo, scales = scales,
        scale_df = scale_df, scale_name = scale_name) {
        if (is.null(census_data_merged[[geo]][[scale_name]])) {
          return(scale_df)
        }
        merge(
          scale_df,
          sf::st_drop_geometry(census_data_merged[[geo]][[scale_name]])
        )
      }
    )
  final_scales <- reorder_columns(final_scales)


  # Keep track of scales for which census data is available ------------------

  avail_scales <-
    map_over_scales(census_data_merged,
      fun = \(geo = geo, scale_name = scale_name, ...) {
        tibble::tibble(
          geo = geo,
          scale = scale_name
        )
      }
    )
  avail_scales <-
    sapply(avail_scales, \(x) do.call(rbind, x),
      simplify = FALSE, USE.NAMES = TRUE
    ) |>
    (\(x) do.call(rbind, x))()
  row.names(avail_scales) <- NULL


  # Create interpolated references as a data.frame --------------------------

  interpolated_ref <-
    sapply(sapply(higher_DA, names), \(scales) {
      sapply(scales, \(scale) {
        if (!scale %in% names(census_data_merged)) {
          return("DA")
        }
        return(FALSE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  interpolated_ref <-
    map_over_scales(interpolated_ref,
      fun = \(geo = geo, scale_name = scale_name,
        scale_df = scale_df, ...) {
        tibble::tibble(
          geo = geo,
          scale = scale_name,
          interpolated_from = scale_df
        )
      }
    )
  interpolated_ref <-
    sapply(interpolated_ref, \(x) do.call(rbind, x),
      simplify = FALSE, USE.NAMES = TRUE
    ) |>
    (\(x) do.call(rbind, x))()
  row.names(interpolated_ref) <- NULL


  # Return data with available scales and interpolated reference ------------

  return(list(
    scales = final_scales,
    avail_scales = avail_scales,
    interpolated_ref = interpolated_ref
  ))
}
