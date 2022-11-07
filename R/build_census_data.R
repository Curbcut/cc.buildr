#' Build census data at all possible scales
#'
#' @param scales_consolidated A named list of sf data.frame
#' containing all scales listed with their regions. The output of
#' \code{\link[susbuildr]{consolidate_scales}}.
#' @param census_data "TKTK"
#' @param census_data_raw_DA "TKTK"
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
build_census_data <- function(scales_consolidated, census_data,
                              census_data_raw_DA, crs) {

  # Only build census data for scales DA and above
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
      \(scale_df = scale_df, ...) scale_df[c("ID", "geometry")]
    )

  # Get zones for which data is missng (interpolation will be needed)
  missing <-
    sapply(higher_DA, \(x) {
      mapply(\(df, scale) {
        if (scale == "CT") {
          csd_data <- census_data$CSD
          csd_data$ID <- paste0("CSD_", csd_data$ID)
        }

        all_possible_ids <-
          if (scale == "CT") {
            c(census_data[[scale]]$ID, csd_data$ID)
          } else {
            census_data[[scale]]$ID
          }

        missing_ids <- df$ID[!df$ID %in% all_possible_ids]
        df[df$ID %in% missing_ids, ]
      }, x, names(x), SIMPLIFY = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Regroup all missing zones per scales to interpolate only once
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

  # Interpolate using raw DA data
  filled_vals <- susdata::census_custom_boundaries(
    destination = ready,
    data_raw_DA = census_data_raw_DA,
    crs = crs
  )

  # Merge all census data to the scales
  census_data_merged <- sapply(higher_DA, \(x) {
    mapply(\(df, scale) {
      all_data <- if (scale %in% names(census_data)) {
        rbind(census_data[[scale]], sf::st_drop_geometry(filled_vals[[scale]]))
      } else {
        sf::st_drop_geometry(filled_vals[[scale]])
      }

      out_df <- merge(df, all_data, by = "ID", all.x = TRUE)

      if (scale == "CT") {
        csd_data <- census_data$CSD
        csd_data$ID <- paste0("CSD_", csd_data$ID)
        ct_csds <- out_df[out_df$ID %in% csd_data$ID, "ID"]
        ct_csds <- merge(ct_csds, csd_data, by = "ID", all.x = TRUE)
        out_df <- rbind(out_df[!out_df$ID %in% ct_csds$ID, ], ct_csds)
      }

      out_df
    }, x, names(x), SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Merge the the raw scale that has been fed and reorder columns
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

  # Scales at which the data is available
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

  # Create interpolated references as a data.frame
  interpolated_ref <-
    sapply(sapply(higher_DA, names), \(scales) {
      sapply(scales, \(scale) {
        if (!scale %in% names(census_data)) {
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

  # Return census data
  return(list(
    scales = final_scales,
    avail_scales = avail_scales,
    interpolated_ref = interpolated_ref
  ))
}
