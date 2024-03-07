#' Consolidate Scales
#'
#' This function takes all the scales and ensure uniformity. It also notes,
#' using the sequences of scales, all the IDs of scales above them. It also
#' calculates all the features which are part of regions.
#'
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param all_scales <`named list`> A list of data frames corresponding to each
#' scale.
#' @param regions <`named list`> A list or unioned polygons of regions for which the
#' consolidated data needs to be processed.
#' @param crs <`character`> A Coordinate Reference System.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{scales}: A modified list of all scales with corrected identifiers.
#'     \item \code{for_region_dict_scales}: A list of scale IDs for each region,
#'     spatially filtered based on 1% threshold.
#'   }
#' @export
consolidate_scales <- function(scales_sequences, all_scales, regions, crs) {

  ## Make sure IDs are unique ------------------------------------------------

  subset_ID <- sapply(all_scales, `[[`, "ID")
  subset_ID <- unlist(subset_ID)
  duplicated_IDs <- sum(table(subset_ID) > 1)

  if (duplicated_IDs > 0) {
    stop(paste0(
      "`ID` aren't unique between all the scales. Curbcut makes ",
      "multiple assumptions that IDs ARE unique accross all the ",
      "dataframes on the plateform."
    ))
  }

  ## Add own ID to scales, and rename census ---------------------------------

  uniform_IDs <-
    mapply(
      \(scale_name, scale_df) {

        # For all column names that end with `UID`, change it to `_ID`
        if (sum(grepl("UID$", names(scale_df))) > 0) {
          names(scale_df)[grepl("UID$", names(scale_df))] <-
            gsub("UID", "_ID", names(scale_df)[grepl("UID$", names(scale_df))])
        }

        # Add own ID to scale
        scale_df[[paste0(scale_name, "_ID")]] <- scale_df$ID

        # Reorder columns
        if (!"name_2" %in% names(scale_df)) scale_df$name_2 <- NA

        reorder_columns(scale_df)
      }, names(all_scales), all_scales,
      SIMPLIFY = FALSE, USE.NAMES = TRUE)


  # Add all *_ID ------------------------------------------------------------

  scales <- lapply(uniform_IDs, sf::st_transform, crs)

  for (seq in scales_sequences) {
    for (scale_name in seq) {
      # Skip first scale
      if (scale_name == seq[[1]]) next
      # Skip small scales
      if (scale_name %in% c("street", "building")) next
      # SKIP GRIDS
      if (grepl("^grd\\d", scale_name)) next


      # Grab the `df` in which to add the *_ID
      lower <- scales[[scale_name]]

      # Get which scales the _ID should be added to
      higher <- seq[1:(which(scale_name == seq) - 1)]

      # Loop over all the higher scale to apply the *_ID
      for (hi in higher) {
        # If the *_ID is already present, skip!
        hi_df <- scales[[hi]]
        id_name <- sprintf("%s_ID", hi)
        if (id_name %in% names(lower)) next

        # Only keep the necessary piece to bind
        hi_df <- hi_df["ID"]
        names(hi_df)[1] <- id_name

        # Transfer the lower `df` to points and perform a spatial join
        df_points_on_surface <- suppressWarnings(sf::st_point_on_surface(lower))
        merged_centroids <- sf::st_join(df_points_on_surface, hi_df)

        # For the NA result, spatial filter
        missing <- merged_centroids[is.na(merged_centroids[[id_name]]), ]
        if (nrow(missing) > 0) {
          id_fit <- sapply(seq_along(missing$geometry),
                           \(r) get_largest_intersection(missing[r, ], hi_df),
                           simplify = TRUE, USE.NAMES = FALSE)

          # Add to the missing *_ID the ID of the largest intersection
          merged_centroids[[id_name]][is.na(merged_centroids[[id_name]])] <-
            unlist(id_fit) |> unname()
        }

        # Duplicates?
        merged <- sf::st_drop_geometry(merged_centroids)
        merged <- merged[!grepl("geometry", names(merged))]
        merged_split <- split(merged, merged$ID)
        merged_split <- lapply(merged_split, \(x) {
          x[[id_name]] <- list(x[[id_name]])
          out <- unique(x)
          if (nrow(out) > 1)
            stop(sprintf(paste0("Duplicates in the %s column for table %s ",
                                "after spatial join."), id_name, scale_name))
          return(out)
        })

        out <- data.table::rbindlist(merged_split) |> tibble::as_tibble()

        # Revert to full geometry
        lower_cols <- c("ID", "geometry", "geometry_digital")
        out <- merge(out,
                     lower[, names(lower) %in% lower_cols],
                     by = "ID")
        out <- tibble::as_tibble(out)
        out <- sf::st_as_sf(out)

        # Apply to the scale
        scales[[scale_name]] <- out

      }

    }
  }

  scales_merged_ids <- scales

  # Add an area column for every scale --------------------------------------

  scales <- mapply(
    \(scale_name, scale_df) {
      if (scale_name %in% c("street", "building")) {
        return(scale_df)
      }
      # SKIP GRIDS
      if (grepl("^grd\\d", scale_name)) return(scale_df)

      scale_df$area <- get_area(scale_df)
      scale_df
    }, names(scales), scales,
    SIMPLIFY = FALSE)


  # Return for every region the ID of ALL scales ----------------------------

  out_for_dict <- lapply(regions, \(region) {

    # Transform
    region <- sf::st_transform(region, crs)

    # Take out too small scales, like street and building which would make this function
    # last for hours.
    take_out_small <- scales[!names(scales) %in% c("street", "building")]

    # Other technique for grid cells (simple spatial filtering)
    grid_cells <- take_out_small[grepl("^grd\\d", names(take_out_small))]
    ids_grids <- lapply(grid_cells, \(scale_df) {
      sf::st_filter(scale_df, region)$ID
    })
    names(ids_grids) <- names(grid_cells)

    # Other technique for grid cells (simple spatial filtering)
    take_out_small <- take_out_small[!grepl("^grd\\d", names(take_out_small))]

    # Spatialy filter scales that have 10% of their content inside of x region.
    ids <- lapply(take_out_small, \(scale_df) {
      spatial_filtering(
        df = scale_df,
        crs = crs,
        master_polygon = region,
        ID_col = "ID",
        area_threshold = 0.1
      )$ID
    })

    # Rename to send, for each region, a named list of all scales with the IDs
    # that fall within the region.
    names(ids) <- names(take_out_small)


    return(c(ids_grids, ids))

  })


  ## Get the CRS back to WGS 84 ---------------------------------------------

  out <- lapply(scales, sf::st_transform, 4326)


  ## Post-processing (make sure all geometry types are right) ---------------

  out <- post_processing(out)


  ## Add a centroid vector column -------------------------------------------

  out <-
    mapply(
      \(scale_name, scale_df) {
        df <- sf::st_make_valid(scale_df)
        if (scale_name %in% c("street", "building")) {
          return(df)
        }

        centroids <- suppressWarnings(lapply(
          sf::st_centroid(df)$geometry,
          sf::st_coordinates
        ))
        centroids <- lapply(centroids, as.vector)
        df$centroid <- centroids

        reorder_columns(df)
      }, names(out), out,
      SIMPLIFY = FALSE, USE.NAMES = TRUE)


  # Return the final output -------------------------------------------------

  return(list(scales = out, for_region_dict_scales = out_for_dict))
}
