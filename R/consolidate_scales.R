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


  ## DIFFERENCE BETWEEN EVERY SCALE AND DA, ONLY KEEP WHAT TOUCHES THE DAS
  ## FOR EVERY SCALE!! (LAND)
  ## AT INTERPOLATION STEP, REMOVE FROM ALL SCALE THE DB WITH POPULATION = 0?
  ## MAYBE ADD A NEW GEOMETRY COLUMN??






















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

  uniform_IDs <- lapply(uniform_IDs, sf::st_transform, crs)

  for (seq in scales_sequences) {
    for (scale_name in seq) {
      # Skip first scale
      if (scale_name == seq[[1]]) next
      # Skip small scales
      if (scale_name %in% c("street", "building")) next

      # Grab the `df` in which to add the *_ID
      lower <- uniform_IDs[[scale_name]]

      # Get which scales the _ID should be added to
      higher <- seq[1:(which(scale_name == seq) - 1)]

      # Loop over all the higher scale to apply the *_ID
      for (hi in higher) {
        # If the *_ID is already present, skip!
        hi_df <- uniform_IDs[[hi]]
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
        out <- merge(out,
                     lower[, c("ID", "geometry")],
                     by = "ID")
        out <- tibble::as_tibble(out)
        out <- sf::st_as_sf(out)

        # Apply to the scale
        uniform_IDs[[scale_name]] <- out

      }

    }
  }


  # Add an area column for every scale --------------------------------------

  uniform_IDs <- lapply(uniform_IDs, \(x) {
    x$area <- get_area(x)
    x
  })


  # Return for every region the ID of ALL scales ----------------------------

  out_for_dict <- future.apply::future_lapply(regions, \(region) {
    # Take out too small scales, like street and building which would make this function
    # last for hours.
    take_out_small <- all_scales[!names(all_scales) %in% c("street", "building")]

    # Spatialy filter scales that have 10% of their content inside of x region.
    ids <- lapply(take_out_small, \(scale_df) {

      region <- sf::st_transform(region, crs)
      df <- sf::st_transform(scale_df, crs)

      spatial_filtering(
        df = df,
        crs = crs,
        master_polygon = region,
        ID_col = "ID",
        area_threshold = 0.1
      )$ID
    })

    # Rename to send, for each region, a named list of all scales with the IDs
    # that fall within the region.
    names(ids) <- names(take_out_small)
    return(ids)

  })


  ## Get the CRS back to WGS 84 ---------------------------------------------

  out <- lapply(uniform_IDs, sf::st_transform, 4326)


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
