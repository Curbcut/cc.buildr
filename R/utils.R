## COLLECTION OF UTILS FUNCTIONS ###############################################

#' Map over the depth of the list of scales
#'
#' As we follow a deep list of scales in regions, we can use this `map_over_scales()`
#' helper function to iterate functions on all the scales.
#'
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#' @param fun <`function`> A function to run on all scales. Possible arguments
#' are `geo` (names of regions), `scales` (scales data.frame), 3
#' `scale_name` (names of scales inside a region), `scale_df` (data.frame, single
#' scale)
#' @param with_progress <`logical`> Should there be a progress bar? Slows things
#' down when the function is just too fast.
#' @param parallel <`logical`> Should the function run in parallel. Defaults to FALSE
#'
#' @return Returns the all_scales list that is fed, with the `fun` ran
#' on all the scales.
#' @export
map_over_scales <- function(all_scales, fun, with_progress = TRUE, parallel = FALSE) {
  if (with_progress) pb <- progressr::progressor(steps = sum(sapply(all_scales, length)))

  out <- if (parallel) {
    # Extract the names of the geographical levels
    geo_names <- names(all_scales)

    # Loop over each geographical level
    result <- lapply(seq_along(all_scales), function(i) {
      geo <- geo_names[i]
      scales_at_geo <- all_scales[[i]]

      # Extract the names of the scales at the current geographical level
      scale_names <- names(scales_at_geo)

      # Loop over each scale at the current geographical level
      res_at_geo <- lapply(seq_along(scales_at_geo), function(j) {
        scale_name <- scale_names[j]
        scale_df <- scales_at_geo[[j]]

        # Apply the user-defined function to the current geographical level and scale
        out <- fun(
          geo = geo, scales = scales_at_geo,
          scale_name = scale_name, scale_df = scale_df
        )

        # Update progress bar if required
        if (with_progress) pb()
        out
      })

      # Preserve the names of the scales at the current geographical level
      names(res_at_geo) <- scale_names
      res_at_geo
    })

    # Preserve the names of the geographical levels
    names(result) <- geo_names
    result
  } else {
    mapply(\(geo, scales) {
      mapply(\(scale_name, scale_df) {
        out <- fun(
          geo = geo, scales = scales,
          scale_name = scale_name, scale_df = scale_df
        )
        if (with_progress) pb()
        out
      }, names(scales), scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    }, names(all_scales), all_scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }

  return(out)
}

#' Compute geometric measurements and drop units
#'
#' @param geometry <`sf`, `sfc` or `sfg`> A column of geometries.
#' @param ... passed on to \code{\link[sf]{st_area}}
#'
#' @return A numeric vector of shape areas.
#' @export
get_area <- function(geometry, ...) {
  x <- sf::st_area(geometry)
  as.vector(x)
}

#' Reorder columns as they are expected in the workflow
#'
#' @param scale_df <`data.frame`> A `scale` data.frame for which columns
#' should be reordered.
#'
#' @return Returns the same list that is fed, with columns re-ordered.
#' @export
reorder_columns <- function(scale_df) {

  mandatory_start <- c("ID", "name", "name_2")
  all_ids <- names(scale_df)[grepl("_ID", names(scale_df))]
  other <- character()

  if ("population" %in% names(scale_df)) other <- c(other, "population")
  if ("households" %in% names(scale_df)) other <- c(other, "households")
  if ("area" %in% names(scale_df)) other <- c(other, "area")

  rest <- names(scale_df)[!names(scale_df) %in% c(
    mandatory_start, all_ids,
    other
  )]
  rest <- rest[!rest %in% c("popw_centroids_coords", "centroid", "geometry", "geometry_digital")]
  last <- names(scale_df)[
    names(scale_df) %in% c("popw_centroids_coords", "centroid", "geometry", "geometry_digital")
  ]

  out <- scale_df[, c(mandatory_start, all_ids, other, rest, last)]

  if (ncol(out) != ncol(scale_df)) {
    stop(
      paste0(
        "Some columns have been dropped along the way in the use of ",
        "`reorder_columns` function."
      )
    )
  }

  out

}

#' Reimplemntation of dplyr::ntile in base R
#'
#' @param x <`numeric vector`> A vector of values to rank. Missing values are
#' left as is. If you want to treat them as the smallest or largest values,
#' replace with Inf or -Inf before ranking.
#' @param n <`numeric`> Number of groups to split up into.
#'
#' @return A rough rank, which breaks the input vector into n buckets. The size
#' of the buckets may differ by up to one, larger buckets have lower rank.
#' @export
rough_rank <- function(x, n) {
  x <- rank(x, ties.method = "first", na.last = "keep")
  len <- length(x) - sum(is.na(x))
  if (len == 0L) {
    rep(NA_integer_, length(x))
  } else {
    n <- as.integer(floor(n))
    n_larger <- as.integer(len %% n)
    n_smaller <- as.integer(n - n_larger)
    size <- len / n
    larger_size <- as.integer(ceiling(size))
    smaller_size <- as.integer(floor(size))
    larger_threshold <- larger_size * n_larger
    bins <- ifelse(x <= larger_threshold,
      (x + (larger_size - 1L)) / larger_size,
      (x + (-larger_threshold + smaller_size - 1L)) /
        smaller_size + n_larger
    )
    as.integer(floor(bins))
  }
}

#' Merge two tibbles and keep the output a tibble
#'
#' @param x <`data.frame`> data frame, or object to be coerced to one.
#' @param y <`data.frame`> data frame, or object to be coerced to one.
#' @param ... arguments to be passed to \code{\link[base]{merge}}.
#'
#' @return Merged tibbles as a tibble and keep sf class if was present
#' @export
merge <- function(x, y, by = intersect(names(x), names(y)), by.x = by,
                  by.y = by, ...) {

  # Store 'geometry_digital' from 'x' and 'y' along with keys for joining later
  if ("geometry_digital" %in% names(x)) {
    x_geometry_digital <- x[, c("geometry_digital", by.x)]
    x_geometry_digital <- sf::st_drop_geometry(x_geometry_digital)
    x <- x[setdiff(names(x), "geometry_digital")]
  }
  if ("geometry_digital" %in% names(y)) {
    y_geometry_digital <- y[, c("geometry_digital", by.y)]
    y_geometry_digital <- sf::st_drop_geometry(y_geometry_digital)
    y <- y[setdiff(names(y), "geometry_digital")]
  }

  # Merge 'x' and 'y' using base merge function
  merged <- base::merge(x, y, by.x = by.x, by.y = by.y, ...)

  if (exists("x_geometry_digital")) {
    merged <- dplyr::left_join(merged, x_geometry_digital, by = by.x)
  }
  if (exists("y_geometry_digital") && !exists("x_geometry_digital")) { # Avoid overwriting 'x' geometry
    merged <- dplyr::left_join(merged, y_geometry_digital, by = by.y)
  }

  # As tibble
  merged <- tibble::as_tibble(merged)

  # If 'sf' class should be preserved, convert back to 'sf' object
  if ("sf" %in% class(x) || "sf" %in% class(y)) {
    merged <- sf::st_as_sf(merged)
  }

  merged
}

#' Spatial filtering function to keep polygons with at least x of their area
#' in the greater boundary.
#'
#' @param df <`sf data.frame`> The census data object to be transformed.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param master_polygon <`sfc_MULTIPOLYGON`> Unioned multipolygon of the boundaries
#' in which to keep the df features.
#' @param ID_col <`character`> The name of the ID column to be used for filtering.
#' Default is "ID".
#' @param area_threshold <`numeric`> The minimum percentage of area in the master polygon.
#' Default is 0.05, as a low percentage can be use for the fact that water can sometimes
#' be a large part of the polygons.
#'
#' @return A subset of the original data frame that meets the filtering condition.
#' @export
spatial_filtering <- function(df, crs, master_polygon, ID_col = "ID", area_threshold = 0.05) {
  # Transform the census data object
  keep_ids <- sf::st_transform(df, crs)

  # Calculate the area for the transformed data
  keep_ids$area <- get_area(keep_ids)

  # Transform the master polygon to the desired coordinate reference system
  master_poly_crs <- sf::st_transform(master_polygon, crs)

  # Find the intersection between the transformed data and the master polygon
  int <- sf::st_intersection(keep_ids, master_poly_crs)

  # Calculate the new area for the intersection
  int$area_new <- get_area(int)

  # Filter in only polygons that have area_threshold% of their area in the master polygon.
  filtered_ids <- int[[ID_col]][int$area_new / int$area > area_threshold]

  # Return a subset of the original data frame that meets the filtering condition
  return(df[df[[ID_col]] %in% filtered_ids, ])
}

get_largest_intersection <- function(x, other) {

  intersections <- sf::st_intersection(x, other)
  # If it intersects nothing, return NA
  if (nrow(intersections) == 0) return(NA)

  areas <- sf::st_area(intersections)
  index <- which.max(areas)
  out <- sf::st_drop_geometry(intersections)[index, names(other)[names(other) != "geometry"]]
  if (nrow(out) == 0) {
    dist <- sf::st_distance(x, other)
    index <- which.min(dist)
    out <- sf::st_drop_geometry(other)[index, ]
  }
  if (nrow(out) == 0) {
    out <- tibble::add_row(out)
    for (n in names(other)) {
      out[[n]] <- NA
    }
  }
  out
}
