#' Split a (probably census) layer with a set of boundaries to cut the first
#' layer with
#'
#' @param destination <`sf data.frame`> Probably a census layer, which is
#' the destination layer, for which some polygons should be cut using the
#' polygons of the `cutting_layer`.
#' @param cutting_layer <`sf data.frame`> An sf data.frame of subdivisions of one
#' or more polygons of the destination data.frame, e.g. for the City of Montreal,
#' the subdivisions is a dataframe of boroughs. The sf data.frame must have 3
#' columns: \code{name}, \code{type} and \code{geometry}.
#' The \code{name} is the name of the borough/neighbourhood/zone e.g.
#' "Le Plateau Mont-Royal", and \code{type} is what should be used as \code{"name_2"},
#' e.g. "Borough" (for the display name: Borough of Le Plateau Mont-Royal).
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame from which
#' population and households will be interpolated.
#' @param destination_pct_threshold <`numeric`> What is the threshold for which
#' an existing feature in the destination data.frame should be removed. Default to
#' 0.95. If 95% of the area of a destination feature fits in the `cutting_layer`,
#' then it should be swapped for the polygons in the `cutting_layer`.
#' @param buffer_around_cutting_layer <`numeric`> Needed if the boundaries of
#' the `cutting_layer` are too short of the removed destination features. The
#' `cutting_layer` should be expanded: defaults to 100m. This will NOT make
#' the `cutting_layer` overlap on non-removed features of the destination dataframe.
#' @param sampled_points_voronoi <`numeric`> Used in the same cases of
#' `buffer_around_cutting_layer`. We found that it works better if the ration
#' buffer:point is very high (e.g. 1:500).
#' @param crs <`numeric`> EPSG coordinate reference system to be
#' assigned, e.g. \code{32618} for Montreal.
#'
#' @return An sf dataframes with features subdivided.
#' @export
split_scale <- function(destination, cutting_layer,
                        DA_table,
                        destination_pct_threshold = 0.95,
                        buffer_around_cutting_layer = 100,
                        sampled_points_voronoi = buffer_around_cutting_layer * 500,
                        crs) {
  # Error checking
  if (!all(names(cutting_layer) %in% c("name", "type", "geometry"))) {
    stop(paste0(
      "The `cutting_layer` must have exactly three columns: `name`,",
      "`type` and `geometry`. Look at the function documentation."
    ))
  }

  # Transform to correct crs, and trim
  destination <- sf::st_transform(destination, crs)
  dest <- destination
  dest$dest_original_area <- cc.buildr::get_area(destination$geometry)
  dest <- dest[, c("ID", "dest_original_area", "geometry")]
  names(dest) <- c("dest_id", "dest_original_area", "geometry")
  cut <- sf::st_transform(cutting_layer, crs)
  cut$cut_original_area <- cc.buildr::get_area(cut$geometry)
  cut$cut_id <- seq_len(nrow(cut))
  cut <- cut[, c("name", "type", "cut_id", "cut_original_area", "geometry")]

  # Intersect
  intersected <- suppressWarnings(sf::st_intersection(dest, cut))
  intersected <-
    intersected[sf::st_geometry_type(intersected) %in% c("POLYGON", "MULTIPOLYGON"), ]

  # Identify the destination polygons that are getting removed
  intersected$dest_new_area <- cc.buildr::get_area(intersected$geometry)
  dest_ids_intersected_areas <-
    stats::aggregate(intersected$dest_new_area,
      by = list(dest_id = intersected$dest_id), FUN = sum
    )
  names(dest_ids_intersected_areas) <- c("dest_id", "stay_area")
  removed_ids <- merge(dest_ids_intersected_areas, dest, by = "dest_id")
  removed_ids$area_prop <-
    removed_ids$stay_area / removed_ids$dest_original_area
  removed_ids <-
    removed_ids$dest_id[removed_ids$area_prop > destination_pct_threshold]

  # Get the new shapes
  new_shapes <- intersected[intersected$dest_id %in% removed_ids, ]
  new_shapes$ID <- paste(new_shapes$dest_id, new_shapes$cut_id, sep = "_")
  new_shapes <- new_shapes[, c("ID", "name", "type", "geometry")]
  names(new_shapes) <- c("ID", "name", "name_2", "geometry")
  new_shapes <- sf::st_cast(new_shapes, "MULTIPOLYGON")

  # Retrieve the missing slivers from the removed destinations, that didn't intersect
  # with the cutting layer.
  missing_slivers <-
    suppressWarnings(sf::st_difference(
      destination[destination$ID %in% removed_ids, ],
      sf::st_union(new_shapes)
    ))
  missing_slivers <- suppressWarnings(sf::st_cast(missing_slivers, "POLYGON"))

  # Attribute these slivers to the correct new shapes
  new_shapes_buffered <- sf::st_buffer(new_shapes, buffer_around_cutting_layer)
  buffer_only <-
    suppressWarnings(sf::st_difference(new_shapes_buffered, sf::st_union(new_shapes)))
  sampled_points <- sf::st_sample(buffer_only,
    size = sampled_points_voronoi,
    type = "hexagonal"
  )
  voronoi <- sf::st_cast(sf::st_voronoi(sf::st_union(sampled_points)))
  voronoi_missing_slivers <-
    sf::st_as_sf(sf::st_intersection(
      sf::st_make_valid(voronoi),
      missing_slivers
    ))
  slivers_attributed <- sf::st_join(voronoi_missing_slivers,
    new_shapes,
    join = sf::st_nearest_feature
  )
  all_new_shapes <- lapply(unique(new_shapes$ID), \(x) {
    slivers <- slivers_attributed[slivers_attributed$ID == x, ]
    if (nrow(slivers) == 0) {
      return(new_shapes[new_shapes$ID == x, ])
    }
    new_shape <- new_shapes[new_shapes$ID == x, ]
    new_geo <- sf::st_union(sf::st_union(slivers), new_shape)
    new_shape <- sf::st_drop_geometry(new_shape)
    merge(new_shape, new_geo) |> sf::st_as_sf()
  })
  all_new_shapes <- sf::st_cast(Reduce(rbind, all_new_shapes), "MULTIPOLYGON")

  # Bind the non-removed destination polygons and the new shapes
  miss_cols <-
    names(destination)[!names(destination) %in% names(all_new_shapes)]
  all_new_shapes <- lapply(miss_cols, \(x) {
    all_new_shapes[[x]] <- NA
    sf::st_drop_geometry(all_new_shapes[, x])
  }) |> (\(x) Reduce(cbind, list(all_new_shapes, x)))()
  # Interpolate population and households for all new shapes
  all_new_shapes <-
    interpolate_from_area(
      to = all_new_shapes,
      from = DA_table,
      additive_vars = c("population", "households"),
      crs = crs
    )
  # Bind
  destination_out <- rbind(
    destination[!destination$ID %in% removed_ids, ],
    all_new_shapes
  )
  destination_out <- sf::st_make_valid(destination_out)

  # Add area
  destination_out$area <- get_area(destination_out)
  destination_out <- destination_out[
    c(names(destination_out)[names(destination_out) != "geometry"], "geometry")]

  # Go back to unprojected
  destination_out <- sf::st_transform(destination_out, 4326)
  destination_out <- sf::st_make_valid(destination_out)

  # Return
  return(destination_out)
}
