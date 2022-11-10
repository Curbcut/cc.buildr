## COLLECTION OF UTILS FUNCTIONS ###############################################

#' Map over the depth of the list of scales
#'
#' As we follow a deep list of scales in regions, we can use this `map_over_scales()`
#' helper function to iterate functions on all the scales.
#'
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#' @param fun <`function`> A function to run on all scales.
#'
#' @return Returns the all_scales list that is fed, with the `fun` ran
#' on all the scales.
#' @export
map_over_scales <- function(all_scales, fun) {
  pb <- progressr::progressor(steps = sum(sapply(all_scales, length)))
  ## TKTK WHY DOESN'T WORK WITH FUTURE.APPLY ?
  mapply(\(geo, scales) {
    mapply(\(scale_name, scale_df) {
      pb()
      fun(
        geo = geo, scales = scales,
        scale_name = scale_name, scale_df = scale_df
      )
    }, names(scales), scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }, names(all_scales), all_scales, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

#' Reconstruct all_tables
#'
#' As all the data arrange in a named list of regions > scales was created using
#' a list of character vector previously named `all_tables`, this function
#' allows to recreate the `all_tables` list of vectors using the data of
#' `all_scales`.
#'
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#'
#' @return A list of vectors of characters. The first level of the list is the
#' geo, and the second is all the scales available in that particular geo.
#' @export
reconstruct_all_tables <- function(all_scales) {
  mapply(names, all_scales, SIMPLIFY = FALSE)
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
#' @param all_scales <`named list`> A named list of scales. The first level is
#' the geo, and the second is the scales.
#'
#' @return Returns the same list that is fed, with columns re-ordered.
#' @export
reorder_columns <- function(all_scales) {
  cc.buildr::map_over_scales(
    all_scales = all_scales,
    fun = \(geo = geo, scales = scales,
      scale_name = scale_name, scale_df = scale_df) {
      scales_order <- names(scales)
      needed_present_scales <-
        scales_order[seq_len(which(scales_order == scale_name))]

      mandatory_start <- c("ID", "name", "name_2")
      all_ids <- paste0(needed_present_scales, "_ID")
      other <- character()

      if ("population" %in% names(scale_df)) other <- c(other, "population")
      if ("households" %in% names(scale_df)) other <- c(other, "households")

      rest <- names(scale_df)[!names(scale_df) %in% c(
        mandatory_start, all_ids,
        other
      )]
      rest <- rest[!rest %in% c("popw_centroids_coords", "geometry")]
      last <- names(scale_df)[
        names(scale_df) %in% c("popw_centroids_coords", "geometry")
      ]

      out <- scale_df[, c(mandatory_start, all_ids, other, rest, last)]

      if (ncol(out) != ncol(scale_df)) {
        stop(
          paste0(
            "Some columns have been dropped along the way in the use of ",
            "x function."
          )
        )
      }

      out
    }
  )
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
merge <- function(x, y, ...) {
  merged <- tibble::as_tibble(base::merge(x, y, ...))
  if ("sf" %in% class(x) || "sf" %in% class(y)) merged <- sf::st_as_sf(merged)
  merged
}
