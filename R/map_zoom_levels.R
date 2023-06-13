#' Create all basic map zoom levels
#'
#' These zoom levels are used used to inform the zoom slider. The default
#' goes: first scale in the region, then `CT`, `DA` and `building` or `street`.
#'
#' @param all_tables <`named list`> A named list of character. The names are
#' the \code{region} e.g. CMA, island, city, ... and the vectors are all the
#' scales available in these geo e.g. CSD, CT, DA, building, ...
#' @param zoom_levels <`named list`> A numeric list with names. Every scale
#' that is not located first in the region's list must be part of this list,
#' with a numeric applied to it (at which zoom level will the user switch between
#' one scale to another on auto-zoom). The first element of every region's list
#' of scales has the name `first` in that list, which will always be defaulted
#' to 0.
#'
#' @return Returns a named list of all basic map zoom levels.
#' @export
map_zoom_levels_create_all <- function(all_tables,
                                       zoom_levels = list(
                                         first = 0, CT = 10.5,
                                         DA = 12.5, building = 15.5
                                       )) {
  levels <- lapply(all_tables, \(scales) {
    out <- sapply(scales, \(s) {
      if (s == scales[[1]]) {
        zoom_levels$first
      } else {
        unlist(zoom_levels[names(zoom_levels) == s])
      }
    })
    names(out) <- scales
    if (sum(sapply(out, is.null)) > 0) {
      stop(paste0(
        "Scale `", names(out)[sapply(out, is.null)], "` does not ",
        "have a zoom_levels supplied."
      ))
    }
    return(out)
  })


  levels <- lapply(levels, list)
  levels <- mapply(\(z, n) {
    stats::setNames(z, paste0("map_zoom_levels_", n))
  }, levels, names(levels), SIMPLIFY = FALSE, USE.NAMES = TRUE)

  return(levels)
}

#' Create a custom zoom level
#'
#' @param map_zoom_levels <`named list`> The previously created zoom levels
#' using \code{\link[cc.buildr]{map_zoom_levels_create_all}}
#' @param all_tables <`named list`> A named list of character. The names are
#' the \code{region} e.g. CMA, island, city, ... and the vectors are all the
#' scales available in these geo e.g. CSD, CT, DA, building, ...
#' @param region <`character`> The region for which this new custom map zoom
#' level should be for, e.g. `CMA`.
#' @param suffix <`character`> The name that should be appended to the name
#' of the map zoom level, e.g. `max_CT`. The full name would then be:
#' `map_zoom_levels_max_CT`.
#' @param content <`character`> The content of the zoom level, e.g.
#' \code{c("CSD" = 0, "CT" = 10.5)}.
#'
#' @return The same list as `map_zoom_levels` with the new custom zoom level
#' appended.
#' @export
map_zoom_levels_create_custom <- function(map_zoom_levels, all_tables, region,
                                          suffix, content) {
  if (!region %in% names(all_tables)) {
    stop("`region` is not in the list of regions from `all_tables`.")
  }

  new_zoom_level <- list(new_name = content)
  names(new_zoom_level) <- paste("map_zoom_levels", region, suffix, sep = "_")

  map_zoom_levels[[region]] <- c(map_zoom_levels[[region]], new_zoom_level)

  return(map_zoom_levels)
}

#' Save all map zoom levels in a `.qsm` file
#'
#' @param data_folder <`character`> Where the `.sqlite` databases should be
#' written to. Defaults to `data/`.
#' @param map_zoom_levels <`named list`> The previously created zoom levels
#' using \code{\link[cc.buildr]{map_zoom_levels_create_all}} and
#' \code{\link[cc.buildr]{map_zoom_levels_create_custom}}
#'
#' @return Returns  nothing if ran successfully.
#' @export
map_zoom_levels_save <- function(data_folder = "data/", map_zoom_levels) {
  for (z_l in names(map_zoom_levels)) {
    deep <- map_zoom_levels[[z_l]]
    for (i in seq_len(length(deep))) {
      assign(names(deep)[[i]], deep[[i]])
    }
  }
  do.call(qs::qsavem, c(
    lapply(unlist(sapply(map_zoom_levels, names)), rlang::sym) |>
      unname(),
    file = paste0(data_folder, "map_zoom_levels.qsm")
  ))
}
