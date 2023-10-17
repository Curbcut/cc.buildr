#' Create all basic map zoom levels
#'
#' These zoom levels are used used to inform the zoom slider. The default
#' goes: first scale in the region, then `CT`, `DA` and `building` or `street`.
#'
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param zoom_levels <`named list`> A numeric list with names. Every scale
#' that is not located first in the `scales_sequences`'s list must be part of this list,
#' with a numeric applied to it (at which zoom level will the user switch between
#' one scale to another on auto-zoom). The first element of every `scales_sequences`'s list
#' of scales has the name `first` in that list, which will always be defaulted
#' to 0.
#'
#' @return Returns a named list of all basic map zoom levels.
#' @export
map_zoom_levels_create_all <- function(scales_sequences,
                                       zoom_levels = list(
                                         first = 0, CT = 10,
                                         DA = 12, building = 16
                                       )) {
  if (!"first" %in% names(zoom_levels)) {
    stop("The first scale in `zoom_levels` must be named `first`.")
  }

  levels <- lapply(scales_sequences, \(scales) {
    out <- sapply(scales, \(s) {
      if (s == scales[[1]]) {
        zoom_levels$first
      } else {
        unlist(zoom_levels[names(zoom_levels) == s]) |> unname()
      }
    })

    if (sum(sapply(out, is.null)) > 0) {
      stop(paste0(
        "Scale `", names(out)[sapply(out, is.null)], "` does not ",
        "have a zoom_levels supplied."
      ))
    }

    out

  })

  names(levels) <- paste0("mzl_", sapply(scales_sequences, paste0, collapse = "_"))

  return(levels)
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
    assign(z_l, deep)
  }
  do.call(qs::qsavem, c(
    lapply(names(map_zoom_levels), rlang::sym),
    file = paste0(data_folder, "map_zoom_levels.qsm")
  ))
}
