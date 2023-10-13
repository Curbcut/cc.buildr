#' Get Available Scale Combinations
#'
#' This function retrieves available scale combinations based on the provided
#' scale sequences and a character vector of scales for which the data of the page
#' is available. It filters out smaller scales that would normally show DA values
#' (like streets/buildings, so we keep scales sequences containing buildings) and
#' returns the combination of scales usable on a particular page.
#'
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param avail_scales <`character vector`> A vector of character containing
#' all the scales at which the data of the module is accessible.
#'
#' @return A character vector of available scale combinations.
#' @export
get_avail_scale_combinations <- function(scales_sequences, avail_scales) {
  possible_scales <- sapply(scales_sequences, function(scales) {
    # Take out smaller scales that will show DA values
    scales_ <- scales[!scales %in% c("street", "building")]

    # All scales of the sequence must be in the available scales
    all(scales_ %in% avail_scales)
  })

  avail_scale_combinations <- scales_sequences[possible_scales]
  avail_scale_combinations <- sapply(avail_scale_combinations, paste0, collapse = "_")

  return(avail_scale_combinations)
}
