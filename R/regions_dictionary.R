#' Build the regions dictionary
#'
#' @param all_regions <`named list`> Named list of regions. The name of every element
#' is a region.
#' @param region <`vector of character`> A vector of characters used to identify
#' which large geometry the user is interested in, e.g.
#' \code{c("CMA", "city", "island")}. Defaults to `names(all_regions)`
#' @param name <`named character vector`> Named with their corresponding region value.
#' Same length as the vector fed in the `region` argument. Used to name the region
#' when the user will want to change which large geometry they are interested in. e.g.
#' \code{c("Metropolitan Area", "City of Montreal", "Island of Montreal")}.
#' @param to_compare <`named character vector`> Named with their corresponding region value.
#' Same length as the vector fed in the `region` argument. Used by the dynamically generated
#' text, when a particular zone is compared with the whole geography. 'x DA
#' has a higher value than y% of DAs in the Montreal region'.
#' e.g. \code{c("in the Montreal region", "in the City of Montreal",
#' "on the island of Montreal")}
#' @param to_compare_determ <`named character vector`> Named with their corresponding
#' region value. Same length as the vector fed in the `region` argument. Used by the
#' dynamically generated text and must start with a determinant. 'This is usually affordable
#' for the Montreal region'. e.g. \code{c("the Montreal region", "the City of Montreal",
#' "the island of Montreal")}
#' @param to_compare_short <`named character vector`> Named with their corresponding
#' region value. Same length as the vector fed in the `region` argument. Used by the
#' dynamically generated text and is used for shorter compare. 'It is higher than 99%
#' of other cities in the region'. e.g. \code{c("in the region", "in the city",
#' "on the island")}
#' @param pickable <`named vector of logical`> Will the user be able to select this
#' scale as a default all over the platform?
#'
#' @return Returns the same vectors fed arranged in a data.frame ordered in
#' priorty.
#' @export
regions_dictionary <- function(all_regions, region = names(all_regions), name, to_compare,
                               to_compare_determ, to_compare_short, pickable) {
  # Error check
  if (is.null(names(name))) {
    stop("`region` must be a named character vector.")
  }
  if (is.null(names(to_compare))) {
    stop("`to_compare` must be a named character vector.")
  }
  if (is.null(names(to_compare))) {
    stop("`to_compare_determ` must be a named character vector.")
  }
  if (is.null(names(to_compare))) {
    stop("`to_compare_short` must be a named character vector.")
  }
  invisible(lapply(c(
    "region", "name", "to_compare", "to_compare_determ",
    "to_compare_short", "pickable"
  ), \(x) {
    if (length(get(x)) != length(all_regions)) {
      stop("length of`", x, "` is not the same as the length of `all_regions`")
    }
  }))

  region <- region[order(match(region, names(all_regions)))]
  name <- name[order(match(names(name), names(all_regions)))]
  to_compare <- to_compare[order(match(names(to_compare), names(all_regions)))]
  to_compare_determ <- to_compare_determ[order(match(names(to_compare_determ), names(all_regions)))]
  to_compare_short <- to_compare_short[order(match(names(to_compare_short), names(all_regions)))]
  pickable <- pickable[order(match(names(pickable), names(all_regions)))]


  tibble::tibble(
    region = region,
    priority = seq_len(length(region)),
    name = name,
    to_compare = to_compare,
    to_compare_determ = to_compare_determ,
    to_compare_short = to_compare_short,
    pickable = pickable
  )
}


#' Add `scales` column to the regions_dictionary
#'
#' @param regions_dictionary <`data.frame`> The regions dictionary built using
#' \code{\link[cc.buildr]{regions_dictionary}}. Will be used to filter out scales
#' for which data should not be calculated.
#' @param region_dict_scales <`named list`> Named list of the IDs of all scales
#' that fit within every region. One of the output of \code{\link[cc.buildr]{scales_consolidated}}
#'
#' @return The same `regions_dictionary` fed with the `scales` column added.
#' @export
regions_dictionary_add_scales <- function(regions_dictionary, region_dict_scales) {

  regions_dictionary$scales <- lapply(1:nrow(regions_dictionary), c)
  for (i in seq_along(region_dict_scales)) {
    regions_dictionary$scales[[i]] <- region_dict_scales[[regions_dictionary$region[[i]]]]
  }

  return(regions_dictionary)
}
