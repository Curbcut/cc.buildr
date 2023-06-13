#' Build the regions dictionary
#'
#' @param all_tables <`named list`> Named list of regions and their scales within,
#' ordered in priority.
#' @param region <`vector of character`> A vector of characters used to identify
#' which large geometry the user is interested in, e.g.
#' \code{c("CMA", "city", "island")}.
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
regions_dictionary <- function(all_tables, region, name, to_compare,
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
    if (length(get(x)) != length(all_tables)) {
      stop("length of`", x, "` is not the same as the length of `all_tables`")
    }
  }))

  region <- region[order(match(region, names(all_tables)))]
  name <- name[order(match(names(name), names(all_tables)))]
  to_compare <- to_compare[order(match(names(to_compare), names(all_tables)))]
  to_compare_determ <- to_compare_determ[order(match(names(to_compare_determ), names(all_tables)))]
  to_compare_short <- to_compare_short[order(match(names(to_compare_short), names(all_tables)))]
  pickable <- pickable[order(match(names(pickable), names(all_tables)))]


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
