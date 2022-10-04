#' Build the geos dictionary
#'
#' @param geo <`vector of character`> A vector of characters used to identify
#' which large geometry the user is interested in, e.g.
#' \code{c("CMA", "city", "island")}.
#' @param name <`vector of character`> A vector of character the same length as
#' the vector fed in the `geo` argument. Used to name the geo when the user will
#' want to change which large geometry they are interested in. e.g.
#' \code{c("Metropolitan Area", "City of Montreal", "Island of Montreal")}.
#' @param to_compare <`vector of character`> A vector of character the same length as
#' the vector fed in the `geo` argument. Used at the end of dynamically generated
#' text, when a particular zone is compared with the whole geography. 'x DA
#' has a higher value than y% of DAs in the Montreal region'.
#' e.g. \code{c("in the Montreal region", "in the City of Montreal",
#' "on the island of Montreal")}
#'
#' @return Returns the same vectors fed arranged in a data.frame.
#' @export
geos_dictionary <- function(geo, name, to_compare) {
  data.frame(
    geo = geo,
    name = name,
    to_compare = to_compare
  )
}
