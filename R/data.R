#' 2016 Population-weighted Dissemination Area Centroids
#'
#' The centroids of all dissemination areas, weighted by population, in the
#' country.
#'
#' @format ## `da_centroids_popw`
#' An sf data frame with 7,240 56590 and 2 columns:
#' \describe{
#'   \item{ID}{The ID of each dissemination area}
#'   \item{geometry}{sfc_POINT representing the population-weighted centroid
#'   of each dissemination area}
#' }
"da_centroids_popw"

#' Current census code, used to retrieve from cancensus
#'
#' @format ## `current_census`
#' A character vector of the most up-to-date available census from cancensus.
"current_census"

#' Province's geometries and pbf download link file
#'
#' @format ## `provinces_pbf`
#' An sf data frame with 10 rows and 3 columns:
#' \describe{
#'   \item{name}{The province's name}
#'   \item{link}{Link from where to download the pbf file}
#'   \item{geometry}{MULTIPOLYGON of the province}
#' }
"provinces_pbf"

#' Provinces links to the National Open Database of Addresses
#'
#' @format ## `addresses_db_links`
#' A data frame of 3 columns and the same number of rows as there are .zip of
#' addresses available
#' \describe{
#'   \item{province_code}{The two character code of the Province}
#'   \item{province}{The province name}
#'   \item{link}{The download link to get the province's databse of addresses}
#' }
#' @source <https://www.statcan.gc.ca/en/lode/databases/oda>
"addresses_db_links"
