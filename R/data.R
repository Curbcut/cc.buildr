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

#' 2016 Dissemination Areas Can-ALE Index
#'
#' Canadian Active Living Environments Database.
#'
#' @format ## `canale_data`
#' A data frame of 2 columns and the same number of rows as there are DAs in
#' the 2016 Canadian Census.
#' \describe{
#'   \item{DA_ID}{The ID of each dissemination area}
#'   \item{canale_2016}{The 2016 Can-ALE index}
#' }
#' @source <http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf>
"canale_data"

#' 2021 Dissemination Areas Can-BICS Metric
#'
#' Neighborhood Canadian Bikeway Comfort and Safety Classification System
#' Database.
#'
#' @format ## `canbics_data`
#' A data frame of 2 columns and the same number of rows as there are DAs in
#' the 2016 Canadian Census.
#' \describe{
#'   \item{DA_ID}{The ID of each dissemination area}
#'   \item{canbics_2021}{The 2021 Can-BICS metric}
#' }
#' @source <https://canue.ca/wp-content/uploads/2022/04/CAN-BICS_UserGuide_format.docx>
"canbics_data"

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

#' Current census code, used to retrieve from cancensus
#'
#' @format ## `current_census`
#' A character vector of the most up-to-date available census from cancensus.
"current_census"
