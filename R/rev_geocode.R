#' Reverse geocode for a point in Quebec
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#'
#' @return The address retrieved from the Adresses Québec service
#' @export
rev_geocode_QC <- function(point_sf) {
  coords <-
    point_sf |>
    sf::st_transform(crs = 32198) |>
    sf::st_coordinates()

  x <- coords[1]
  y <- coords[2]

  link <-
    paste0(
      "https://servicescarto.mern.gouv.qc.ca/pes/rest/services/Territoi",
      "re/AdressesQuebec_Geocodage/GeocodeServer/reverseGeocode?location=",
      x, "%2C+", y, "&distance=&langCode=&outSR=4326&returnIntersection=false&f",
      "=pjson"
    )

  out <- jsonlite::fromJSON(httr::content(httr::GET(link)))

  if (!is.null(out$error)) {
    return(NA_character_)
  }
  return(paste(out$address$Street, out$address$City, sep = ", "))
}

#' Reverse geocode for points in British Columbia
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#'
#' @return The address retrieved from the BC Geocoder service
#' @export
rev_geocode_BC <- function(point_sf) {
  coords <-
    point_sf |>
    sf::st_transform(crs = 4326) |>
    sf::st_coordinates()

  x <- coords[1]
  y <- coords[2]

  link <-
    paste0(
      "https://geocoder.api.gov.bc.ca/sites/nearest.json?point=",
      x, ",", y
    )

  out <- httr::content(httr::GET(link))

  if (is.null(out$properties$fullAddress)) {
    return(NA_character_)
  }
  return(gsub(", BC$", "", out$properties$fullAddress))
}

#' Reverse geocode using OSM
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#'
#' @return The address retrieved from the OSM service
#' @export
rev_geocode_OSM <- function(point_sf) {
  coords <-
    point_sf |>
    sf::st_transform(crs = 4326) |>
    sf::st_coordinates()

  x <- coords[1]
  y <- coords[2]

  link <- paste0(
    "https://nominatim.openstreetmap.org/reverse?format=json&lat=",
    y, "&lon=", x, "&addressdetails=1"
  )

  out <- httr::content(httr::GET(link))

  if (is.null(out$address)) {
    return(NA_character_)
  }

  # Third level of the address, after street number and street name
  third <-
    out |> (\(out) {
      if (!is.null(out$address$city)) {
        return(gsub(" \\(\\d{2}\\)$", "", out$address$city))
      }
      if (!is.null(out$address$town)) {
        return(out$address$town)
      }
      if (!is.null(out$address$village)) {
        return(out$address$village)
      }
      if (!is.null(out$address$suburb)) {
        return(out$address$suburb)
      }
      if (!is.null(out$address$region)) {
        return(out$address$region)
      }
    })()

  second <-
    out |> (\(out) {
      if (is.null(out$address$road)) {
        return(third)
      }
      return(paste(out$address$road, third, sep = ", "))
    })()

  name <-
    out |> (\(out) {
      if (is.null(out$address$house_number)) {
        return(second)
      }
      return(paste(out$address$house_number, second, sep = " "))
    })()

  return(name)
}
