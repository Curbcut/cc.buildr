#' Build census scales
#'
#' `build_census_scales` retrieves all the fed census levels
#' present in the master_polygon. It can split a CSD in multiple smaller zones.
#' The \code{split_CSD} fed in the function then sums population and households for
#' each of its zones from the census tracts, and the geometry of the zones are
#' swaped by an aggregation of census tracts for which centroids fall in each zones,
#' so that borders of the split zones fit perfectly with the rest of the CSD.
#' Boundaries of both the zones and the overall split_CSD dataframe MUST fit
#' with census tract boundaries.
#'
#'
#' @param master_polygon <`sfc_MULTIPOLYGON`>. Unioned multipolygon of all the
#' geometries for which census data must be gathered.
#' @param census_dataset <`character`> The dataset to query for available
#' regions, e.g. \code{"CA16"}.
#' @param regions <`named list`> A named list of census regions to retrieve.
#' Names must be valid census aggregation levels. Preferably a whole province
#' to make sure all geometries present in the <`master_polygon`> is added.
#' e.g. \code{list(PR = 24)} for Montreal.
#' @param levels <`character`> The census aggregation levels to retrieve.
#' Can be \code{"CMA"}, \code{"CD"}, \code{"CT"}, \code{"DA"}, \code{"DB"}, ...
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#' @param fill_CTs_with_CSDs <`logical`> If the master polygon is bigger than a
#' CMA, should what is outside the CMA display the CSDs on the CT scale?
#' @param split_CSD <`sf data.frame`> An sf dataframe of subdivisions of one
#' CSD. e.g. for the City of Montreal, the subdivisions is a dataframe of boroughs.
#' The sf data.frame must have 3 columns: \code{name}, \code{type} and \code{geometry}.
#' The \code{name} is the name of the borough/neighbourhood/zone e.g.
#' "Le Plateau Mont-Royal", and \code{type} is what should be used as \code{"name_2"},
#' e.g. "Borough". The data.frame must fit with CTs boundaries.
#' @param default_name_2_for_CSD <`character`> The default name_2 for CSDs. Default
#' to "City". The display on Sus would then be e.g. `City of Laval`.
#'
#' @return A list of sf dataframes of census scales filtered by the master polygon,
#' with the option of one CSD subdivided.
#' @export
build_census_scales <- function(master_polygon, census_dataset, regions,
                                levels = c("CSD", "CT", "DA"), crs,
                                fill_CTs_with_CSDs = TRUE, split_CSD,
                                default_name_2_for_CSD = "City") {

  # Get census data with the help of susbuilder::get_census_sus()
  census_datasets <-
    sapply(levels, \(x) {
      get_census_sus(master_polygon = master_polygon,
                     census_dataset = census_dataset,
                     regions = regions,
                     level = x,
                     crs = crs)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Rename `name` so they don't include the parenthesis
  census_datasets <-
    lapply(census_datasets, \(x) {
      x$name <- gsub(" \\(.*\\)$", "", x$name)
      x
    })

  # If the master polygon is bigger than a CMA, then there are gaps that need
  # to be fill with CSDs.
  if (fill_CTs_with_CSDs) {

    csds <- sf::st_transform(census_datasets$CSD, crs)
    csds <- suppressWarnings(sf::st_point_on_surface(csds))
    cts <- sf::st_transform(census_datasets$CT, crs)
    csds_in_cts <- sf::st_filter(csds, cts)$ID

    csds_to_bind <-
      census_datasets$CSD[!census_datasets$CSD$ID %in% csds_in_cts, ]

    if (nrow(csds_to_bind) != 0) {

      csds_to_bind$CSDUID <- csds_to_bind$ID
      csds_to_bind$ID <- paste0("CSD_", csds_to_bind$ID)

      census_datasets$CT <- rbind(census_datasets$CT, csds_to_bind)

    }

    # Update CTUID for the rest of the levels
    change_CTUID_for <-
    census_datasets[{which(names(census_datasets) == "CT") + 1}:length(census_datasets)]

    updated_rest_levels <-
      lapply(change_CTUID_for, \(x) {

        x <- sf::st_transform(x, crs)
        cts <- sf::st_transform(census_datasets$CT, crs)
        x_centroids <- suppressWarnings(
          sf::st_centroid(x, of_largest_polygon = TRUE)[, "ID"])

        CSDUID_key <-
          sf::st_drop_geometry(sf::st_join(x_centroids, cts[, "ID"]))
        names(CSDUID_key) <- c("ID", "CTUID")


        x <- merge(x[, names(x) != "CTUID"], CSDUID_key, by = "ID")
        x <- x[, c("ID", "name", "CSDUID", "CTUID", names(x)[
          !names(x) %in% c("ID", "name", "CSDUID", "CTUID")])]

        sf::st_transform(x, 4326)

      })

    census_datasets <-
      c(census_datasets[!names(census_datasets) %in% names(change_CTUID_for)],
        updated_rest_levels)

  }

  # If a CSD must be split into multiple smaller zones
  if (!missing(split_CSD)) {

    # Which CSD is taken out
    to_split_with <- sf::st_transform(split_CSD, crs)
    csds <- sf::st_transform(census_datasets$CSD, crs)
    csds_centroids <- suppressWarnings(sf::st_point_on_surface(csds))
    to_replace <-
      suppressWarnings(sf::st_intersection(csds_centroids, to_split_with)$ID)

    # Take out the CSD to replace
    csds <- csds[!csds$ID %in% to_replace, ]

    # Join CTs to remaining new geometries by centroid
    cts <- sf::st_transform(census_datasets$CT, crs)
    # Get centroid on POLYGONS instead of MULTIPOLYGON, to be sure to get a match
    cts_centroids <- suppressWarnings(
      sf::st_point_on_surface(cts))[, c("ID", "geometry")]
    to_split_with_join <-
      sf::st_join(cts_centroids, to_split_with, left = FALSE)[, c("ID", "name")] |>
      sf::st_drop_geometry() |>
      unique()

    # Redo data processing for CTs in to_split_with_join
    light_cts <- sf::st_drop_geometry(cts[, names(cts)[names(cts) != "name"]])
    cts_from_split <- merge(light_cts, to_split_with_join, by = "ID", all = FALSE)

    population <- stats::aggregate(cts_from_split$population,
                            by = list(name = cts_from_split$name), FUN = sum)
    names(population) <- c("name", "population")
    households <- stats::aggregate(cts_from_split$households,
                            by = list(name = cts_from_split$name), FUN = sum)
    names(households) <- c("name", "households")

    to_split_with <- merge(to_split_with, population, by = "name")
    to_split_with <- merge(to_split_with, households, by = "name")

    names(to_split_with)[names(to_split_with) == "type"] <- "name_2"
    to_split_with$ID <- paste(to_replace, seq_along(to_split_with$name),
                              sep = "_")

    # Get MULTIPOLYGONS out of the CTs for the new geometries
    cts_for_split <- merge(to_split_with_join, cts[, "ID"], by = "ID")
    new_to_split_geo <-
      Reduce(rbind,
             lapply(unique(cts_for_split$name), \(x) {
               geo <- cts_for_split[cts_for_split$name == x, ] |>
                 sf::st_as_sf() |>
                 sf::st_union()

               data.frame(name = x,
                          geometry = geo)
             })) |> sf::st_as_sf()

    to_split_with <-
      merge(sf::st_drop_geometry(to_split_with), new_to_split_geo[, "name"],
            by = "name") |>
      sf::st_as_sf() |>
      sf::st_cast("MULTIPOLYGON")

    # Update the CSD dataframe in our census_datasets
    csds$name_2 <- default_name_2_for_CSD
    bind <- rbind(csds, to_split_with)
    bind <- bind[, c("ID", "name", "name_2", "population", "households",
                     "geometry")]
    census_datasets$CSD <- sf::st_transform(bind, 4326)

    # Update CSDUID for the rest of the levels
    updated_rest_levels <-
      lapply(census_datasets[names(census_datasets) != "CSD"], \(x) {

        x <- sf::st_transform(x, crs)
        csds <- sf::st_transform(census_datasets$CSD, crs)
        x_centroids <- suppressWarnings(
          sf::st_centroid(x, of_largest_polygon = TRUE)[, "ID"])

        CSDUID_key <-
          sf::st_drop_geometry(sf::st_join(x_centroids, csds[, "ID"]))
        names(CSDUID_key) <- c("ID", "CSDUID")

        x <- merge(x[, names(x) != "CSDUID"], CSDUID_key, by = "ID")
        x <- x[, c("ID", "name", "CSDUID", names(x)[
          !names(x) %in% c("ID", "name", "CSDUID")])]

        sf::st_transform(x, 4326)

      })

    census_datasets <-
      c(census_datasets['CSD'], updated_rest_levels)

  }

  # Add name_2
  census_datasets <-
    mapply(\(x, y) {
      if ("name_2" %in% names(x)) return(x)
      if (y == "CSD") {
        x$name_2 <- default_name_2_for_CSD
      } else {
        csds <- sf::st_drop_geometry(census_datasets$CSD[, c("ID", "name")])
        names(csds) <- c("CSDUID", "name_2")
        x <- merge(x, csds, by = "CSDUID")
      }

      x[, c("ID", "name", "name_2", names(x)[
        !names(x) %in% c("ID", "name", "name_2")])]
    }, census_datasets, names(census_datasets))

  census_datasets

}
