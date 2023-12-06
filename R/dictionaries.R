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


#' Build census scales dictionary
#'
#' @param census_scales <`list`> A named list containing census scales built
#' with the \code{\link[cc.buildr]{build_census_scales}} function.
#'
#' @return A data.frame used as dictionary for texts on the platform.
#' @export
census_scales_dictionary <- function(census_scales) {
  dict <-
    tibble::tibble(
      scale = c("CSD", "CT", "DA", "DB"),
      sing = c(
        "city",
        "census tract",
        "dissemination area",
        "dissemination block"
      ),
      sing_with_article = c(
        "the city",
        "the census tract",
        "the dissemination area",
        "the dissemination block"
      ),
      plur = c(
        "cities",
        "census tracts",
        "dissemination areas",
        "dissemination blocks"
      ),
      slider_title = c(
        "City",
        "Census tract",
        "Dissemination area",
        "Dissemination block"
      ),
      place_heading = c(
        "{name_2} of {name}",
        "Census tract {name} ({name_2})",
        "Dissemination area {name} ({name_2})",
        "Dissemination block {name} ({name_2})"
      ),
      place_name = c(
        "{name}",
        "Census tract {name}",
        "Dissemination area {name}",
        "Dissemination block {name}"
      ),
      subtext = c(
        "Census units (Census subdivisions)",
        "Census units",
        "Census units",
        "Census units",
      )
    )

  dict[dict$scale %in% names(census_scales), ]
}


#' Add regions to scales dictionary
#'
#' This function adds region information to a given scales dictionary based on
#' spatial features and known regions.
#'
#' @param scales_dictionary <`data.frame`> The scales dictionary built using
#' \code{\link{build_census_scales}}.
#' @param regions <`named list`> A list containing unioned regions, output of
#' \code{\link{create_master_polygon}}$regions.
#' @param scales_consolidated <`named list`> A named list of sf data.frame
#' containing all scales listed with their regions. The output of
#' \code{\link[cc.buildr]{consolidate_scales}}.
#' @param known_regions <`named list`> A named list of scales we already know
#' exactly which regions they should be assigned, useful for smaller scales requiring
#' a lot of computational power. e.g. `list(grid250 = c("island", "city"),  grid100 = c("island", "city")`.
#' Defaults to NULL.
#' @param DA_carto <`sf data.frame`> The cartographic version of DAs, one of the
#' output of \code{\link{create_master_polygon}}.
#'
#' @return A modified scales_dictionary with region information added.
#' @export
add_regions_to_scales_dictionary <- function(scales_dictionary, regions,
                                             scales_consolidated,
                                             known_regions = NULL, DA_carto) {

  # Digital to cartographic
  regions <- lapply(regions, \(reg) {
    # Transform to the specified CRS
    reg <- sf::st_transform(reg, crs = crs)
    DA_carto <- sf::st_transform(DA_carto, crs = crs)

    # Find the intersection of 'scale' and 'DA_carto'
    cartographic <- sf::st_intersection(reg, DA_carto)
    cartographic <- sf::st_cast(cartographic, "MULTIPOLYGON")

    return(cartographic)
  })

  # Grab region area
  regions_area <- sapply(regions, sf::st_area)

  # For every scales, extract the matching region
  scales_to_match <-
    scales_consolidated$scales[!names(scales_consolidated$scales) %in% names(known_regions)]
  scales_to_match <-
    scales_to_match[!names(scales_to_match) %in% c("street", "building")]

  avail_regions <- sapply(scales_to_match, \(scale_df) {

    # Union all the scale features
    scale_df_unioned <- sf::st_union(scale_df)
    scale_df_unioned <- sf::st_make_valid(scale_df_unioned)
    scale_df_unioned <- sf::st_transform(scale_df_unioned, crs = crs)

    area_after_intersection <- sapply(regions, \(reg) {
      intersected <- sf::st_intersection(reg, scale_df_unioned)
      intersected <- sf::st_make_valid(intersected)
      area <- sum(sf::st_area(intersected) |> as.vector())

      area
    })

    # If more than 95% of the area of the region is covered by the scale features,
    # count the region as valid for the scale.
    ind_regions <- area_after_intersection / regions_area > 0.95

    names(ind_regions)[ind_regions]
  }, simplify = FALSE, USE.NAMES = TRUE)

  no_region <- which(lengths(avail_regions) == 0)
  if (length(no_region) > 0) {
    stop(sprintf("The following scales have no matching region: %s",
                 paste0(names(avail_regions)[no_region], collapse = ", ")))
  }

  # Arrange the new regions column
  if (!is.null(known_regions)) avail_regions <- c(avail_regions, known_regions)
  scales_dictionary$regions <- sapply(scales_dictionary$scale, \(scale) avail_regions[[scale]])

  return(scales_dictionary)
}
