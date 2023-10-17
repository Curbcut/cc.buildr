#' Build census data at all possible scales
#'
#' @param scales_consolidated A named list of sf data.frame
#' containing all scales listed with their regions. The output of
#' \code{\link[cc.buildr]{consolidate_scales}}.
#' @param region_DA_IDs <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param census_vectors <`character vector`> Data variables that should be added
#' to the scales. By default, all: \code{\link[cc.data]{census_vectors}}. Look
#' at the \code{\link[cc.data]{census_vectors_table}} to view all
#' variables explained.
#' @param census_years <`character vector`> Years for which the census data
#' should be added to the scales. Defaults to \code{\link[cc.data]{census_years}}
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#' @param census_scales <`character vector`> Vector of all the census scales for
#' which we have data in the database.
#' @param skip_scale_interpolation <`character vector`> Scales for which census
#' data should not be interpolated (e.g. very small scales like 25m grid cells.).
#' In those cases, census data won't be interpolated and appended. Defaults to
#' NULL to interpolate to everything.
#'
#' @return Returns a list of length 4. The first is the possible scales for which
#' census data can be added in scales_consolidated. The second is a
#' data.frame of scales reference, to know for which scales the data is
#' available. It can directly be fed to the scales argument of add_variable.
#' The third is a data.frame of interpolation reference, to know for which
#' scales the data has been interpolated. It can directly be fed to the
#' interpolated argument of add_variable. The fourth is a character
#' vector of all regions at which the data will be available.
#' @export
build_census_data <- function(scales_consolidated, region_DA_IDs,
                              census_vectors, census_years, crs,
                              census_scales = cc.data::census_scales,
                              skip_scale_interpolation = NULL) {

  # Filter out scales smaller than DAs --------------------------------------

  higher_DA <- scales_greater_than(base_scale = scales_consolidated$DA,
                                   all_scales = scales_consolidated,
                                   crs = crs)
  higher_DA <- scales_consolidated[names(scales_consolidated) %in% higher_DA]


  # Interpolate for scales that are not census scales -----------------------

  non_census <- higher_DA[!names(higher_DA) %in% census_scales]

  # If there is nothing to fill, assign nothing
  non_census <- if (length(non_census) > 0) {
    # Get the values
    vals <- cc.data::census_custom_boundaries(
      destination = non_census,
      DA_IDs = region_DA_IDs,
      census_vectors = census_vectors,
      census_years = census_years,
      crs = crs
    )

    # Merge the values to scales_consolidated
    mapply(\(scale_name, scale_df) {
      merge(scale_df, sf::st_drop_geometry(vals[[scale_name]]), by = "ID")
    }, names(non_census), non_census, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  } else {
    list()
  }


  # Fill in data for census scales ------------------------------------------

  census_scales_df <- higher_DA[names(higher_DA) %in% census_scales]

  census_scales_df <- mapply(\(scale_name, scale_df) {
    all_vectors <- cc.data::census_add_parent_vectors(census_vectors)
    vals <- cc.data::db_read_data(
      table = paste0("processed_", scale_name),
      columns = sapply(
        census_years,
        \(x) paste(all_vectors, x, sep = "_")
      ),
      IDs = scale_df$ID,
      crs = crs,
      keep_geometry = FALSE
    )
    merge(scale_df, vals, by = "ID")
  }, names(census_scales_df), census_scales_df, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  final_scales <- c(census_scales_df, non_census)

  # Re-attach missing scales
  missing_scales <- scales_consolidated[!names(scales_consolidated) %in% names(final_scales)]
  final_scales <- c(final_scales, missing_scales)


  # Keep track of scales for which census data is available ------------------

  avail_scale <- names(final_scales)


  # Create interpolated references as a data.frame --------------------------

  interpolated_ref_census <- tibble::tibble(
    scale = names(census_scales_df),
    interpolated_from = rep(FALSE, length(census_scales_df))
  )
  interpolated_ref_non_census <- tibble::tibble(
    scale = names(non_census),
    interpolated_from = rep("DA", length(non_census))
  )
  interpolated_ref <- rbind(interpolated_ref_census, interpolated_ref_non_census)


  # Get variable types ------------------------------------------------------

  vecs <- cc.data::census_add_parent_vectors(census_vectors)

  types <- sapply(vecs, \(var) {
    cc.data::census_vectors_table$type[
      cc.data::census_vectors_table$var_code == var
    ]
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Return data with available scales and interpolated reference ------------

  return(list(
    scales = final_scales,
    avail_scale = avail_scale,
    interpolated_ref = interpolated_ref,
    types = types
  ))
}
