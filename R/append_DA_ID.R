#' Append a DA ID to a data frame
#'
#' This function takes a spatial data frame of DAs , a data
#' frame with spatial information, and a coordinate reference system (CRS) for both
#' layers. The function then computes the DA ID of each spatial feature in
#' the data frame by checking if its centroid falls within a DA polygon. The DA
#' ID is appended to the data frame as a new column and the modified data frame is returned.
#'
#' @param DA_table <`sf dataframe`> A spatial data frame representing a DA layer. The data
#' frame must contain an ID column that uniquely identifies each DA.
#' @param df <`sf dataframe`> A spatial data frame with the spatial features for
#' which the district ID needs to be computed. The data frame must contain at
#' least one spatial column (geometry column).
#' @param crs <`character`> The coordinate reference system of both the district
#' layer and the data frame. This can be either a proj4 string or an epsg code.
#'
#' @return The data frame \code{df} with a new column called \code{DA_ID} that
#' contains the DA ID for each spatial feature in the data frame.
#' @export
append_DA_ID <- function(DA_table, df, crs) {
  das <- sf::st_transform(DA_table, crs = crs)
  df <- sf::st_transform(df, crs = crs)

  df_centroid <- suppressWarnings(sf::st_point_on_surface(df))
  inters <- sf::st_intersects(df_centroid, das)

  ids <- sapply(inters, function(x) das$ID[x])
  df$DA_ID <- sapply(ids, \(x) if (length(x) == 0) NA else x)

  return(df)
}
