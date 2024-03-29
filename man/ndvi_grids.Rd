% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndvi_grids.R
\name{ndvi_grids}
\alias{ndvi_grids}
\title{Generate NDVI grids}
\usage{
ndvi_grids(
  census_scales,
  base_polygons,
  output_path = "dev/data/ndvi/",
  save_grids_dir = "dev/data/built/",
  overwrite_ndvi_tiles = FALSE,
  overwrite_final_grids = FALSE,
  crs = crs,
  grid_sizes = c(30, 60, 120, 300, 600)
)
}
\arguments{
\item{census_scales}{<\code{data.frame}> Data frame containing census scales.}

\item{base_polygons}{<\verb{sf object}> Simple Features (sf) object representing
base polygons. The output of \code{create_master_polygon}.}

\item{output_path}{<\code{character}> Path to the directory for output files.
Default is \code{"dev/data/ndvi/"}.}

\item{save_grids_dir}{<\code{character}> Directory path for saving processed grids.
Default is \code{"dev/data/built/"}.}

\item{overwrite_ndvi_tiles}{<\code{logical}> Flag to determine if existing NDVI
tiles should be overwritten. Default is \code{FALSE}.}

\item{overwrite_final_grids}{<\code{logical}> Flag to determine if existing final
grids should be overwritten. Default is \code{FALSE}.}

\item{crs}{<\code{numeric}> EPSG}

\item{grid_sizes}{<\verb{character vector}> What should the size of the grid cells?
Defaults to \code{c(30, 60, 120, 300, 600)}. Must multiples of 30.}
}
\value{
<\code{list}> Returns a list of processed grid data at various cell sizes.
}
\description{
This function generates NDVI (Normalized Difference Vegetation Index) grids
based on given census scales and base polygons. It manages the import of NDVI
tiles, processing of grids, and saving of final grid data. The function allows
control over the output path, temporary directories, and overwrite options.
}
