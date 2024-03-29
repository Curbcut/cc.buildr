% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid.R
\name{new_grid}
\alias{new_grid}
\title{Create a New Spatial Grid with Reverse Geocoding and Append DA IDs}
\usage{
new_grid(original_grid, cellsize, DA_table, crs)
}
\arguments{
\item{original_grid}{<\verb{sf object}> The original spatial grid in 'sf' format.}

\item{cellsize}{<\code{numeric}> The desired cell size for the new grid.}

\item{DA_table}{<\code{data.frame}> Data frame containing Dissemination Area IDs.}

\item{crs}{<\code{integer}> Coordinate Reference System code for spatial transformation.}
}
\value{
An 'sf' object representing the new grid with additional columns for
grid ID, place names, and DA IDs.
}
\description{
This function takes an original spatial grid and transforms it into a new grid
with specified cell size. It performs reverse geocoding on each grid cell
and appends Dissemination Area (DA) IDs.
}
