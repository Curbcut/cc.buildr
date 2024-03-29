% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/digital_to_cartographic.R
\name{digital_to_cartographic}
\alias{digital_to_cartographic}
\title{Convert digital geographic data to cartographic representation}
\usage{
digital_to_cartographic(scale, DA_carto, crs)
}
\arguments{
\item{scale}{<\verb{sf object}> An sf object representing the digital geographic
data to be converted.}

\item{DA_carto}{<\verb{sf object}> An sf object representing the cartographic
boundaries to intersect with the scale parameter.}

\item{crs}{<\code{character}> A character string specifying the coordinate
reference system to which the input data should be transformed.}
}
\value{
An sf object with the original digital data's geometry and the
converted cartographic data.
}
\description{
This function transforms digital geographic data into a cartographic
format suitable for mapping and spatial analysis, ensuring that both
input datasets are in the same coordinate reference system before
performing spatial operations.
}
