% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_processing.R
\name{post_processing}
\alias{post_processing}
\title{Post process scales}
\usage{
post_processing(scales)
}
\arguments{
\item{scales}{<\verb{named list}> A list of all the scales ordered in regions.
Usually \code{scales_variables_modules$scales}}
}
\value{
A list containing the post-processed scales, variables and modules
}
\description{
This function casts the geometry of scales to the right type, and reorders
the columns so that IDs and names are first.
}
