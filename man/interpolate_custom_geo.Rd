% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate.R
\name{interpolate_custom_geo}
\alias{interpolate_custom_geo}
\title{Interpolate variables from any geometry to all scales bigger in area}
\usage{
interpolate_custom_geo(
  data,
  all_scales,
  crs,
  only_scales = names(all_scales),
  average_vars = c(),
  additive_vars = c(),
  name_interpolate_from,
  construct_for = NULL,
  overwrite = FALSE,
  time_regex = "_\\\\d{4}$",
  inst_prefix = inst_prefix
)
}
\arguments{
\item{data}{<\code{data.frame}> Containing any number of column with data,
and an ID that corresponds to the source scale, e.g. \code{"DA_ID"}.}

\item{all_scales}{<\verb{named list}> A named list of scales.}

\item{crs}{<\code{numeric}> EPSG coordinate reference system to be assigned, e.g.
\code{32618} for Montreal.}

\item{only_scales}{<\verb{character vector}> All the scales for which data should
be interpolated and appended. Defults to all the scales in \code{all_scales}.}

\item{average_vars}{<\verb{character vector}> Corresponds to the column names
of the variables that are to be interpolated as an average, like a percentage,
a median, an index, etc. weighted by the \code{weight_by} argument.}

\item{additive_vars}{<\verb{character vector}> Corresponds to the column names of
the variables that are 'count' variables. In the case of this function, the output
value of a CSD would be the sum of the values of the DAs or CTs that are present
inside the CSD.}

\item{name_interpolate_from}{<\code{character}> The name of the scale from which
data has been interpolated, e.g. \code{DA}. If it is an entry in the scales_dictionary,
the entry of the \code{plur} column will be used as text. If it isn't, the character
of \code{name_interpolate_from} will be used, e.g.
\verb{...has been spatially interpolated from green space polygons} where
\verb{green space polygons} would be the value of \code{name_interpolate_from}.}

\item{construct_for}{<\code{list}> A list where each region is a level, and
each of them has a vector character of scales for which the \code{data} should be
interpolated to. Used by \code{\link[cc.buildr]{interpolate_from_census_geo}}.
It bypasses the process where \code{interpolate_custom_geo} only interpolates
for geometries bigger than the data.}

\item{overwrite}{<\code{logical}> Should the data already processed and stored be
overwriten?}

\item{time_regex}{<\code{character}> Regular expression which corresponds to
a timeframe, placed at the end of the \code{vars} vector. e.g. \verb{\\\\d\{4\}} for
years.}

\item{inst_prefix}{<\code{character}> The prefix of the instance, e.g. \code{'mtl'} which
is the database schema in which the data is saved.}
}
\value{
Returns a list of length 4. The first is the same list that is fed in
\code{all_scales}, with the columns from \code{data} interpolated in. The second is
a data.frame of scales reference, to know for which scales the data is
available. It can directly be fed to the \code{scales}
argument of \code{\link[cc.buildr]{add_variable}}.The third
is a data.frame of interpolation reference, to know for which scales the data
has been interpolated. It can directly be fed to the \code{interpolated}
argument of \code{\link[cc.buildr]{add_variable}}. The fourth is a character
vector of all regions at which the data will be available.
}
\description{
Used to interpolate any polygons to all the used scale in the instance of
Curbcut. Data is interpolated to a particular scale if the average size
of the \code{data} polygons is smaller than the average size of any scale in
\code{all_scales}.
}
