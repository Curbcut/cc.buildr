% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/additional_scale.R
\name{additional_scale}
\alias{additional_scale}
\title{Add a scale}
\usage{
additional_scale(
  additional_table,
  DB_table,
  ID_prefix,
  name_2,
  crs,
  DA_carto,
  switch_to_carto = TRUE
)
}
\arguments{
\item{additional_table}{<\verb{sf data.frame}> An sf data.frame of two or three columns:
\code{name} and \code{geometry} or \code{ID}, \code{name} and \code{geometry}.
Ideally, the name of each zone should be unique.}

\item{DB_table}{<\verb{sf data.frame}> A \code{DB} sf data.frame from which
population and households will be interpolated.}

\item{ID_prefix}{<\code{character}> A character of length 1. In order to keep
the identifiers unique on the platform, the prefix used before the
\code{seq_along()} that will be called to create unique identifiers.
e.g. \code{"ward"}. If there is already an ID in the \code{additional_table},
the ID_prefix will be appended in front of it.}

\item{name_2}{<\code{character}> A character of length 1. \code{name_2} will be
used to define the zone along with the \code{name}. The display on Sus would
be e.g. \verb{\{name_2\} of \{name\}}, or \verb{Ward of Davenport}.}

\item{crs}{<\code{numeric}> EPSG coordinate reference system to be assigned, e.g.
\code{32617} for Toronto.}

\item{DA_carto}{<\verb{sf data.frame}> The cartographic version of DAs, one of the
output of \code{\link{create_master_polygon}}.}

\item{switch_to_carto}{<\code{logical}> Should the geometry be switched to cartographic?
Unnecessary for scales that are already cartographic.}
}
\value{
An sf data.frame with population and households interpolated from
the \code{DB_table}, containing all columns necessary to import on the
platform.
}
\description{
Using \code{additional_scale()} is useful both for interpolating population and
households using dissemination areas, but also for ensuring that the new
scale is uniform, containing all the columns expected in an added scale.
}
