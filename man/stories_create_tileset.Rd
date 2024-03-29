% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tilesets.R
\name{stories_create_tileset}
\alias{stories_create_tileset}
\title{Create a tileset from a stories dataframe}
\usage{
stories_create_tileset(stories, inst_prefix, username, access_token)
}
\arguments{
\item{stories}{<\code{dataframe}>A dataframe with columns "ID", "name_id", "lon",
"lat".}

\item{inst_prefix}{<\code{character}> Prefix attached to every tile source and
created and published tileset. Should correspond to the Curbcut city, e.g. \code{mtl}.}

\item{username}{<\code{character}> Mapbox account username.}

\item{access_token}{<\code{character}> Private access token to the Mapbox account.}
}
\value{
Returns nothing if succeeds. Tilesets are created and published and
ready to be used.
}
\description{
Create a tileset from a stories dataframe
}
