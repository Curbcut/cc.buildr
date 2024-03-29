% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dyk.R
\name{dyk_uni_change}
\alias{dyk_uni_change}
\title{Generate Change-over-time DYKs}
\usage{
dyk_uni_change(var_left, region, scale, date, svm, langs)
}
\arguments{
\item{var_left}{\if{html}{\out{<character>}} A string representing the name of the variable
for which the DYK should be calculated.}

\item{region}{<\code{character}> A string representing the name of the region
for which the DYK should be calculated.}

\item{scale}{<\code{character}> A string representing the name of the scale
for which the DYK should be calculated.}

\item{date}{<\verb{list of character vector}> Reprenseting the dates for which
the DYK should be calculated2}

\item{svm}{<\code{list}> A list, usually \code{scales_variables_modules}, containing
the scales, modules, and variables tables.}
}
\value{
A data frame with two columns (\code{change_text} and \code{change_val}),
which contain a character vector of DYK outputs and a numeric vector of the
values respectively.
}
\description{
This function creates "Did you know" text strings highlighting the change
over time from a combination of variable, region and scale. The output is a
data frame containing a \code{change_text} and \code{change_val} column.
}
