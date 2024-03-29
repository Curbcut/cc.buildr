% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate.R
\name{interpolate_fast_weighted_mean}
\alias{interpolate_fast_weighted_mean}
\title{Fast Weighted Mean for Interpolation}
\usage{
interpolate_fast_weighted_mean(df, id_col, weight_col, value_col)
}
\arguments{
\item{df}{<\code{data.frame}> A data frame containing the data to be interpolated.
It must have columns corresponding to the specified id_col, weight_col, and
value_col.}

\item{id_col}{<\code{character}> A string representing the name of the ID column
in the input data frame.}

\item{weight_col}{<\code{character}> A string representing the name of the weight
column in the input data frame.}

\item{value_col}{<\code{character}> A string representing the name of the value
column in the input data frame.}
}
\value{
A data frame with two columns: the ID column (with the same name as
in the input data frame) and a column containing the weighted means, named
after the input value_col
}
\description{
This function computes the weighted mean for each group of IDs interpolated
within a data frame. The data frame must have columns for ID, weight, and
value. The output is a data frame containing the IDs and their corresponding
weighted means.
}
