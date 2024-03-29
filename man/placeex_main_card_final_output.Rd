% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/place_explorer.R
\name{placeex_main_card_final_output}
\alias{placeex_main_card_final_output}
\title{Final function for the place explorer main card}
\usage{
placeex_main_card_final_output(
  region,
  scale,
  scale_df,
  select_id,
  lang = "en",
  pe_main_card_data,
  scales_dictionary,
  regions_dictionary
)
}
\arguments{
\item{region}{<\code{character}> Region under study, e.g. \code{CMA}.}

\item{scale}{<\code{character}> Scale under study, e.g. \code{DA}.}

\item{select_id}{<\code{character}> Selected identifier for the selected combinasion
of \code{region} and \code{df}.}

\item{lang}{<\code{character}> Language that should be used to produce the main card
output. There need to be a function \code{placeex_main_card_prep_output_x} available
in that language. \code{en} or \code{fr}.}

\item{pe_main_card_data}{<\code{list}> Place explorer data for the maincard. Usually
the output of \code{\link[cc.buildr]{placeex_main_card_data}} saved in the
data folder.}

\item{scales_dictionary}{<\code{data.frame}> The scales dictionary built using
\code{\link[cc.buildr]{build_census_scales}}}

\item{regions_dictionary}{<\code{data.frame}> The regions dictionary built using
\code{\link[cc.buildr]{regions_dictionary}}. Will be used to filter out scales
for which data should not be calculated.}
}
\value{
Returns a list of all the main card variables and how the selected
id compares in its dataset.
}
\description{
Final function for the place explorer main card
}
