% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ba_var.R
\name{ba_var}
\alias{ba_var}
\title{Build and append a new variable}
\usage{
ba_var(
  data,
  scales_variables_modules,
  base_scale,
  scales_sequences,
  weight_by = "households",
  crs,
  additive_vars = c(),
  average_vars = c(),
  time_regex = "_\\\\d{4}$",
  inst_prefix,
  variable_var_code,
  variable_classification,
  variable_type,
  variable_var_title,
  variable_var_short,
  variable_explanation,
  variable_exp_q5,
  variable_theme,
  variable_private,
  variable_source,
  variable_pe_include = TRUE,
  module_id = NULL,
  module_theme = NULL,
  module_nav_title = NULL,
  module_title_text_title = NULL,
  module_title_text_main = NULL,
  module_title_text_extra = NULL,
  module_metadata = NULL,
  module_dataset_info = NULL,
  module_dates = NULL,
  module_main_dropdown_title = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{data}{<\code{data.frame}> Containing any number of column of THE SAME data
(different years possible), and an ID that corresponds to the base scale,
e.g. \code{"DA_ID"}.}

\item{scales_variables_modules}{<\verb{named list}> A list of length three.
The first is all the scales, the second is the variables table, and the
third is the modules table.}

\item{base_scale}{<\code{character}> The denominator for which data should be
interpolated. Defaults to \code{households}. The other option is \code{population}.
Directly fed to \code{\link[cc.buildr]{interpolate_from_census_geo}}}

\item{scales_sequences}{<\code{list}> A list of scales sequences representing the
hierarchical ordering of scales on an auto-zoom.}

\item{weight_by}{<\code{character}> The denominator for which data should be
interpolated. Defaults to \code{households}. The other option is \code{population}.
Directly fed to \code{\link[cc.buildr]{interpolate_from_census_geo}}}

\item{crs}{<\code{numeric}> EPSG coordinate reference system to be assigned, e.g.
\code{32618} for Montreal.}

\item{additive_vars}{<\code{charater}> See \code{\link[cc.buildr]{interpolate_from_census_geo}}.}

\item{average_vars}{<\code{charater}> See \code{\link[cc.buildr]{interpolate_from_census_geo}}.}

\item{time_regex}{<\code{character}> Regular expression which corresponds to
a timeframe, placed at the end of the \code{vars} vector. e.g. \verb{\\\\d\{4\}} for
years.}

\item{inst_prefix}{<\code{character}> The prefix of the instance, e.g. \code{'mtl'} which
is the database schema in which the data is saved.}

\item{variable_var_code}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_classification}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_type}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_var_title}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_var_short}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_explanation}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_exp_q5}{<\code{character}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_theme}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_private}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_source}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{variable_pe_include}{<\code{charater}> See \code{\link[cc.buildr]{add_variable}}.}

\item{module_id}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_theme}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_nav_title}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_title_text_title}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_title_text_main}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_title_text_extra}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_metadata}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_dataset_info}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_dates}{<\verb{numeric vector}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{module_main_dropdown_title}{<\code{charater}> Optional. See \code{\link[cc.buildr]{add_module}}.}

\item{overwrite}{<\code{logical}> Should the data already processed and stored be
overwriten?}
}
\value{
A list of length 3, similar to the one fed to
\code{scales_variables_modules} with the new variable added, and possible a new
module.
}
\description{
Build and append a new variable
}
