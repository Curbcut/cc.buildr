% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_data_files.R
\name{save_short_tables_qs}
\alias{save_short_tables_qs}
\title{Save short tables as .qs}
\usage{
save_short_tables_qs(
  data_folder = "data/",
  all_scales,
  skip_scales = c("building", "street", "grd30", "grd60", "grd120", "grd300")
)
}
\arguments{
\item{data_folder}{<\code{character}> Where the \code{.qsm} files should be
written to. Defaults to \verb{data/}.}

\item{all_scales}{<\verb{named list}> A named list of sf data.frame
containing all scales, normally
\code{scales_variables_modules$scales}.}

\item{skip_scales}{<\verb{character vector}> Scales to skip (not to keep as
a short table). These scales should be saved in the database instead, if
they are too large to be kept on memory.}
}
\value{
Returns an error or nothing if ran successfully. Every \code{scale} is
its own \code{.qs} containing a  trimed down  version of its data. Only
the columns from \code{ID} to \code{households} are kept, with centroid. NO data columns.
As light as possible so they live in the global environment.
}
\description{
Save short tables as .qs
}
