% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_postgresql.R
\name{db_table_size}
\alias{db_table_size}
\title{Get Pretty-Printed Table Size from PostgreSQL Database}
\usage{
db_table_size(table_name)
}
\arguments{
\item{table_name}{<\code{character}> The name of the table for which the size
is to be retrieved. The table name should be fully qualified if it is not in
the default schema, i.e., "schema.table_name".}

\item{conn}{<\code{DBIConnection}> Represents an active connection to a PostgreSQL database.}
}
\value{
A data frame with a single column and a single row containing the
pretty-printed size of the table. The column name is \code{pg_size_pretty}.#'
}
\description{
This function retrieves the disk space used by a specified table within a
PostgreSQL database and returns it in a human-readable format. It uses
PostgreSQL's \code{pg_size_pretty} and \code{pg_relation_size} functions to obtain and
format the table size.
}
