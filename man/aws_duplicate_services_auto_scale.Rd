% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aws.R
\name{aws_duplicate_services_auto_scale}
\alias{aws_duplicate_services_auto_scale}
\title{Duplicate AWS Services and Auto-Scale for a new city}
\usage{
aws_duplicate_services_auto_scale(
  new_city,
  from = "montreal",
  from_TG = NULL,
  new_TG = NULL,
  from_LB = NULL,
  new_LB = NULL,
  suffix = NULL
)
}
\arguments{
\item{new_city}{<\code{character}> string specifying the name of the new city
for which the AWS services should be duplicated.}

\item{from}{<\code{character}> string specifying the name of the original city
from which the AWS services are duplicated. Defaults to "montreal".}

\item{from_TG}{An optional \code{data.frame} specifying the original target group
details. If \code{NULL}, it will be fetched automatically based on the
\code{from} parameter.}

\item{new_TG}{An optional \code{data.frame} specifying the new target group details.
If \code{NULL}, it will be fetched automatically based on the \code{new_city} parameter.}

\item{from_LB}{An optional \code{data.frame} specifying the original load balancer
details. If \code{NULL}, it will be fetched automatically based on the
\code{from} parameter.}

\item{new_LB}{An optional \code{data.frame} specifying the new load balancer details.
If \code{NULL}, it will be fetched automatically based on the \code{new_city} parameter.}

\item{suffix}{<\code{character}> Issues sometimes when a service has been deleted, it
stays in a draining states for ever. We need to use a different name. Add a suffix
just for the service name.}
}
\value{
The function does not return any value but performs operations that
result in the creation and update of AWS resources.
}
\description{
Duplicates AWS auto-scaling configurations and related resources from an
existing city to a new city. This includes duplicating target groups and
load balancers, and updating scaling policies accordingly.
}
\details{
This function automatically identifies and duplicates AWS target groups,
load balancers, and auto-scaling policies from a specified "from" city
to a "new_city". It handles the creation of new target groups and load
balancers if not explicitly provided and updates scaling policies to
reflect the new city's resources.
}
