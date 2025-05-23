% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IMF_load_or_download.R
\name{download_imf}
\alias{download_imf}
\title{Download IMF}
\usage{
download_imf(to_obtain, column_filters, quiet)
}
\arguments{
\item{to_obtain}{A data.frame as returned by \code{\link[=determine_variables]{determine_variables()}}.}

\item{column_filters}{A character vector storing the column names of
filters. Length 0 if not present.}

\item{quiet}{Logical with default = \code{FALSE}. Should messages be displayed?
These messages are intended to give more information about the estimation
and data retrieval process.}
}
\value{
Returns a list with two named elements: \code{$df} stores the
downloaded data and \code{$to_obtain} the updated data.frame tracking which
variables still need to be obtained.
}
\description{
Downloads IMF data given a data.frame of required variables
}
