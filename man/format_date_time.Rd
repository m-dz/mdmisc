% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{format_date_time}
\alias{format_date_time}
\title{Formats \code{POSIXct} (e.g. \code{Sys.time()}) object to YYYYMMDD_HHMMSS string}
\usage{
format_date_time(date_time = Sys.time())
}
\value{
Formated string
}
\description{
Formats \code{POSIXct} (e.g. \code{Sys.time()}) object to YYYYMMDD_HHMMSS string
}
\examples{
format_date_time()
format_date_time(date_time = as.POSIXct("2017-02-15 16:05:31 GMT"))
}
