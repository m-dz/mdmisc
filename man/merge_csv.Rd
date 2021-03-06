% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_longer.R
\name{merge_csv}
\alias{merge_csv}
\title{Merge multiple \code{.csv} files reading them one by one}
\usage{
merge_csv(path, str_to_match, suffix = "_merged")
}
\arguments{
\item{str_to_match}{}
}
\description{
Merge multiple \code{.csv} files with limited memory usage by reading them
one by one and appending them to an output file with default suffix '_merge'
}
\examples{
path <- mdmisc::get_active_file_path()
mdmisc::merge_csv(path, 'string to match')
}
