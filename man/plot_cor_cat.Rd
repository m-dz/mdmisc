% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_clean_and_move_somewhere_else.R
\name{plot_cor_cat}
\alias{plot_cor_cat}
\title{Plots correlation matrix for categorical variables}
\usage{
plot_cor_cat(dt, levels_max = 1000, print = TRUE,
  simulate.p.value = FALSE)
}
\arguments{
\item{print}{}
}
\description{
Plots correlation matrix for categorical variables
}
\examples{
library(data.table); library(magrittr)
set.seed(2016)
sample(LETTERS[1:4], 10000, replace = TRUE, prob = c(0.1, 0.2, 0.65, 0.05)) \%>\% plot_cor_cat()
}
