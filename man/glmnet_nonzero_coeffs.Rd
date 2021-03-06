% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling_misc.R
\name{glmnet_nonzero_coeffs}
\alias{glmnet_nonzero_coeffs}
\title{Returns \code{data.table} with coefficients from a \code{glmnet} model}
\usage{
glmnet_nonzero_coeffs(glmnet_model, lambda = "lambda.1se")
}
\arguments{
\item{lambda}{}
}
\description{
For \code{caret} package see examples.
}
\examples{
glmnet_nonzero_coeffs(
  glmnet_caret_model$finalModel,
  glmnet_caret_model$bestTune$lambda)
}
