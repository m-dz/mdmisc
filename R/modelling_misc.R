

#' Normalized Gini score
#'
#' Normalized Gini score for binary outcome modelling
#'
#' Based on \link{https://www.kaggle.com/wiki/RCodeForGini}
#'
#' @param actual      Actual outcome
#' @param predictions Predicted outcome
#'
#' @return Normalized Gini score
#' @export
#'
#' @examples
#' df <- data.frame(
#'   actual = c(1,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0),
#'   predicted = seq(1, 0, length.out = 40)
#' )
#' gini_val <- gini_norm(df[, 'actual'], df[, 'predicted'])
#' ## Compare to AUC from pROC package
#' # AUC_val <- pROC::roc(df[, 'actual'], df[, 'predicted'])$auc
#' # gini_val == 2 * AUC_val - 1
gini_norm <- function(actual, predictions) {
  gini(actual, predictions) / gini(actual, actual)
}

#' Gini score
#'
#' Gini score for binary outcome modelling
#' (rather use normalized version: mdmisc::gini_norm())
#'
#' Based on \link{https://www.kaggle.com/wiki/RCodeForGini}
#'
#' @param actual      Actual outcome
#' @param predictions Predicted outcome
#'
#' @return Gini score
#' @export
#'
#' @examples
gini <- function(actual, predictions) {
  if (length(actual) !=  length(predictions)) stop("Actual and Predicted need to be equal lengths!")
  df <- data.frame(actual = actual, pred = predictions, range = c(1:length(actual)))
  df <- df[order(-df$pred, df$range),]
  population.delta <- 1 / length(actual)
  total_losses <- sum(actual)
  null_losses <- rep(population.delta, length(actual)) # Hopefully is similar to accumulatedPopulationPercentageSum
  accum_losses <- df$actual / total_losses # Hopefully is similar to accumulatedLossPercentageSum
  gini.sum <- cumsum(accum_losses - null_losses) # Not sure if this is having the same effect or not
  sum(gini.sum) / length(actual)
}

#' Returns \code{data.table} with coefficients from a \code{glmnet} model
#'
#' For \code{caret} package see examples.
#'
#' @param glmnet_model
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples
#' glmnet_nonzero_coeffs(
#'   glmnet_caret_model$finalModel,
#'   glmnet_caret_model$bestTune$lambda)
glmnet_nonzero_coeffs <- function(glmnet_model, lambda = "lambda.1se") {
  stopifnot(require(glmnet), require(data.table))
  coeffs <- coef(glmnet_model, s = lambda)
  return(data.table(
    Feature = coeffs@Dimnames[[1]][(coeffs@i + 1)],
    Coefficient = coeffs@x)[order(-abs(Coefficient))])
}

#' Calculates conversion ratio table with counts and proportions
#'
#' @param dt
#' @param cols
#' @param target
#' @param order
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' n <- 200
#' sample_dt <- data.table::data.table(
#'   ColA = sample(letters[1:5], n, replace = TRUE),
#'   ColB = sample(letters[1:5], n, replace = TRUE),
#'   Target = runif(n) <= 0.1)
#'
#' cr_table(sample_dt, 'ColA', 'Target', prop_count = FALSE)
#' cr_table(sample_dt, 'ColA', 'Target', prop_count = TRUE)
#' cr_table(sample_dt, 'ColA', 'Target')
#'
#' cr_table(sample_dt, c('ColA','ColB'), 'Target')
#' cr_table(sample_dt, c('ColA','ColB'), 'Target', order = 'cr')
#' cr_table(sample_dt, c('ColA','ColB'), 'Target', order = 'count')
#' cr_table(sample_dt, c('ColA','ColB'), 'Target', order = 'prop')
cr_table <- function(dt, cols, target, order = 'cols', prop_count = TRUE) {
  # Recode character target column using first letter, assuming it is 'Yes', 'Y' etc. for sale
  recode_char_target <- function(col) {
    if(is.character(col)) ifelse(str_to_lower(str_sub(col, 1, 1)) == 'y', 1L, 0L)
    else col
  }
  switch(
    prop_count + 1,
    ret <- dt[, .(
      CR = sum(recode_char_target(get(target)), na.rm = TRUE)/.N*100),
      by = eval(cols)],
    ret <- dt[, .(
      CR = sum(recode_char_target(get(target)), na.rm = TRUE)/.N*100,
      Events = sum(recode_char_target(get(target)), na.rm = TRUE),
      Count = .N,
      Prop = .N/nrow(dt) * 100
    ), by = eval(cols)]
  )
  switch(
    str_to_lower(order),
    cr = setorderv(ret, 'CR', -1),
    count = setorderv(ret, 'Count', -1),
    prop = setorderv(ret, 'Prop', -1),
    cols = setorderv(ret, cols)
  )
  ret
}

#' Check whether data features match the ones used in the model
#'
#' @param x         \code{data.matrix} with data to be scored
#' @param glm_model GLM model object
#' @param s         Optional, lambda parameter (`s` in the \code{glmnet} package)
#'
#' @return
#' @export
#'
#' @examples
check_features_match_glm <- function(x, glm_model, lambda = 'lambda.1se') {
  names_diff <- mdmisc::setdiff_two_way(
    dimnames(x)[[2]],
    dimnames(coef(glm_model, lambda = lambda))[[1]]
  )
  if(!identical(names_diff$`First`, character())) stop('Incorrect features in the data.')
  if(names_diff$`Second` != '(Intercept)') stop('Missing features in the data.')
  return(TRUE)
}
