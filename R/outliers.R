
#' Find outliers using 1.5 IQR method
#'
#' Function returns \code{logical} \code{vector} indicating whether a value lies
#' within 1.5 * \code{IQR} from specified quantiles of the \code{numeric}
#' \code{vector}
#'
#' @param x      Processed \code{vector}
#' @param na.rm  Remove NAs from \code{x}
#' @param quants Which quantiles should be used, defaults to \code{c(.25, .75)}
#' @param ...    Other parameters passed to \code{quantile()}
#'
#' @return \code{logical} \code{vector} indicating whether a value is an outlier
#' @export
#'
#' @examples mark_outliers_IQR(rnorm(20, 0, 1))
mark_outliers_IQR <- function(x, na.rm = TRUE, quants = c(0.25, 0.75), return_params = FALSE, ...) {
  qnt <- quantile(unlist(x), probs = quants, na.rm = na.rm, ...)
  H <- 1.5 * IQR(unlist(x), na.rm = na.rm)
  if(return_params) list(lower = (qnt[1] - H), upper = (qnt[2] + H))
  else return(x < (qnt[1] - H) | x > (qnt[2] + H))
}

#' Find outliers using 0.01 and 0.99 quantiles
#'
#' Function returns \code{logical} \code{vector} indicating whether a value lies
#' outside of the 0.01 and 0.99 quantiles.
#'
#' @param x      Processed \code{vector}
#' @param na.rm  Remove NAs from \code{x}
#' @param quants Which quantiles should be used, defaults to \code{c(0.01, 0.99)}
#' @param ...    Other parameters passed to \code{quantile()}
#'
#' @return \code{logical} \code{vector} indicating whether a value is an outlier
#' @export
#'
#' @examples mark_outliers_quant(rnorm(20, 0, 1))
mark_outliers_quant <- function(x, na.rm = TRUE, quants = c(0.01, 0.99), return_params = FALSE, ...) {
  qnt <- quantile(unlist(x), probs = quants, na.rm = na.rm, ...)
  if(return_params) list(lower = (qnt[1]), upper = (qnt[2]))
  else return(x < qnt[1] | x > qnt[2])
}

#' Find outliers using density based method
#'
#' Function returns \code{logical} \code{vector} indicating whether a value lies
#' outside of density based clusters
#'
#' @param x
#' @param na.rm
#' @param quant
#' @param dist_method
#' @param ...
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
mark_outliers_DBSCAN <- function(x, na.rm = TRUE, quant = 0.15,
                                 dist_method = "euc", ...){
  # dist_mat <- dist(x, method = dist_method)
  # dbscan_res <- dbscan::dbscan(
  #   dist_mat, eps = quantile(dist_mat, probs = quant, na.rm = na.rm), ...)
  # return(predict(dbscan_res, data = x) == 0)
  # Same below:
  x %>%
    dist(method = dist_method) %>%
    dbscan::dbscan(eps = quantile(., probs = quant, na.rm = na.rm), ...) %>%
    predict(., data = x) %>%
    "=="(0) %>%
    return()
}

# library(ggplot2)
# ggplot(data.frame(x = x), aes(x = factor(0), y = x, colour = factor(dbscan_pred))) +
#   geom_point()
