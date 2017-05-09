#' Calculater Cramer's V matrix for categorical data
#'
#' @param dt
#' @param levelsMax
#'
#' @return
#' @export
#' @import data.table
#' @import magrittr
#'
#' @examples
cramersV_mat <- function(dt) {
  if (!is.null(dt)) {
    if (!all(sapply(dt, is.character)))
      stop("'dt' must be categorical only.")
    stopifnot(all(sapply(dt, is.atomic)))
  }
  names_grid <- names(dt) %>% expand.grid(x = ., y = .) %>% data.table()

  ### TODO: lapply instead of by?
  names_grid[,
    c("cramersV", "chiSqP") := cramersV_with_pvalue(
      dt, as.character(x), as.character(y)),
    by = 1:nrow(names_grid)]

  cramersV <- dcast(names_grid, x ~ y, value.var = "cramersV") %>% .[ , x := NULL] %>% as.matrix()
  rownames(cramersV) <- colnames(cramersV)
  return(cramersV)
}

#' Title
#'
#' @param dt
#' @param arg1
#' @param arg2
#'
#' @return
#' @export
#'
#' @examples
cramersV_with_pvalue <- function(dt, arg1, arg2, simulate.p.value = FALSE) {
  tab <- table(dt[[arg1]], dt[[arg2]], useNA = "ifany")
  chi2 <- chisq.test(tab, simulate.p.value)
  cramersV <- unname(sqrt((chi2$statistic / sum(tab)) / (min(ncol(tab), nrow(tab)) - 1)))
  return(list(cramersV = cramersV, pvalue = chi2$p.value))
}

#' Title
#'
#' @param dt
#' @param exclude_list
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#' dt <- data.table(V1 = 1, V2 = 2, V3 = 3)
#' dt
#' remove_single_value_cols(dt)
remove_single_value_cols <- function(dt, exclude_list = NULL, verbose = FALSE) {
  message('Warning, function modifies input data.')
  cols_to_drop <- character()
  for (col_name in names(dt)) {
    if((dt[, length(unique(get(col_name)))] < 2) &
       !(col_name %in% exclude_list)) {
      if(verbose) message(
        paste0("Removing column: ", col_name, " with value: ", dt[, unique(get(col_name))]))
      cols_to_drop <- c(cols_to_drop, col_name)
    }
  }
  drop_cols(dt, cols_to_drop, modify = TRUE)
  return(dt)
}

#' Title
#'
#' @param dt
#' @param exclude_list
#'
#' @return
#' @export
#'
#' @examples
plotCorrCat <- function(dt, exclude_list) {
  stopifnot(require(dplyr))
  dt %>% drop_cols(exclude_list) %>% extract_cat_cols() %>% plot_cor_cat()
}

#' Title
#'
#' @param dt
#' @param exclude_list
#'
#' @return
#' @export
#'
#' @examples
PlotCorrCont <- function(dt, exclude_list) {
  stopifnot(require(dplyr))
  dt %>% drop_cols(exclude_list) %>% extract_cont_cols() %>% plot_cor_cont()
}

#' Title
#'
#' @param dt
#' @param exclude_list
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
find_corr_cont <- function(dt, exclude_list, cutoff = 0.9, exact = TRUE, ...) {
  dt %>% drop_cols(exclude_list) %>% extract_cont_cols() %>% cor() %>%
    caret::findCorrelation(cutoff = cutoff, verbose = TRUE, ...)
}

#' Title
#'
#' @param dt
#' @param exclude_list
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
find_corr_cat <- function(dt, exclude_list, cutoff = 0.9, exact = TRUE, ...) {
  dt %>% drop_cols(exclude_list) %>% extract_cat_cols() %>% cramersV_mat() %>%
    caret::findCorrelation(cutoff = cutoff, exact = exact, ...)
}
