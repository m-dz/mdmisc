
#' Group categories by (cumulative) frequencies
#'
#' @param dt
#' @param feature
#' @param threshold
#' @param cum_threshold
#' @param no_of_categories
#' @param return_data
#' @param modify
#' @param other_cat_name
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(2016)
#' dt <- data.table::data.table('cat' = sample(letters[1:10], 20, replace = TRUE), 'cont' = rnorm(20))
#'
#' # View cumulative frequencies
#' group_by_threshold(dt, 'cat')
#'
#' # Group bottom 20\% categories based on cumulative frequency
#' dt_mod <- group_by_threshold(dt, 'cat', cum_threshold = 0.2, return_data = TRUE)
#' dt_mod
#'
#' # Group and modify in place
#' group_by_threshold(dt, 'cat', cum_threshold = 0.2, return_data = TRUE, modify = TRUE)
#' dt
group_by_threshold <- function(dt,
                          feature,
                          threshold = NULL,
                          cum_threshold = NULL,
                          no_of_categories = NULL,
                          return_data = FALSE,
                          modify = FALSE,
                          other_cat_name = 'OTHER') {
  # Parameters check
  if(isTRUE(return_data) & (sum(is.null(threshold), is.null(cum_threshold), is.null(no_of_categories)) == 2)) {
    stop('Incorrect parameters.')
  }
  if(!isTRUE(return_data) & (sum(is.null(threshold), is.null(cum_threshold), is.null(no_of_categories)) < 2)) {
    stop('Incorrect parameters.')
  }
  # Declare variable first to pass R CMD check
  # TODO: Why this is needed?
  N <- PCT <- CumPCT <- NULL
  # Set dt to data.table
  if(!is.data.table(dt)) setDT(dt)
  # Check if not numeric
  if(is.numeric(dt[[feature]])) stop(paste0('Feature \'', feature, '\' is numeric.'))
  # no_of_categories correction
  if(mdmisc::is.not.null(no_of_categories)) {
    if(no_of_categories >= dt[, length(unique(get(feature)))]) {
      warning(paste0('Number of categories to leave is smaller than in the data, leaving every category in.'))
      no_of_categories <- dt[, length(unique(get(feature)))]
    }
  }

  # Set feature to discrete
  set(dt, j = feature, value = as.character(dt[[feature]]))
  # Calculate cumulative frequency for each category
  freq_tab <- dt[, list(N = .N), by = feature][order(-N)]
  freq_tab[, PCT := N / sum(N)][, CumPCT := cumsum(PCT)]
  # Prepare feature names
  top_cats <- freq_tab[, get(feature)]
  # Categories not to be collapsed
  if(!is.null(threshold))        top_cats <- top_cats[freq_tab[, PCT]    <= (threshold)]
  if(!is.null(cum_threshold))    top_cats <- top_cats[freq_tab[, CumPCT] <= (1 - cum_threshold)]
  if(!is.null(no_of_categories)) top_cats <- top_cats[1:no_of_categories]
  # Group categories and return if return_data is true
  if (return_data) {
    if (!modify) {
      dt_out <- copy(dt)
      return(dt_out[!(get(feature) %in% top_cats), c(feature) := other_cat_name])
    } else {
      dt[!(get(feature) %in% top_cats), c(feature) := other_cat_name]
      return(dt)
    }
  } else {
    if(mdmisc::is.not.null(threshold))             return(freq_tab[, ToGroup := !(PCT    <= (threshold))][])
    else if(mdmisc::is.not.null(cum_threshold))    return(freq_tab[, ToGroup := !(CumPCT <= (1 - cum_threshold))][])
    else if(mdmisc::is.not.null(no_of_categories)) return(freq_tab[, ToGroup := c(rep(FALSE, no_of_categories), rep(TRUE, .N - no_of_categories))][])
    else return(freq_tab)
  }
}
