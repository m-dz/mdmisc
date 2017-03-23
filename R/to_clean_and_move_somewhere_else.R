#' Plots correlation matrix for categorical variables
#'
#' @param dt
#' @param levels_max
#' @param print
#'
#' @return
#' @export
#' @import data.table
#' @importFrom magrittr '%>%'
#' @importFrom ggcorrplot ggcorrplot
#'
#' @examples
#' library(data.table); library(magrittr)
#' set.seed(2016)
#' sample(LETTERS[1:4], 10000, replace = TRUE, prob = c(0.1, 0.2, 0.65, 0.05)) %>% plot_cor_cat()
plot_cor_cat <- function(dt, levels_max = 1000, print = TRUE, simulate.p.value = FALSE) {
  dt_temp <- dt %>% extract_cat_cols(levels_max)
  names_grid <- names(dt_temp) %>% expand.grid(x = ., y = .) %>% data.table()
  names_grid[, c("cramerV", "chiSqP") := cramersV_with_pvalue(
    dt_temp, as.character(x), as.character(y), simulate.p.value = simulate.p.value),
    by = 1:nrow(names_grid)]
  cramerV <- dcast(names_grid, x ~ y, value.var = "cramerV") %>% .[ , x := NULL] %>% as.matrix()
  rownames(cramerV) <- colnames(cramerV)
  chiSqP <- dcast(names_grid, x ~ y, value.var = "chiSqP") %>% .[ , x := NULL] %>% as.matrix()
  rownames(chiSqP) <- colnames(chiSqP)
  g <- ggcorrplot(
    cramerV, # hc.order = TRUE, p.mat = p.mat, insig = "pch"
    type = "lower",
    outline.color = "white",
    hc.order = TRUE,
    ggtheme = ggplot2::theme_light,
    colors = c("#006837", "white", "#A50026"),
    lab = TRUE,
    p.mat = chiSqP) +
    theme(axis.ticks = element_blank(), panel.grid = element_blank())
  if (print) print(g) else return(g)
}
# set.seed(2016)
# dt <- data.table(
#   Cat1 = sample(LETTERS[1:4], 10000, replace = TRUE),
#   Cat2 = sample(LETTERS[1:4], 10000, replace = TRUE),
#   Cat3 = sample(LETTERS[1:4], 10000, replace = TRUE),
#   Cat4 = sample(LETTERS[1:4], 10000, replace = TRUE),
#   Cont1 = runif(10000),
#   Cont2 = runif(10000),
#   Cont3 = runif(10000),
#   Cont4 = runif(10000)
# )
# levels_max = 1000
# print = TRUE
# extract_cat_cols(dt, levels_max = levels_max)
# plot_cor_cat(dt)


#' Plots correlation matrix for continuous variables
#'
#' @param dt
#' @param print
#'
#' @return
#' @export
#' @import ggcorrplot
#' @import ggplot2
#' @import data.table
#' @importFrom magrittr "%>%"
#'
#' @examples
#' library(data.table); library(magrittr)
#' data(mtcars)
#' mtcars %>% data.table() %>% plot_cor_cont()
plot_cor_cont <- function(dt, print = TRUE) {
  calc_cor <- function(dt) list(
    corr = dt %>% na.omit() %>% cor() %>% round(2),
    p.mat = dt %>% na.omit() %>% cor_pmat())
  ls <- dt %>%
    extract_cont_cols() %>%
    calc_cor()
  # corr <- dt %>% na.omit() %>% cor() %>% round(2)
  # p.mat <- dt %>% na.omit() %>% cor_pmat()
  g <- ggcorrplot(
    ls[['corr']], # hc.order = TRUE, p.mat = p.mat, insig = "pch"
    type = "lower",
    outline.color = "white",
    ggtheme = ggplot2::theme_light,
    colors = c("#006837", "white", "#A50026"),
    lab = TRUE,
    p.mat = ls[['p.mat']]) +
    theme(axis.ticks = element_blank(), panel.grid = element_blank())
  if (print) {
    print(g)
    return(TRUE)
  }
  else return(g)
}

#' Title
#'
#' @param dt
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' dt <- data.table(V1 = 1.1, V2 = 2L, V3 = 'a')
#' str(dt)
#' extract_cont_cols(dt)
#' extract_cont_cols(dt, exclude = 'V1')
#' extract_cont_cols(dt, exclude = 'V3')
#' dt
extract_cont_cols <- function(dt, exclude = NA) {
  ### TODO: Check which one is faster?
  cols_cont <- sapply(dt, function(x) (is.numeric(x))) & !(names(dt) %in% exclude)
  # dt[ , cols_cont, with = FALSE]
  dt[ , .SD, .SDcols = cols_cont]
}

#' Title
#'
#' @param dt
#' @param levels_max
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   V1 = rep(1.1, 10),
#'   V2 = rep(2L, 10),
#'   V3 = rep('a',10),
#'   V4 = letters[1:10])
#' extract_cat_cols(dt, levels_max = NULL, levels_min = 1)
#' extract_cat_cols(dt, levels_max = 5, levels_min = 1)
#' extract_cat_cols(dt, levels_max = NULL, levels_min = 5)
#' extract_cat_cols(dt, exclude = 'V3')
#'
#' # Args check:
#' extract_cat_cols(dt, levels_max = -1, levels_min = 1)
#' extract_cat_cols(dt, levels_max = NULL, levels_min = -1)
#' extract_cat_cols(dt, levels_max = 1, levels_min = 5)
#' dt
extract_cat_cols <- function(dt, levels_max = NULL, levels_min = 1, exclude = NA) {
  if(
    (length(levels_max) > 0 && levels_max < 1) ||
    (length(levels_min) > 0 && levels_min < 1) ||
    (length(levels_max) > 0 && length(levels_min) > 0 && levels_max < levels_min)
  ) stop('Please check your arguments.')

  if (is.null(levels_max) & levels_min >= 1)
    fun <- function(x) (is.character(x) && length(unique(x)) >= levels_min)
  else
    fun <- function(x) (is.character(x) && length(unique(x)) >= levels_min && length(unique(x)) <= levels_max)
  cols_cat <-
    xor(
      sapply(dt, fun),
      names(dt) %in% exclude
    )
  dt[ , .SD, .SDcols = cols_cat]
}
### TODO: Test with data.table and data.frame
### TODO: Factors?
### TODO: Tests! Tests! Tests!
