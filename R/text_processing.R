
#' Convert 'string' to 'String', or 'this.string' to 'ThisString'
#'
#' Based on https://stackoverflow.com/a/11672354/4272484
#'
#' @param x        Input character vector
#' @param split    Pattern/char(s) to split on
#' @param collapse Char(s) to join with
#'
#' @return
#' @export
#'
#' @examples
#' t1 <- c('this text', 'next text')
#' t2 <- c('this.text', 'next.text')
#' t3 <- 'this'
#' t4 <- 'this text'
#'
#' str_to_CamelCase(t1) # c('ThisText', 'NextText')
#' str_to_CamelCase(t1, collapse = ' ') # c('This Text', 'Next Text')
#' str_to_CamelCase(t2, split = '.', collapse = ' ') # c('This Text', 'Next Text')
#'
#' str_to_CamelCase(t3) # 'This'
#' str_to_CamelCase(t4) # 'ThisText'
str_to_CamelCase <- function(x, split = ' ', collapse = '') {
  sapply(strsplit(x, Hmisc::escapeRegex(split)), function(x) paste(str_to_Capital(x), collapse = collapse))
}

#' Helper function for str_to_CamelCase, convert 'string' to 'String'
#'
#' @param x Input string
#'
#' @return
#'
#' @examples
str_to_Capital <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
}
