
#' Internal function to parse user 'yes/no' input
#'
#' @return User input if matching one of \code{c('y','yes','Yes','n','no','No')}
#'
#' @examples
yes_no_input <- function() {
  repeat{
    message(paste0('Please enter (y)es/(n)o:'))
    input <- readline()
    if(input %in% c('y','yes','Yes','n','no','No')) return(input)
  }}
