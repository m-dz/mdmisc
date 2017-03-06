
#' Merge multiple \code{.csv} files reading them one by one
#'
#' Merge multiple \code{.csv} files with limited memory usage by reading them
#' one by one and appending them to an output file with default suffix '_merge'
#'
#' @param path
#' @param str_to_match
#'
#' @return
#' @export
#' @import data.table
#' @importFrom stringr str_detect
#'
#' @examples
#' path <- mdmisc::get_active_file_path()
#' mdmisc::merge_csv(path, 'string to match')
merge_csv <- function(path, str_to_match, suffix = '_merged') {
  # Helper function
  fread_fwrite <- function(file_name, append = FALSE) {
    fwrite(
      fread(file.path(path, file_name), na.strings = c('','NA')),
      file = file.path(path, paste0(str_to_match, '.csv')), append = append)
  }
  process <- TRUE
  # List csv files and check for input and output matches
  csv_files <- list_csv_files(path)
  csv_files <- csv_files[str_detect(csv_files, str_to_match)]
  if (length(csv_files) <= 1) stop('Single or no matching files, quitting.')
  in_match <- sapply(csv_files, function(x) {x == paste0(str_to_match, '.csv')})
  out_match <- sapply(csv_files, function(x) {x == paste0(str_to_match, '_merged.csv')})
  if (any(in_match)) {
    message(paste0('File \'', paste0(str_to_match, '.csv\''), ' exactly matching input pattern, omit?'))
    input <- yes_no_input()
    if (input %in% c('y','yes','Yes')) {
      message('Omitting exactly matching file.')
      csv_files <- csv_files[!in_match]
    }}
  if (any(out_match)) {
    message(paste0('File \'', paste0(str_to_match, '_merged.csv\''), ' exists, overwrite?'))
    input <- yes_no_input()
    if (input %in% c('n','no','No')) {
      message('Quitting without overwriting.')
      process <- FALSE
    }}
  # Process
  if(process) {
    fread_fwrite(csv_files[1], append = FALSE)
    lapply(csv_files[-1], function(x) fread_fwrite(x, append = TRUE))
    return(TRUE)
  } else return(FALSE)
}