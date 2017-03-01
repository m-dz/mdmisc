
#' Title
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

#' Formats \code{POSIXct} (e.g. \code{Sys.time()}) object to YYYYMMDD_HHMMSS string
#'
#' @return Formated string
#' @export
#'
#' @examples
#' format_date_time()
#' format_date_time(date_time = as.POSIXct("2017-02-15 16:05:31 GMT"))
format_date_time <- function(date_time = Sys.time()) { gsub(' ','_',gsub('[-:]', '', date_time)) }

#' Checks for an argument not being NA
#'
#' @param x
#'
#' @return \code{Boolean} indicating whether x is not NA
#' @export
#'
#' @examples
is.not.na <- function(x) { !is.na(x) }

#' Checks for an argument not being NULL
#'
#' @param x
#'
#' @return \code{Boolean} indicating whether x is not NULL
#' @export
#'
#' @examples
is.not.null <- function(x) { !is.null(x) }

#' \code{microbenchmark::microbenchmark} with default \code{reps = 1}
#'
#' @param expr Expressions to benchmark.
#' @param reps No. of replications.
#'
#' @return Object created and benchmark results.
#' @export
#' @importFrom microbenchmark microbenchmark
#'
#' @examples
#'
single_microbenchmark <- function(expr, reps = 1) {
  microbenchmark::microbenchmark(expr, times = reps, unit = "s")
}

#' Two way set difference
#'
#' @param first
#' @param second
#'
#' @return
#' @export
#'
#' @examples
setdiff_two_way <- function(first, second) {
  list(
    "First" = setdiff(first, second),
    "Second" = setdiff(second, first))
}

#' Read password from specified file
#'
#' @param file Text file with password stored
#'
#' @return String with password
#' @export
#'
#' @examples
read_pass <- function(file_path) {
  con = file(file_path, "r")
  readLines(con, n = 1)
  close(con)
}

#' Lists CSV files in the specified directory
#'
#' @param path
#' @param full_names
#'
#' @return List of CSV files in the directory
#' @export
#'
#' @examples
list_csv_files <- function(path = '.', full_names = FALSE) {
  list.files(path = path, pattern = "\\.csv$", full.names = full_names)
}

#' Funtion returns the active file's path
#'
#' @return Active file's path
#' @export
#' @importFrom rstudioapi getActiveDocumentContext
#'
#' @examples getActiveFilePath()
get_active_file_path <- function() {
  dirname(rstudioapi::getActiveDocumentContext()$path)
}

#' Clear console
#'
#' @return None (invisible NULL).
#' @export
#'
#' @examples ClearConsole()
clear_console <- function() {
  cat("\014")
}

#' Recreate ggplot2 colour palette
#'
#' @param n Number of colours
#'
#' @return \code{Vector} of colours
#' @export
#'
#' @examples gg_color_palette(3)
gg_color_palette <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Returns sorted names of the object
#'
#' @param obj Object to process
#'
#' @return \code{character} \code{vector} of object's sorted names
#' @export
#'
#' @examples df <- data.frame(b = 1, a = 2); names(df); sorted_names(df)
sorted_names <- function(obj) {
  sort(names(obj))
}

#' Save data set to \code{dump_dir} folder
#'
#' @param dt        Data set to save
#' @param file_name File name
#' @param dump_dir  Folder
#'
#' @return
#' @export
#'
#' @examples
dump_to_csv <- function(dt, file_name = paste0(deparse(substitute(dt))),
                        dump_dir = "C:/Users/dziedzm/Desktop/temp_file_dump") {
  write.csv(dt, file = file.path(dump_dir, paste0(file_name, ".csv")),
            row.names = FALSE)
}
