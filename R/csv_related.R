
#' Read (using \code{data.table::fread()}) most recent file from directory
#'
#' @param dir_    Directory path
#' @param pattern Character string to filter files
#' @param oldest  Boolean switch to reverse ordering
#'
#' @return
#' @export
#'
#' @examples
fread_most_recent_file <- function(dir_, pattern = NULL, oldest = FALSE) {
  csv_list <- list_csv_files(dir_, full_names = FALSE)
  if(is.not.null(pattern)) {
    csv_list <- csv_list[stringr::str_detect(csv_list, pattern)]
  }
  csv_to_read <- ifelse(!oldest, tail(csv_list, 1), head(csv_list, 1))
  data.table::fread(file.path(dir_, csv_to_read))
}

#' Read (using \code{data.table::fread()}) and rbind files from directory
#'
#' @param dir_    Directory path
#' @param verbose Boolean switch for printing file names etc.
#' @param fill    If \code{TRUE} fills missing columns with NAs. By default \code{FALSE}.
#' @param pattern Character string to filter files
#'
#' @return
#' @export
#'
#' @examples
fread_CSVs_from_dir <- function(dir_, verbose = FALSE, fill = TRUE, pattern = NULL) {
  dir_to_read <- list_csv_files(dir_, full_names = TRUE)
  fread_CSVs_from_list(dir_to_read, verbose = verbose, fill = fill, pattern = pattern)
}

#' Read (using \code{data.table::fread()}) and rbind files from list
#'
#' @param file_list Character vector from e.g. \code{list.files()} function
#' @param verbose   Boolean switch for printing file names etc.
#' @param fill      If \code{TRUE} fills missing columns with NAs. By default \code{FALSE}.
#'
#' @return
#' @export
#'
#' @examples
fread_CSVs_from_list <- function(file_list, verbose = FALSE, fill = TRUE, pattern = NULL) {
  out_list <- list()
  if(is.not.null(pattern)) {
    file_list <- file_list[stringr::str_detect(file_list, pattern)]
  }
  for (file_name in file_list) {
    data_name <- str_extract(file_name, '([0-9a-zA-Z -]*)\\.csv$')
    if(verbose) print(paste0("Reading file: ", data_name))
    out_list[[str_replace(data_name, ".csv", "")]] <- data.table::fread(file_name)
  }
  if(fill) {
    warning('Function is filling missing columns!')
    rbindlist(out_list, fill = TRUE)
  } else {
    rbindlist(out_list, fill = FALSE)
  }
}
