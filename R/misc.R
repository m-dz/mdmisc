
#' Lists CSV files in the specified directory
#'
#' @param path
#' @param full_names
#'
#' @return List of CSV files in the directory
#' @export
#'
#' @examples
list_csv_files <- function(path = '.', full_paths = FALSE) {
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
