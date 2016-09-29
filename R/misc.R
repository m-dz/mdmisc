
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