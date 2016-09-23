
#' Funtion returns the active file's path
#'
#' @return Active file's path
#' @export
#' @importFrom rstudioapi getActiveDocumentContext
#'
#' @examples getActiveFilePath()
getActiveFilePath <- function() {
  dirname(rstudioapi::getActiveDocumentContext()$path)
}
