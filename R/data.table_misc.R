# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Title
#'
#' Source: http://stackoverflow.com/a/18540144/4272484
#'
#' @param invec
#' @param move_command
#'
#' @return
#' @export
#'
#' @examples
#' move_vec(c("a", "b", "g"), "g first")
#' df <- data.frame(a = 1:3, b = 1:3, g = 1:3)
#' move_vec(names(df), "g first")
#' move_vec(names(df), "g first; a last")
move_vec <- function(invec, move_command) {
  move_command <- lapply(strsplit(strsplit(move_command, ";")[[1]], ",|\\s+"),
                        function(x) x[x != ""])
  movelist <- lapply(move_command, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp)-1
      } else if (A == "after") {
        after <- match(ba, temp)
      }
    } else if (A == "first") {
      after <- 0
    } else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

#' Applies \code{move_columns} to a \code{data.table}
#'
#' @param dataT       Input \code{data/table} object.
#' @param move_command
#'
#' @return
#' @export
#'
#' @examples
#' dt <- data.table::data.table(a = 1:3, b = 1:3, g = 1:3)
#' move_columns(dt, "g first")
#' move_columns(dt, "g first; a last")
move_columns <- function(data_in, move_command) {
  if (!data.table::is.data.table(data_in) & !base::is.data.frame(data_in)) stop("Function meant to work with data.frames and data.tables.")
  if (!data.table::is.data.table(data_in)) {
    df_flag <- TRUE
    data.table::setDT(data_in)
  }
  data.table::setcolorder(data_in, move_vec(names(data_in), move_command))
  if (!exists("df_flag")) return(data.table::setDF(data_in))
  else return(data_in)
}
