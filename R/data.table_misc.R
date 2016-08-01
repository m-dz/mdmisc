
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

.datatable.aware = TRUE

#' Rearrange character vector.
#'
#' \code{move_vec} rearranges and returns an input character vector.
#' Possible "rearrange" words: c("before", "after", "first", "last")
#'
#' Source: http://stackoverflow.com/a/18540144/4272484 with minor amendments
#'
#' @param vec_in Input character vector.
#' @param move_command Rearrange command.
#'
#' @return Rearranged \code{vec_in}.
#' @export move_vec
#'
#' @examples
#' move_vec(c("a", "b", "g"), "g first")
#' df <- data.frame(a = 1:3, b = 1:3, g = 1:3)
#' move_vec(names(df), "g first")
#' move_vec(names(df), "g first; a last")
move_vec <- function(vec_in, move_command) {
  move_command <- lapply(strsplit(strsplit(move_command, ";")[[1]], ",|\\s+"),
                        function(x) x[x != ""])
  move_list <- lapply(move_command, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  vec_out <- vec_in
  for (i in seq_along(move_list)) {
    temp <- setdiff(vec_out, move_list[[i]][[1]])
    A <- move_list[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- move_list[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp)-1
      } else if (A == "after") {
        after <- match(ba, temp)
      }
    } else if (A == "first") {
      after <- 0
    } else if (A == "last") {
      after <- length(vec_out)
    }
    vec_out <- append(temp, values = move_list[[i]][[1]], after = after)
  }
  vec_out
}

#' Applies \code{move_columns} to a \code{data.table}.
#'
#' @param data_in Input \code{data.table} object.
#' @param move_command Rearrange command string.
#'
#' @return Rearranged input object.
#' @export move_columns
#'
#' @examples
#' dt <- data.table::data.table(a = 1:3, b = 1:3, g = 1:3)
#' move_columns(dt, "g first")
#' move_columns(dt, "g first; a last")
move_columns <- function(data_in, move_command) {
  if (!data.table::is.data.table(data_in)) stop(
    paste("Function is designed to work with a data.table object, please use",
          "data.table::setDT() before passing it to move_columns."))
  data.table::setcolorder(data_in, move_vec(names(data_in), move_command))
}
