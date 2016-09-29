

# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# .datatable.aware = TRUE
# Not needed:
# http://stackoverflow.com/questions/15223367/
# wrapping-data-table-using-an-evaluated-call-in-a-package

#' Rearrange character vector.
#'
#' \code{reorder_vec} rearranges and returns an input character vector.
#' Possible "rearrange" words: c("before", "after", "first", "last")
#'
#' Source: http://stackoverflow.com/a/18540144/4272484 with minor amendments
#'
#' @param vec_in Input character vector.
#' @param reorder_command Rearrange command.
#'
#' @return Rearranged \code{vec_in}.
#' @export reorder_vec
#'
#' @examples
#' reorder_vec(c("a", "b", "g"), "g first")
#' df <- data.frame(a = 1:3, b = 1:3, g = 1:3)
#' reorder_vec(names(df), "g first")
#' reorder_vec(names(df), "g first; a last")
reorder_vec <- function(vec_in, reorder_command) {
  reorder_command <- lapply(strsplit(strsplit(reorder_command, ";")[[1]], ",|\\s+"),
                         function(x) x[x != ""])
  reorder_list <- lapply(reorder_command, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  vec_out <- vec_in
  for (i in seq_along(reorder_list)) {
    temp <- setdiff(vec_out, reorder_list[[i]][[1]])
    A <- reorder_list[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- reorder_list[[i]][[2]][2]
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
    vec_out <- append(temp, values = reorder_list[[i]][[1]], after = after)
  }
  vec_out
}

#' Applies \code{reorder_vec} to a \code{data.table}.
#'
#' @param data_in Input \code{data.table} object.
#' @param reorder_command Rearrange command string.
#'
#' @return Rearranged input object.
#' @export reorder_columns
#'
#' @examples
#' dt <- data.table::data.table(a = 1:3, b = 1:3, g = 1:3)
#' reorder_columns(dt, "g first")
#' reorder_columns(dt, "g first; a last")
reorder_columns <- function(data_in, reorder_command) {
  if (!data.table::is.data.table(data_in)) stop(
    paste("Function is designed to work with a data.table object, please use",
          "data.table::setDT() before passing it to reorder_columnsumns."))
  data.table::setcolorder(data_in, reorder_vec(names(data_in), reorder_command))
}

#' Print sum(s) of missing values
#'
#' Prints sum(s) of missing values (coded as \code{NA}) in an object
#' (\code{vector}, \code{data.frame}, \code{data.table} etc.).
#'
#' @param data Input object.
#' @param prop Flag indicating if results should include proportions.
#' @param print Flag indicating if results should be printed.
#'
#' @return Table with missing values counts (and proportions).
#' @export
#'
#' @examples
#' dt <- data.table::data.table(a = rnorm(n=1000, mean=20, sd=5),
#'                              b = rnorm(n=1000, mean=20, sd=5),
#'                              c = rnorm(n=1000, mean=20, sd=5))
#' dt[sample.int(nrow(dt), round(nrow(dt) * 0.15)), a := NA_real_]
#' count_NAs(dt)
#' count_NAs(dt, prop = TRUE)
#' count_NAs(dt[, a])
#' count_NAs(dt[, a], prop = TRUE)
#' count_NAs(dt[, .(a)])
#' count_NAs(dt[, .(a)], prop = TRUE)
count_NAs <- function(data_in, prop = FALSE, print = TRUE) {
  if (is.null(dim(data_in))) {
    message(paste("To see nicer formatted output use data.table's",
                  "dt[, .(col_name)] syntax."))
    NAs_tab <- data.frame(Count = sum(is.na(data_in)), row.names = "Variable")
    if(prop) NAs_tab <-
      cbind(Count = NAs_tab,
            Prop = round(sum(is.na(data_in))/length(data_in)*100, 2))
  } else {
    NAs_tab <- data.table::data.table(Variable = names(data_in),
                                      Count = colSums(is.na(data_in)))
    if(prop) NAs_tab[, Prop := round(Count/nrow(data_in)*100, 2)]
    data.table::setorder(NAs_tab, -Count, Variable)
  }
  if (print) print(NAs_tab)
  else return(NAs_tab)
}

#' Counts and percentages table by a specified colname(s)
#'
#' @param data_in \code{data.table} to process
#' @param colname Column name(s) to calculate counts and parcentages
#'
#' @return \code{data.table} with counts and parcentages
#' @export
#' @import data.table
#'
#' @examples
#' set.seed(2016)
#' dt <- data.table::data.table(A = sample(LETTERS[1:10], 200, replace = TRUE),
#'                              B = sample(LETTERS[1:10], 200, replace = TRUE))
#' table_data_table(dt, "A")
#' table_data_table(dt, "B")
#' table_data_table(dt, c("A", "B"))
table_data_table <- function(data_in, colname) {
  if (!data.table::is.data.table(data_in)) stop(
    paste("Function is designed to work with a data.table object, please use",
          "data.table::setDT() before passing it to reorder_columnsumns."))
  t <- data_in[, .(N = .N,
                   PCT = round(.N/nrow(data_in)*100, 2)),
               by = colname][order(-N)]
  return(t)
}