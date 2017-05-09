
#' Return a random sample of row ids (by grouping)
#'
#' @param dt
#' @param pct
#' @param grouping
#' @param sort
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' set.seed(2016)
#' size <- 10
#' dt <- data.table(
#'   id = 1:size, A = sample(letters[1:3], size, replace = TRUE), B = 'N',
#'   C = sample(1:100, size, replace = TRUE) + sample(30:70, size, replace = TRUE))
#' set.seed(2016)
#' sample_ids(dt, 0.5, grouping = 'A')
#' set.seed(2016)
#' sample_ids(dt, 0.5, grouping = 'A', sort = TRUE)
#' set.seed(2016)
#' sample_ids(dt, 0.5, grouping = 'A', return_grouping = TRUE)
#' set.seed(2016)
#' sample_ids(dt, 0.5, grouping = 'A', sort = TRUE, return_grouping = TRUE)
sample_ids <- function(dt, pct, grouping = NULL, sort = FALSE, return_grouping = FALSE) {
  out <- dt[, .I[sample(.N, round(.N*pct))], by = grouping]
  if(sort) {
    if(return_grouping) return(out[order(V1)])
    else return(out[order(V1), V1])
  } else {
    if(return_grouping) return(out)
    else return(out[, V1])
  }
}

#' Title
#'
#' @param dt       \code{data.table} to sample from
#' @param pct      Percentage of data sampled
#' @param grouping Grouping (a \code{character vector})
#' @param sort     c('no', 'id', 'gr')
#'
#' @return \code{data.table} with sampled data
#' @export
#' @import data.table
#'
#' @examples
#' set.seed(2016)
#' size <- 10
#' dt <- data.table(
#'   id = 1:size, A = sample(letters[1:3], size, replace = TRUE), B = 'N',
#'   C = sample(1:100, size, replace = TRUE) + sample(30:70, size, replace = TRUE))
#' set.seed(2016)
#' sample_rows(dt, 0.5, grouping = 'A')
#' sample_rows(dt, 0.5, grouping = 'A', sort = TRUE)
#' sample_rows(dt, 0.5, grouping = c('A','B'), sort = TRUE)
sample_rows <- function(dt, pct, grouping, sort = FALSE) {
  out <- dt[dt[, sample(.I, round(.N*pct)), by = grouping]$V1, ]
  if(sort) {
    setorderv(out, grouping)
    out
  } else out
}

#' Replace given value in a random pct of rows (by grouping)
#'
#' Function modifies the input dt!
#'
#' @param dt
#' @param pct
#' @param col_name
#' @param value
#' @param grouping
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' set.seed(2016)
#' size <- 10
#' dt <- data.table(
#'   id = 1:size, A = sample(letters[1:3], size, replace = TRUE), B = 'N',
#'   C = sample(1:100, size, replace = TRUE) + sample(30:70, size, replace = TRUE))
#' set.seed(2016)
#' sample_and_replace_value(dt, pct = 0.5, col_name = 'B', value = 'Y', grouping = 'A')
#' dt[, table(A, B)]
#' sample_and_replace_value(dt, pct = 0.5, col_name = 'B', value = 'Y', grouping = 'A', sort = TRUE)
#' dt
sample_and_replace_value <- function(dt, pct, col_name, value, grouping = NULL, sort = FALSE) {
  # dt[dt[, .I[sample(.N, round(.N*pct))], by = grouping]$V1, (col_name) := value]
  dt[dt[, sample(.I, round(.N*pct)), by = grouping]$V1, (col_name) := value]
  if(sort) setorderv(dt, grouping) else dt
}

#' Leave selected columns in input \code{data.table}
#'
#' @param dt        Input \code{data.table}
#' @param col_names Column names to leave
#' @param modify    Logical, whether to modify dt or return copy
#'
#' @return Modified dt (if \code{modify == TRUE}) or copy of dt with removed columns
#' @export
#'
#' @examples
leave_cols <- function(dt, col_names, modify = FALSE, print = TRUE) {
  drop_names <- setdiff(names(dt), col_names)
  drop_cols(dt, drop_names, modify = FALSE)
}

#'
#' Drop columns in \code{data.table} by reference or returning a copy.
#'
#' @param dt     \code{data.table} to process
#' @param cols   Column names to drop, vector of strings
#' @param modify Boolean, indicating whether columns should be dropped by reference of a copy should be returned
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#' dt <- data.table(V1 = 1, V2 = 2, V3 = 3)
#' print(dt)
#' dt_dropped <- drop_cols(dt, c("V1", "V2"), modify = FALSE)
#' print(dt)
#' print(dt_dropped)
#' drop_cols(dt, c("V1", "V2"), modify = TRUE)
#' print(dt)
drop_cols <- function(dt, cols, modify = FALSE) {
  # if (modify == TRUE) stop("modify = TRUE not implemented")
  cols_to_drop <- intersect(names(dt), cols)
  if (length(cols_to_drop) == 0) {
    warning('No columns to drop.')
    return(dt)
  } else {
    if (!modify) {
      return(dt[, .SD, .SDcols = !cols_to_drop])
    } else {
      return(dt[, (cols_to_drop) := NULL])
    }
  }
}

# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# .datatable.aware = TRUE
# Not needed:
# http://stackoverflow.com/questions/15223367/
# wrapping-data-table-using-an-evaluated-call-in-a-package

#' Fills specified values (def. empty strings) in data.table with an NA.
#'
#' @param data_in
#' @param values_to_fill
#' @param cols_to_fill
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
fill_values_with_NA <- function(data_in, values_to_fill = '', cols_to_fill = names(data_in)) {
  # TODO: Write tests
  # TODO: Split to different column classes (bool, numeric, character etc.)
  data_in[, (cols_to_fill) := lapply(
    .SD, function(col) { ifelse(col %in% values_to_fill, NA, col) }),
    .SDcols = cols_to_fill]
}

#' Fills missing values in data.table with a specified constant value.
#'
#' @param data_in
#' @param const_val
#' @param cols_to_fill
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
fill_NAs_with_const <- function(data_in, const_val = 0, cols_to_fill = names(data_in)) {
  # TODO: Write tests
  # TODO: Split to different column classes (bool, numeric, character etc.)
  message('Function currently working with numeric columns only.')
  data_in[, (cols_to_fill) := lapply(
    .SD, function(col) { ifelse(is.na(col), const_val, col) }),
    .SDcols = cols_to_fill]
}

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

#' Prints count(s) of missing values
#'
#' Prints count(s) of missing values (coded as \code{NA}) in an object
#' (\code{vector}, \code{data.frame}, \code{data.table} etc.).
#'
#' @param data Input object.
#' @param prop Flag indicating if results should include proportions.
#' @param print Flag indicating if results should be printed.
#'
#' @return Table with counts (and proportions if TRUE) of missing values.
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

#' Prints count(s) of specified values
#'
#' Prints count(s) of specified values in a \code{data.table} object.
#'
#' @param dt_in
#' @param value
#' @param prop
#' @param na.rm
#' @param print
#'
#' @return Table with counts (and proportions if TRUE) of specified values.
#' @export
#'
#' @examples
#' dt <- data.table::data.table(a = rnorm(n=1000, mean=20, sd=5),
#'                              b = rnorm(n=1000, mean=20, sd=5),
#'                              c = rnorm(n=1000, mean=20, sd=5))
#' dt[sample.int(nrow(dt), round(nrow(dt) * 0.15)), a := -Inf]
#' count_values(dt)
#' count_values(dt, prop = TRUE)
#' count_values(dt, -Inf)
#' count_values(dt, -Inf, prop = TRUE)
#'
#' TODO: Write tests
count_values <- function(dt_in, value = '', prop = FALSE, na.rm = TRUE, print = TRUE) {
  if(!data.table::is.data.table(dt_in)) error('Function currently working only on data.tables')
  NAs_tab <- data.table::data.table(
    Variable = names(dt_in),
    Count = colSums(dt_in[, lapply(.SD, function(col) ifelse(col == value,1L,0L))], na.rm = na.rm))
  if(prop) NAs_tab[, Prop := round(Count/nrow(dt_in)*100, 2)]
  data.table::setorder(NAs_tab, -Count, Variable)
  if (print) print(NAs_tab)
  else return(NAs_tab)
}
# Same with direct sapply
# count_values <- function(dt_in, value = '', prop = FALSE, na.rm = TRUE) {
#   sapply(dt_in, function(col) { sum(ifelse(col == value,1L,0L), na.rm = na.rm) }) }

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

#' Copies data from clipboard (e.g. Excel)
#'
#' @param header \code{Logical}, indicates if the data have a header row
#'
#' @return \code{data.table} with pasted data
#' @export
#' @import data.table
#'
#' @examples
copy_from_clipboard <- function(header = TRUE) {
  read.table("clipboard", header = header, stringsAsFactors = FALSE) %>%
    data.table()
}