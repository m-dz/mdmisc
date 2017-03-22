
#' Clean memory by running \code{gc()} n times.
#'
#' @param n Defaults to 10.
#'
#' @return
#' @export
clean_memory <- function(n = 10) { for (i in 1:n) gc() }

#' Setup proxy settings
#'
#' @param url      Proxy url
#' @param port     Proxy port
#' @param username Proxy username
#' @param password Proxy password
#'
#' @return \code{TRUE} if successful.
#' @export
#' @importFrom httr reset_config set_config use_proxy
#' @importFrom getPass getPass
#'
#' @examples
#' setup_proxy(url = "1.1.1.1", port = 1111, username = "domain//username")
#' ## Not run: file template
#' ##       url, port,           username
#' ## "1.1.1.1", 1111, "domain//username"
setup_proxy <- function(file_path = NA_character_, url = NULL, port = NULL, username = NULL, password = getPass::getPass()) {
  if(!file.exists(file_path) & (is.null(url) | is.null(port) | is.null(username))) stop('Please provide a valid file_path or proxy settings.')
  if(file.exists(file_path)) {
    setup_values <- read.csv(file_path, stringsAsFactors = FALSE)
    if(is.null(url))      url <- setup_values[1, 'url']
    if(is.null(port))     port <- setup_values[1, 'port']
    if(is.null(username)) username <- setup_values[1, 'username']
  }
  if(is.null(url) | is.null(port) | is.null(username)) {
    missing_args <- c('url', 'port', 'username')[c(is.null(url), is.null(port), is.null(username))]
    stop(paste0('Arguments: ', paste(missing_args, collapse = ', '), ' missing.'))
  }
  message(paste0('Setting up proxy with the following parameters: url = ', url, ', port = ', port, ', username = ', username))
  reset_config()
  set_config(
    use_proxy(url, port, username, password)
  )
  return(TRUE)
}

#' Remove all user defined variables except functions
#'
#' @param ask \code{logical} indicating whether user confirmation is needed.
#'
#' @return None (invisible NULL).
#' @export
#'
#' @examples
#' rm_all_exc_func()  # Will ask for confirmation.
rm_all_exc_func <- function(ask = TRUE) {
  yn <- TRUE
  if(ask) {
    user_input <- yes_no_input('Removing all user defined variables except functions, continue?')
    yn <- is.yes(user_input)
  }
  if(yn) rm(
    list = setdiff(
      ls(envir = parent.frame()),
      lsf.str(envir = parent.frame())),
    envir = parent.frame()) else message('Quitting without removal.')
}

#' Simple wrapper for \code{pacman::p_load}
#'
#' Function name matches the first characters of \code{library}
#'
#' @param package
#'
#' @return None (invisible NULL).
#' @export
#' @importFrom pacman p_load
#'
#' @examples
#' lib_pacman(ggplot2)
#' lib_pacman('ggplot2')
lib_pacman <- function(package) {
  p_load(char = as.character(substitute(package)))
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
#' @param x Input argument
#'
#' @return \code{Boolean} indicating whether x is not NA
#' @export
#'
#' @examples
is.not.na <- function(x) { !is.na(x) }

#' Checks for an argument not being NULL
#'
#' @param x Input argument
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
#' @param first  Input argument
#' @param second Input argument
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

#' Lists CSV files in the specified directory
#'
#' @param path       Path to directory
#' @param full_names \code{logical}, passed to \code{full.names} of \code{list.files}
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
#' Warning: Conversion to \code{data.frame} tested only from \code{matrix}
#'
#' @param dt        Data to save
#' @param file_name File name
#' @param dump_dir  Folder directory
#' @param ...       Arguments passed to \code{data.table::fwrite} function
#'
#' @return
#' @export
#' @importFrom data.table fwrite
#'
#' @examples
#' x <- data.frame(A = c(1:3), B = letters[1:3])
#' dump_to_csv(x)
#' dump_to_csv(data.frame(A = c(1:3), B = letters[1:3]))
#' dump_to_csv(data.frame(A = c(1:3), B = letters[1:3]), file_name = 'a.csv')
#' # Conversion to data.frame:
#' dump_to_csv(as.matrix(c(1:4)))
#' # Errors:
#' dump_to_csv(x, file_name = ':a.csv')
#' dump_to_csv(data.frame(A = c(1:3), B = letters[1:3]), file_name = ':a.csv')
#' # Not run: other scenarios not tested!
dump_to_csv <- function(dt, file_name = NULL, dump_dir = NULL, ...) {
  if(is.null(file_name)) {
    obj_name <- paste0(deparse(substitute(dt)))
    if(exists(obj_name)) file_name <- paste0(obj_name, '.csv') else file_name <- 'temp.csv'
  }
  if(is.null(dump_dir)) dump_dir <- file.path(path.expand('~'), 'Desktop/temp_file_dump')
  if(!is.data.frame(dt)) {
    warning('Object is not a valid fwrite input, converting to a data.frame')
    dt <- as.data.frame(dt)
  }
  tryCatch(
    fwrite(dt, file = file.path(dump_dir, file_name), ...),
    warning = function(w) warning(w), error = function(e) stop(e)
  ) # END tryCatch
}
