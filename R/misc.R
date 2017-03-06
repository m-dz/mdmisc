
#' Setup proxy settings
#'
#' @param url      Proxy url
#' @param port     Proxy port
#' @param username Proxy username
#' @param password Proxy password
#'
#' @return \code{TRUE}
#' @export
#' @importFrom httr reset_config set_config use_proxy
#' @importFrom getPass getPass
#'
#' @examples
#' mdmisc::setup_proxy() # Runs with default file_path
setup_proxy <- function(file_path = 'F:/Marketing/Group Marketing/Database Marketing/Marketing Analytics/Personal/Mateusz/00 DandG proxy settings/proxy_settings.csv',
                        url = NULL, port = NULL, username = NULL, password = getPass::getPass()) {
  if(!file.exists(file_path) & (is.null(url) | is.null(port) | is.null(username))) stop('Please provide a valid file_path or proxy settings.')
  if(file.exists(file_path)) {
    setup_values <- read.csv(file_path, stringsAsFactors = FALSE)
    url = setup_values[1, 'url']
    port = setup_values[1, 'port']
    username = setup_values[1, 'username']
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
