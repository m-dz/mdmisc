
#' Title
#'
#' @param params_table   Table (\code{data.frame} or \code{data.table}) with parameters to substitute.
#' @param template_files List of template files to substitute the params in.
#' @param target_dir     Directory where to create files, defaults to active file directory.
#' @param cols_to_ignore Columns from the parameters table \code{params_table} to ignore when substituting, defaults to \code{c('PARAM_FILE_NAME_DATE', 'COMMENTS')}.
#'
#' @return
#' @export
#' @import data.table
#' @import magrittr
#'
#' @examples
substitute_params <- function(params_table, template_files, target_dir = get_active_file_path(), cols_to_ignore = c('PARAM_FILE_NAME_DATE', 'COMMENTS')) {
  params_names <- names(params_table) %>% setdiff(cols_to_ignore)
  for(param_row_id in seq_len(nrow(params_table))) {
    param_row <- params_table[param_row_id, ]
    ## New dir to create
    new_dir <- file.path(dirname(target_dir), paste0(param_row[1, PARAM_FILE_NAME_DATE]))
    ## Proceed only if target_dir does not exist
    # if(!dir.exists(new_dir)) {
      new_dirnot_run <- paste0(new_dir, '_NOT_RUN')
      dir.create(new_dirnot_run)
      ## Parse all templates
      for(template in template_files) {
        ## Read in
        con_in <- file(file.path(target_dir, template), 'r', blocking = TRUE)
        query_content <- readLines(con_in)
        ## Replace all params
        for(param in params_names) {
          # print(param)
          query_content <- stringr::str_replace(query_content, param, unname(unlist(param_row[1, ..param])))
        }
        ## Replace _YYYYMM_ in template name with PARAM_FILE_NAME_DATE
        new_query_name <- stringr::str_replace(template, '_YYYYMM_', paste0('_', param_row[1, PARAM_FILE_NAME_DATE], '_'))
        con_out <- file(file.path(new_dirnot_run, new_query_name), 'w', blocking = TRUE)
        writeLines(query_content, con_out)
        close(con_in)
        close(con_out)
      }
    # }
  }
}
