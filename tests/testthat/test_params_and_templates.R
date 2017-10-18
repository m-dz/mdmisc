
context('Test params_and_templates.R functions from mdmisc package')

library(testthat)
library(mdmisc)
library(data.table)

test_that('substitute_params is working as expected', {
  dt_params <- data.table(
    PARAM_FILE_NAME_DATE = c('201710_AAA', '201710_BBB', 201711),
    PARAM_1 = c('XYZ', 'ZYX', 'AAA'),
    PARAM_2 = c('QWERTY', 'YTREWQ', 'BBB'),
    COMMENTS = c('ignore', '', 'ignore_2')
  )
  templates <- c('abcd PARAM_1 abcd', 'abcdPARAM_2abcd')
  temp_dir <- tempdir()
  lapply(seq_along(templates), FUN = function(template_id) {
    con_out <- file(file.path(temp_dir, paste0('test_template_YYYYMM_', template_id, '.csv')), 'w', blocking = TRUE)
    writeLines(templates[template_id], con_out)
    close(con_out)
  })
  template_files <- list.files(path = temp_dir, pattern = 'csv$')
  substitute_params(params_table = dt_params, template_files = template_files, target_dir = temp_dir)

  ## Cleanup template files
  lapply(seq_along(templates), FUN = function(template_id) {
    file.remove(file.path(temp_dir, paste0('test_template_YYYYMM_', template_id, '.csv')))
  })

  # testthat::expect_equal(dt, dt_res)
  ## Cleanup created files
  for(param_row_id in seq_len(nrow(dt_params))) {
    new_dir <- file.path(dirname(temp_dir), paste0(dt_params[param_row_id, PARAM_FILE_NAME_DATE]))
    unlink(paste0(new_dir, '_NOT_RUN'), recursive = TRUE)
  }
})
