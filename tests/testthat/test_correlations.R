
context('Test correlations.R functions from mdmisc package')

test_that('remove_single_value_cols is working as expected', {
  library(data.table)
  set.seed(2017)
  dt <- data.table(V1 = rep(1, 5), V2 = letters[1:5], V3 = runif(5))
  dt_res <- dt[, .(V2, V3)]
  remove_single_value_cols(dt)
  testthat::expect_equal(dt, dt_res)
})
