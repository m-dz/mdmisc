
context("Test data.table_misc.R functions from mdmisc package")

test_that("move_vec is working as expected", {
  df <- data.frame(a = 1:3, b = 1:3, g = 1:3)
  expect_equal(move_vec(c("a", "b", "g"), "g first"), c("g", "a", "b"))
  expect_equal(move_vec(names(df), "g first; a last"), c("g", "b", "a"))
})

test_that("move_columns is working as expected", {
  require(data.table)
  dt <- data.table::data.table(a = 1:3, b = 1:3, g = 1:3)
  expect_equal(move_columns(dt, "g first"),  dt[, .(g, a, b)])
  expect_equal(move_columns(dt, "g first; a last"), dt[, .(g, b, a)])
})