
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

test_that("count_NAs is working as expected", {
dt <- data.table::data.table(a = rnorm(n=1000, mean=20, sd=5),
                             b = rnorm(n=1000, mean=20, sd=5),
                             c = rnorm(n=1000, mean=20, sd=5))
dt[sample.int(nrow(dt), round(nrow(dt) * 0.15)), a := NA_real_]

expected_1 <- data.table::data.table(Variable = c("a", "b", "c"),
                                     Count = c(150, 0, 0))
expected_2 <- data.table::data.table(Variable = c("a", "b", "c"),
                                     Count = c(150, 0, 0),
                                     Prop = c(15, 0, 0))
expected_3 <- data.frame(Count = 150, row.names = "Variable")
expected_4 <- data.frame(Count = 150, Prop = 15, row.names = "Variable")
expected_5 <- data.table::data.table(Variable = "a", Count = 150)
expected_6 <- data.table::data.table(Variable = "a", Count = 150, Prop = 15)

expect_equal(count_NAs(dt, print = FALSE), expected_1)
expect_equal(count_NAs(dt, prop = TRUE, print = FALSE), expected_2)
expect_equal(count_NAs(dt[, a], print = FALSE), expected_3)
expect_equal(count_NAs(dt[, a], prop = TRUE, print = FALSE), expected_4)
expect_equal(count_NAs(dt[, .(a)], print = FALSE), expected_5)
expect_equal(count_NAs(dt[, .(a)], prop = TRUE, print = FALSE), expected_6)
})
