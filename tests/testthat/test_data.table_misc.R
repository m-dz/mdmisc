
context("Test data.table_misc.R functions from mdmisc package")

library(testthat)
library(mdmisc)
library(data.table)

test_that('sample_rows is working and expected', {
  # set.seed(2016)
  size <- 20
  dt <- data.table(
    id = 1:size, A = sample(letters[1:3], size, replace = TRUE),
    B = sample(c('Y','N'), size, replace = TRUE),
    C = sample(1:100, size, replace = TRUE) + sample(30:70, size, replace = TRUE))

  ### Parameters checks
  # sample_rows(dt, vol = 1000)
  expect_error(sample_rows(dt, pct = 50), regexp = 'Please specify pct as a fraction')
  expect_error(sample_rows(dt, vol = 1000, pct = 0.1), regexp = 'Please specify either pct or vol.')
  expect_error(sample_rows(dt), regexp = 'Please specify either pct or vol.')

  ### Volumes (should NOT fail even without seeds)
  # set.seed(2016)
  dt_sampled <- sample_rows(dt, 0.5, grouping = 'A')
  expect_equal(dt[, round(table(A)/2)], dt_sampled[, table(A)])
  # set.seed(2016)
  dt_sampled <- sample_rows(dt, 0.5, grouping = 'A', sort = TRUE)
  expect_equal(dt[, round(table(A)/2)], dt_sampled[, table(A)])
  expect_equal(dt_sampled[, A], sort(dt_sampled[, A]))
  # set.seed(2016)
  dt_sampled <- sample_rows(dt, 0.5, grouping = c('A','B'), sort = TRUE)
  expect_true(all(between(dt[, round(table(A, B)/2)] - dt_sampled[, table(A, B)], -1, 1)))
})

test_that('leave_cols is working and expected', {
  require(data.table)
  dt <- data.table(a = 1:3, b = 1:3, g = 1:3)
  expect_equal(leave_cols(dt, c('a', 'b')), dt[, .(a, b)])
  expect_equal(leave_cols(dt, c('a')), dt[, .(a)])
})

test_that("reorder_vec is working as expected", {
  df <- data.frame(a = 1:3, b = 1:3, g = 1:3)
  expect_equal(reorder_vec(c("a", "b", "g"), "g first"), c("g", "a", "b"))
  expect_equal(reorder_vec(names(df), "g first; a last"), c("g", "b", "a"))
})

test_that("reorder_columns is working as expected", {
  require(data.table)
  dt <- data.table::data.table(a = 1:3, b = 1:3, g = 1:3)
  expect_equal(reorder_columns(dt, "g first"),  dt[, .(g, a, b)])
  expect_equal(reorder_columns(dt, "g first; a last"), dt[, .(g, b, a)])
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

test_that("table_data_table is working as expected", {
  require(data.table)
  set.seed(2016)
  dt <- data.table(A = sample(LETTERS[1:10], 200, replace = TRUE),
                   B = sample(LETTERS[1:10], 200, replace = TRUE))
  expected_A <- data.table(
    A = c("B", "D", "G", "A", "F", "C", "I", "E", "H", "J"),
    N = c(26L, 25L, 24L, 23L, 20L, 19L, 18L, 17L, 15L, 13L),
    PCT = c(13, 12.5, 12, 11.5, 10, 9.5, 9, 8.5, 7.5, 6.5))
  expected_B <- data.table(
    B = c("G", "I", "H", "F", "B", "A", "J", "E", "C", "D"),
    N = c(28L, 22L, 21L, 21L, 21L, 20L, 19L, 19L, 17L, 12L),
    PCT = c(14, 11, 10.5, 10.5, 10.5, 10, 9.5, 9.5, 8.5, 6))
  expected_AB <- data.table(
    A = c("B", "B", "G", "C", "H", "D", "D", "G",
          "D", "G", "D", "F", "A", "F", "E", "I", "A", "C", "F", "A", "F",
          "E", "B", "E", "J", "I", "B", "I", "A", "I", "D", "C", "H", "B",
          "G", "A", "I", "D", "I", "G", "A", "C", "H", "B", "F", "E", "J",
          "D", "B", "E", "G", "H", "G", "J", "A", "E", "B", "F", "A", "J",
          "E", "I", "C", "J", "J", "G", "C", "C", "C", "A", "J", "G", "C",
          "D", "H", "F", "A", "I", "H", "G", "D", "B", "F", "H", "C", "J"
    ),
    B = c("G", "H", "B", "G", "E", "I", "C", "F", "J", "G", "B",
          "H", "A", "G", "B", "G", "G", "J", "I", "F", "C", "C", "A", "I",
          "F", "F", "J", "E", "E", "H", "A", "I", "I", "F", "J", "I", "I",
          "F", "J", "A", "H", "F", "A", "E", "A", "D", "G", "E", "D", "A",
          "H", "F", "C", "E", "B", "J", "B", "D", "D", "B", "H", "A", "C",
          "D", "J", "E", "H", "B", "E", "C", "C", "D", "A", "G", "H", "B",
          "J", "C", "B", "I", "H", "C", "J", "D", "D", "I"),
    N = c(6L, 5L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L,
          3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L),
    PCT = c(3, 2.5, 2.5, 2.5, 2.5, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5,
            1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.5))
  expect_equal(table_data_table(dt, "A"), expected_A)
  expect_equal(table_data_table(dt, "B"), expected_B)
  expect_equal(table_data_table(dt, c("A", "B")), expected_AB)
})
