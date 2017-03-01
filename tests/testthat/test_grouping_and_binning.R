
context("Test grouping_and_binning.R functions from mdmisc package")

test_that("group_by_threshold parameters checks", {
  # Load packages and generate data
  set.seed(2016)
  dt <- data.table::data.table('cat' = sample(letters[1:10], 20, replace = TRUE), 'cont' = rnorm(20))

  expect_error(group_by_threshold(dt, 'cat', threshold = 0.2, cum_threshold = 0.2, no_of_categories = NULL))
  expect_error(group_by_threshold(dt, 'cat', threshold = 0.2, cum_threshold = 0.2, no_of_categories = 10))
  expect_error(group_by_threshold(dt, 'cont'))
  expect_error(group_by_threshold(dt, 'cont', threshold = 0.2, cum_threshold = NULL, no_of_categories = NULL))
})

test_that("cumulative frequencies and cuts are calculated correctly", {
  # Load packages and generate data
  set.seed(2016)
  dt <- data.table::data.table('cat' = sample(letters[1:10], 20, replace = TRUE), 'cont' = rnorm(20))

  expect_equal(
    group_by_threshold(dt, 'cat')[['PCT']],
    c(0.25, 0.15, 0.15, 0.15, 0.10, 0.10, 0.05, 0.05))
  expect_equal(
    group_by_threshold(dt, 'cat')[['CumPCT']],
    c(0.25, 0.40, 0.55, 0.70, 0.80, 0.90, 0.95, 1.00))
  expect_equal(
    group_by_threshold(dt, 'cat', threshold = 0.1)[['ToGroup']],
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(
    group_by_threshold(dt, 'cat', cum_threshold = 0.3)[['ToGroup']],
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(
    group_by_threshold(dt, 'cat', no_of_categories = 5)[['ToGroup']],
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that("data are modified correctly", {
  # Load packages and generate data
  set.seed(2016)
  dt <- data.table::data.table('cat' = sample(letters[1:10], 20, replace = TRUE), 'cont' = rnorm(20))

  expect_error(group_by_threshold(dt, 'cat', return_data = TRUE))
  # expect_equal(
  #   group_by_threshold(dt, 'cat')[['CumPCT']],
  #   c(0.25, 0.40, 0.55, 0.70, 0.80, 0.90, 0.95, 1.00))
  # expect_equal(
  #   group_by_threshold(dt, 'cat', threshold = 0.1)[['ToGroup']],
  #   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  # expect_equal(
  #   group_by_threshold(dt, 'cat', cum_threshold = 0.3)[['ToGroup']],
  #   c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  # expect_equal(
  #   group_by_threshold(dt, 'cat', no_of_categories = 5)[['ToGroup']],
  #   c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
})