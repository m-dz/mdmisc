
context("Test outliers.R functions from mdmisc package")

test_that("mark_outliers_IQR is working as expected", {
  expected <- c(5, 123)

  set.seed(2016)
  x <- rnorm(200, 0, 1)
  test_val <- which(mark_outliers_IQR(x, quants = c(0.25, 0.75)))
  expect_equal(test_val, expected)

  set.seed(2016)
  df <- data.frame(x = rnorm(200, 0, 1))
  test_val <- which(
    mark_outliers_IQR(df[, "x", drop = FALSE], quants = c(0.25, 0.75)))
  expect_equal(test_val, expected)
})

test_that("mark_outliers_quant is working as expected", {
  expected <- c(5, 53, 108, 123)

  set.seed(2016)
  x <- rnorm(200, 0, 1)
  test_val <- which(mark_outliers_quant(x, quants = c(0.01, 0.99)))
  expect_equal(test_val, expected)

  set.seed(2016)
  df <- data.frame(x = rnorm(200, 0, 1))
  test_val <- which(
    mark_outliers_quant(df[, "x", drop = FALSE], quants = c(0.01, 0.99)))
  expect_equal(test_val, expected)
})

test_that("mark_outliers_DBSCAN is working as expected", {
  expected <- c(5, 53, 99, 108, 112, 123)

  set.seed(2016)
  x <- rnorm(200, 0, 1)
  test_val <- which(mark_outliers_DBSCAN(x, quant = 0.15))
  expect_equal(test_val, expected)

  set.seed(2016)
  df <- data.frame(x = rnorm(200, 0, 1))
  test_val <- which(
    mark_outliers_DBSCAN(df[, "x", drop = FALSE], quant = 0.15))
  expect_equal(test_val, expected)
})
