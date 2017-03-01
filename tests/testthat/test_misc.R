
context("Test misc.R functions from mdmisc package")

# getActiveFilePath cannot be tested because of error:
# 1: mdmisc::getActiveFilePath() at C:\Users\dziedzm\Local_Projects\packages\mdmisc/tests/testthat/test_misc.R:5
# 2: dirname(rstudioapi::getActiveDocumentContext()$path) at C:\Users\dziedzm\Local_Projects\packages\mdmisc/R/misc.R:10
# 3: rstudioapi::getActiveDocumentContext()
# 4: callFun("getActiveDocumentContext")
# 5: verifyAvailable()
# 6: stop("RStudio not running", call. = FALSE)
#
# test_that("getActiveFilePath is working as expected", {
#   test_val <- mdmisc::getActiveFilePath()
#   expected <- dirname(rstudioapi::getActiveDocumentContext()$path)
#   expect_equal(test_val, expected
#   )
# })

test_that("sorted_names is working as expected", {
  df <- data.frame(b = 1, a = 2)
  test_val <- mdmisc::sorted_names(df)
  expected <- c("a", "b")
  expect_equal(test_val, expected)
})

test_that("setdiff_two_way is working as expected", {
  test_diff <- mdmisc::setdiff_two_way(
    letters[1:10],
    letters[2:10]
  )
  expected_first <- c("a")
  expected_second <- character()
  expect_equal(test_diff$First, expected_first)
  expect_equal(test_diff$Second, expected_second)
})
