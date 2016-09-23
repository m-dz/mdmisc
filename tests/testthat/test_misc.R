
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
