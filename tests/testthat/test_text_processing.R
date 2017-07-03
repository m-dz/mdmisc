
context("Test text_processing.R functions from mdmisc package")

test_that("str_to_Capital is working as expected", {
  expect_equal(mdmisc:::str_to_Capital('test'), 'Test')
})

test_that("str_to_CamelCase is working as expected", {
  t1 <- c('this text', 'next text')
  t2 <- c('this.text', 'next.text')
  t3 <- 'this'
  t4 <- 'this text'

  expect_equal(str_to_CamelCase(t1), c('ThisText', 'NextText'))
  expect_equal(str_to_CamelCase(t1, collapse = ' '), c('This Text', 'Next Text'))
  expect_equal(str_to_CamelCase(t2, split = '.', collapse = ' '), c('This Text', 'Next Text'))

  expect_equal(str_to_CamelCase(t3), 'This')
  expect_equal(str_to_CamelCase(t4), 'ThisText')
})
