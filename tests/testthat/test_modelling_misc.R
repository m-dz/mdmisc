
context('Test modelling_misc.R functions from mdmisc package')

test_that('gini_norm is equal 2 * AUC - 1', {
  df <- data.frame(
   actual = c(1,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0),
   predicted = seq(1, 0, length.out = 40)
  )
  gini_val <- gini_norm(df[, 'actual'], df[, 'predicted'])
  AUC_val <- pROC::roc(df[, 'actual'], df[, 'predicted'])$auc
  expect_equal(gini_val, 2 * AUC_val - 1)
})