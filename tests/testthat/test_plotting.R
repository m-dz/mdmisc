
context('Test plotting.R functions from mdmisc package')

test_that('plot_binary_outcome is working as expected', {
  df <- data.frame(
    actual = c(1,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0),
    predicted = seq(1, 0, length.out = 40)
  )

  p1 <- plot_binary_outcome(df, 'predicted', 'actual')
  p2 <- plot_binary_outcome(df, 'predicted', 'actual', smoothing_level = 0.75)
  p3 <- plot_binary_outcome(df, 'predicted', 'actual', x_lab = 'Predicted', y_lab = 'Actual')
  p4 <- plot_binary_outcome(df, 'predicted', 'actual', theme = theme_bw())
  p5 <- plot_binary_outcome(df, 'predicted', 'actual', type = 'pipes', theme = theme_bw())
  p6 <- plot_binary_outcome(df, 'predicted', 'actual', type = 'jitter', theme = theme_bw())

  expect_is(p1, class = c('gg', 'ggplot'))
  expect_is(p2, class = c('gg', 'ggplot'))
  expect_is(p3, class = c('gg', 'ggplot'))
  expect_is(p4, class = c('gg', 'ggplot'))
  expect_is(p5, class = c('gg', 'ggplot'))
  expect_is(p6, class = c('gg', 'ggplot'))
})

test_that('plot_binary_outcome throws an error when column not in data', {
  df <- data.frame(
    actual = c(1,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0),
    predicted = seq(1, 0, length.out = 40)
  )

  expect_error(
    plot(plot_binary_outcome(df, 'predicted', 'not_in_df')),  # Creates Rplot.pdf, same for print()
    regexp = "object 'not_in_df' not found"
  )
})
