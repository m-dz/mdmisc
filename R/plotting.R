
#' Plots binary outcome
#'
#' Plots binary outcome, use for binary models outcomes etc.
#'
#' @param df              Data
#' @param pred_name       Predictions' column name
#' @param actual_name     Actual values' column name
#' @param x_lab           X-axis label
#' @param y_lab           Y-axis label
#' @param theme           Theme function to use
#' @param type            Plot type, \code{type = c('pipes', 'jitter')}
#' @param smooth          Logical, whether plot smoothing line or not
#' @param smoothing_level If \code{smooth == TRUE} the smoothing level, otherwise ignored
#'
#' @return Plot object
#' @export
#'
#' @examples
#' df <- data.frame(
#'   actual = c(1,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0),
#'   predicted = seq(1, 0, length.out = 40)
#' )
#' plot_binary_outcome(df, 'predicted', 'actual')
#' plot_binary_outcome(df, 'predicted', 'actual', smoothing_level = 0.75)
#' plot_binary_outcome(df, 'predicted', 'actual', x_lab = 'Predicted', y_lab = 'Actual')
#' plot_binary_outcome(df, 'predicted', 'actual', theme = theme_bw())
#' plot_binary_outcome(df, 'predicted', 'actual', type = 'pipes', theme = theme_bw())
#' plot_binary_outcome(df, 'predicted', 'actual', type = 'jitter', theme = theme_bw())
plot_binary_outcome <- function(
  df,
  pred_name,
  actual_name,
  x_lab = NULL,
  y_lab = NULL,
  theme = NULL,
  type = 'pipes',
  smooth = TRUE,
  smoothing_level = NA)
{
  if(is.null(x_lab)) x_lab <- pred_name
  if(is.null(y_lab)) y_lab <- actual_name
  g <- ggplot2::ggplot(df, aes_string(x = pred_name, y = actual_name)) +
    xlab(x_lab) +
    ylab(y_lab) +
    coord_cartesian(ylim = c(0, 1))
  # Add point markers
  g <- g + switch(
    type,
    pipes = geom_point(size = 4, alpha = 1, shape = 124),
    jitter = geom_jitter(size = 2, alpha = 0.4, position = position_jitter(height = .02)))
  # Smoothing and theme
  if(mdmisc::is.not.null(smooth)) g <- g + stat_smooth(method = "loess", colour = "blue", size = 0.5, level = smoothing_level)
  if(mdmisc::is.not.null(theme)) g <- g + theme
  return(g)
}

#' Plots lift chart using binary target and model predictions
#'
#' @param target
#' @param predictions
#' @param breaks_no
#' @param xlabel
#' @param ylabel
#' @param title
#' @param file_name
#' @param plot_volumes
#' @param show_plot
#' @param save_to_file
#' @param return_plot
#'
#' @return
#' @export
#' @import data.table
#' @import magrittr
#'
#' @examples
plot_lift_chart <- function(
  target,
  predictions,
  breaks_no = 4,
  cut_by_volume = FALSE,
  xlabel = "Predicted response",
  ylabel = "Actual conversion ratio",
  title = "Lift chart",
  file_name = "lift_chart.svg",
  plot_volumes = FALSE,
  show_plot = TRUE,
  save_to_file = FALSE,
  return_plot = FALSE)
{
  tab <- data.table(
    Sale = target,
    Pred = predictions,
    PredBinned = if(cut_by_volume) cut(predictions, breaks = breaks_no) else Hmisc::cut2(predictions, g = breaks_no, levels.mean = TRUE)
  ) %>% setnames(c("Sale", "Pred", "PredBinned"))
  tab_summary <- tab[, list(ConvPerBin = sum(Sale)/.N*100, Count = .N), by = PredBinned]

  if (plot_volumes) {
    dummyTable1 <- tab_summary[, .(PredBinned, y = ConvPerBin)] %>% .[, DummyPanel := "1. Conversion [%]"]
    dummyTable2 <- tab_summary[, .(PredBinned, y = Count)] %>% .[, DummyPanel := "2. Count"]
    dummyTable <- rbind(dummyTable1, dummyTable2)

    # ggplot2::ggplot(tab_summary) + geom_bar(aes(PredBinned, ConvPerBin), stat = "identity") + theme_light()
    g <- ggplot2::ggplot(dummyTable) +
      facet_grid(DummyPanel ~ ., scales = "free_y") +
      geom_bar(aes(PredBinned, y), stat = "identity", position = "identity") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      # scale_fill_gradient(low = "#00abff", high = "#003f5e") +
      labs(x = xlabel, title = title) +
      theme(axis.title.y = element_blank()) +
      theme(panel.grid.major.x = element_blank())
  } else {
    g <- ggplot2::ggplot(tab_summary) +
      geom_bar(aes(PredBinned, ConvPerBin), stat = "identity") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      # scale_fill_gradient(low = "#00abff", high = "#003f5e") +
      labs(x = xlabel, y = ylabel, title = title) +
      theme(panel.grid.major.x = element_blank())
  }
  if (length(unique(tab_summary[, PredBinned])) > 35) g <- g + scale_x_discrete(breaks = NULL)
  if (show_plot) print(g)
  if (save_to_file) ggsave(paste0(file_name), g, width = 297, height = 210, units = "mm")
  if (return_plot) return(g)
}


#
#' Multiple plot function
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow = 2, byrow = TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' @param ...      \code{ggplot} objects plotlist (use this or \code{plotlist})
#' @param plotlist a \code{list} of \code{ggplot} objects
#' @param file
#' @param cols     Number of columns in the layout
#' @param layout   A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' @return Nothing (?)
#' @export
#'
#' @examples
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}
