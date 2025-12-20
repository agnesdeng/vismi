# overimp1D_qq ------------------------------------------------------------
overimp1D_qq <- function(long_data, x, comb_title, point_size, ylim) {
  train_plot <- ggplot(long_data$train$long_df, aes(sample = .data[[x]], color = Group)) +
    stat_qq(size = point_size) +
    scale_color_manual(values = long_data$train$colors) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(long_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(long_data$test$long_df, aes(sample = .data[[x]], color = Group)) +
      stat_qq(size = point_size) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = long_data$test$colors) +
      labs(title = "Test Data")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(ylim)) test_plot <- test_plot + ylim(ylim)


    combined <- gridExtra::arrangeGrob(train_plot, test_plot, ncol = 2,
                                       top = comb_title)

  } else {
    combined <- gridExtra::arrangeGrob(train_plot, ncol = 1,
                                       top = comb_title)
  }

  class(combined)<- c("overimp_plot",class(combined))
  combined
}

#overimp1D_qqline --------------------------------------------------------
overimp1D_qqline <- function(long_data, x, comb_title,  linewidth, ylim) {
  train_plot <- ggplot(long_data$train$long_df, aes(sample = .data[[x]], color = Group)) +
    stat_qq_line(linewidth = linewidth) +
    scale_color_manual(values = long_data$train$colors) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(long_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(long_data$test$long_df, aes(sample = .data[[x]], color = Group)) +
      stat_qq_line(linewidth=linewidth) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = long_data$test$colors) +
      labs(title = "Test Data")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- gridExtra::arrangeGrob(train_plot, test_plot, ncol = 2,
                                       top = comb_title)
  } else {
    combined <- gridExtra::arrangeGrob(train_plot, ncol = 1,
                                       top = comb_title)
  }

  class(combined)<- c("overimp_plot",class(combined))
  combined
}




# #overimp1D_ridge --------------------------------------------------------

overimp1D_ridge<-function(long_data, x, comb_title, alpha, ylim){
  train_plot <- ggplot(long_data$train$long_df, aes(x = .data[[x]])) +
    geom_density_ridges(alpha = alpha, aes(y = Group, fill = Group)) +
    scale_fill_manual(values = long_data$train$colors, guide = guide_legend(reverse = TRUE)) +
    labs(title = "Training Data")+
    guides(fill = "none")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(long_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(long_data$test$long_df, aes(x = .data[[x]])) +
      geom_density_ridges(alpha = alpha, aes(y = Group, fill = Group)) +
      coord_cartesian(xlim = xrange) +
      scale_fill_manual(values = long_data$test$colors, guide = guide_legend(reverse = TRUE)) +
      labs(title = "Test Data")+
      guides(fill = "none")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- gridExtra::arrangeGrob(train_plot, test_plot, ncol = 2,
                                       top = comb_title)
  } else {
    combined <- gridExtra::arrangeGrob(train_plot, ncol = 1,
                                       top = comb_title)
  }

  class(combined)<- c("overimp_plot",class(combined))
  combined
}



# #overimp1D_density --------------------------------------------------------
overimp1D_density<-function(long_data, x, comb_title, alpha, linewidth, ylim){
  train_plot <- ggplot(long_data$train$long_df, aes(x = .data[[x]], color = Group)) +
    geom_density(alpha = alpha, linewidth = linewidth) +
    scale_color_manual(values = long_data$train$colors) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(long_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(long_data$test$long_df, aes(x = .data[[x]], color = Group)) +
      geom_density(alpha = alpha, linewidth = linewidth) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = long_data$test$colors) +
      labs(title = "Test Data")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- gridExtra::arrangeGrob(train_plot, test_plot, ncol = 2,
                                       top = comb_title)
  } else {
    combined <- gridExtra::arrangeGrob(train_plot, ncol = 1,
                                       top = comb_title)
  }

  class(combined)<- c("overimp_plot",class(combined))
  combined
}


# overimp1D_bar --------------------------------------------------------
overimp1D_bar<-function(long_data, x, comb_title, position, alpha, ylim){
  train_plot <- ggplot(long_data$train$long_df, aes(x = .data[[x]])) +
    geom_bar(aes(fill=Group)) +
    facet_grid(vars(Group), switch="y")+
    scale_fill_manual(values = long_data$train$colors) +
    labs(title = "Training Data")+
    guides(fill = "none")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(long_data$test)) {

    test_plot <-  ggplot(long_data$test$long_df, aes(x = .data[[x]])) +
      geom_bar(aes(fill=Group)) +
      facet_grid(vars(Group), switch="y")+
      scale_fill_manual(values = long_data$test$colors) +
      labs(title = "Test Data")+
      guides(fill = "none")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- gridExtra::arrangeGrob(train_plot, test_plot, ncol = 2,
                                       top = comb_title)
  } else {
    combined <- gridExtra::arrangeGrob(train_plot, ncol = 1,
                                       top = comb_title)
  }

  class(combined)<- c("overimp_plot",class(combined))
  combined
}


# overimp1D_dodge --------------------------------------------------------
overimp1D_dodge<-function(long_data, x, comb_title, position, alpha, ylim){
  train_plot <- ggplot(long_data$train$long_df, aes(x = .data[[x]], fill = Group)) +
    geom_bar(alpha = alpha, position = position) +
    scale_color_manual(values = long_data$train$colors) +
    scale_fill_manual(values = long_data$train$colors) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(long_data$test)) {

    test_plot <- ggplot(long_data$test$long_df, aes(x = .data[[x]], fill = Group)) +
      geom_bar(alpha = alpha,position = position) +
      scale_color_manual(values = long_data$train$colors) +
      scale_fill_manual(values = long_data$train$colors) +
      labs(title = "Test Data")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- gridExtra::arrangeGrob(train_plot, test_plot, ncol = 2,
                                       top = comb_title)
  } else {
    combined <- gridExtra::arrangeGrob(train_plot, ncol = 1,
                                       top = comb_title)
  }

  class(combined)<- c("overimp_plot",class(combined))
  combined
}

