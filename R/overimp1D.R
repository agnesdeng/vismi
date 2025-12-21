# overimp1D_qq ------------------------------------------------------------
overimp1D_qq <- function(plot_data, x, comb_title, point_size, xlim, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(sample = .data[[x]], color = Group)) +
    stat_qq(size = point_size) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(sample = .data[[x]], color = Group)) +
      stat_qq(size = point_size) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
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
overimp1D_qqline <- function(plot_data, x, comb_title,  linewidth, xlim, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(sample = .data[[x]], color = Group)) +
    stat_qq_line(linewidth = linewidth) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(sample = .data[[x]], color = Group)) +
      stat_qq_line(linewidth=linewidth) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
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

overimp1D_ridge<-function(plot_data, x, comb_title, alpha, xlim, train_color_pal, test_color_pal){
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]])) +
    geom_density_ridges(alpha = alpha, aes(y = Group, fill = Group)) +
    scale_fill_manual(values = train_color_pal)+
    scale_y_discrete(limits = rev) +
    labs(title = "Training Data")+
    guides(fill = "none")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(xlim)) train_plot <- train_plot + xlim(xlim)

  if(!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]])) +
      geom_density_ridges(alpha = alpha, aes(y = Group, fill = Group)) +
      coord_cartesian(xlim = xrange) +
      scale_fill_manual(values = test_color_pal) +
      scale_y_discrete(limits = rev) +
      labs(title = "Test Data")+
      guides(fill = "none")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(xlim)) test_plot <- test_plot + xlim(xlim)

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
overimp1D_density<-function(plot_data, x, comb_title, alpha, linewidth, xlim, ylim, train_color_pal, test_color_pal){
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]], color = Group)) +
    geom_density(alpha = alpha, linewidth = linewidth) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]], color = Group)) +
      geom_density(alpha = alpha, linewidth = linewidth) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data")
    test_plot<-.ggplot_overimp_theme(test_plot)

    if(!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
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
overimp1D_bar<-function(plot_data, x, comb_title, position, alpha, ylim, train_color_pal, test_color_pal){
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]])) +
    geom_bar(aes(fill=Group)) +
    facet_grid(vars(Group), switch="y")+
    scale_fill_manual(values = train_color_pal) +
    labs(title = "Training Data")+
    guides(fill = "none")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(plot_data$test)) {

    test_plot <-  ggplot(plot_data$test$all_dt, aes(x = .data[[x]])) +
      geom_bar(aes(fill=Group)) +
      facet_grid(vars(Group), switch="y")+
      scale_fill_manual(values = test_color_pal) +
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
overimp1D_dodge<-function(plot_data, x, comb_title, position, alpha, ylim, train_color_pal, test_color_pal){
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]], fill = Group)) +
    geom_bar(alpha = alpha, position = position) +
    scale_color_manual(values = train_color_pal) +
    scale_fill_manual(values = train_color_pal) +
    labs(title = "Training Data")
  train_plot<-.ggplot_overimp_theme(train_plot)

  if(!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if(!is.null(plot_data$test)) {

    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]], fill = Group)) +
      geom_bar(alpha = alpha,position = position) +
      scale_color_manual(values = train_color_pal) +
      scale_fill_manual(values = train_color_pal) +
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

