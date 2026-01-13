# overimp2D_scatter ------------------------------------------------------------
overimp2D_scatter <- function(plot_data, x, y, title, subtitle, alpha, point_size, xlim, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]], y = .data[[y]], color = Group)) +
    geom_point(alpha = alpha, size = point_size) +
    facet_grid(rows = vars(Group)) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data", x = x, y = y) +
    guides(color = "none")
  train_plot <- .ggplot_overimp_theme(train_plot, showlegend = FALSE)

  if (!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if (!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_scales_x[[1]]$range$range
    yrange <- ggplot_build(train_plot)$layout$panel_scales_y[[1]]$range$range

    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]], y = .data[[y]], color = Group)) +
      geom_point(alpha = alpha, size = point_size) +
      coord_cartesian(xlim = xrange, ylim = yrange) +
      facet_grid(rows = vars(Group)) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data", x = x, y = y) +
      guides(color = "none")
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)

    if (!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)


    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = title, subtitle = subtitle,
        theme = .vismi_overimp_combine_title()
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = title, subtitle = subtitle,
        theme = .vismi_overimp_combine_title()
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}


# overimp2D_box ----------------------------------------------------------------
overimp2D_box <- function(plot_data, x, y, title, subtitle, alpha, point_size, boxpoints, xlim, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]], y = .data[[y]])) +
    facet_grid(rows = vars(Group)) +
    scale_fill_manual(values = train_color_pal) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data", x = x, y = y) +
    guides(fill = "none", color = "none")

  if (isFALSE(boxpoints)) {
    train_plot <- train_plot +
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
  } else if (boxpoints == "all") {
    train_plot <- train_plot +
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA) +
      geom_jitter(alpha = alpha, size = point_size, position = position_jitter(seed = 2025))
  } else if (boxpoints == "outliers") {
    train_plot <- train_plot +
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.size = point_size, outlier.alpha = alpha)
  }
  train_plot <- .ggplot_overimp_theme(train_plot, showlegend = FALSE)

  if (!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if (!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_scales_x[[1]]$range$range
    yrange <- ggplot_build(train_plot)$layout$panel_scales_y[[1]]$range$range

    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      facet_grid(rows = vars(Group)) +
      scale_fill_manual(values = test_color_pal) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data", x = x, y = y) +
      guides(fill = "none", color = "none")
    if (isFALSE(boxpoints)) {
      test_plot <- test_plot +
        geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
    } else if (boxpoints == "all") {
      test_plot <- test_plot +
        geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA) +
        geom_jitter(alpha = alpha, size = point_size, position = position_jitter(seed = 2025))
    } else if (boxpoints == "outliers") {
      test_plot <- test_plot +
        geom_boxplot(alpha = alpha, aes(fill = Group), outlier.size = point_size, outlier.alpha = alpha)
    }
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)

    if (!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)


    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = title, subtitle = subtitle,
        theme = .vismi_overimp_combine_title()
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = title, subtitle = subtitle,
        theme = .vismi_overimp_combine_title()
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}


# overimp2D_bar ----------------------------------------------------------------
overimp2D_bar <- function(plot_data, x, y, title, subtitle, width, alpha, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]], alpha = .data[[y]])) +
    geom_bar(aes(fill = Group, color = Group), width = width, position = "dodge2") +
    facet_grid(rows = vars(Group)) +
    scale_alpha_discrete(range = c(0.1, 1)) +
    scale_color_manual(values = train_color_pal) +
    scale_fill_manual(values = train_color_pal) +
    labs(title = "Training Data", x = x, y = y) +
    guides(fill = "none", color = "none")
  train_plot <- .ggplot_overimp_theme(train_plot, showlegend = FALSE)

  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)


  if (!is.null(plot_data$test)) {
    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]], alpha = .data[[y]])) +
      geom_bar(aes(fill = Group, color = Group), width = width) +
      facet_grid(rows = vars(Group)) +
      scale_alpha_discrete(range = c(0.1, 1)) +
      scale_color_manual(values = test_color_pal) +
      scale_fill_manual(values = test_color_pal) +
      labs(title = "Test Data", x = x, y = y) +
      guides(fill = "none", color = "none", alpha = "none")
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)


    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = title, subtitle = subtitle,
        theme = .vismi_overimp_combine_title()
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = title, subtitle = subtitle,
        theme = .vismi_overimp_combine_title()
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}
