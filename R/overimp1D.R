# plot imputed vs masked true for a single numeric variable ---------------
overimp1D_cv_num <- function(plot_data, x, comb_title, point_size, xlim, ylim, train_color_pal, test_color_pal) {
  .transform_data <- function(data, x) {
    mt_data <- data |>
      filter(Group == "Masked true") |>
      select(row_index, `Masked true` = all_of(x))

    join_data <- data |>
      filter(Group != "Masked true") |>
      left_join(mt_data, by = "row_index")

    line_data <- join_data |>
      group_by(.data[["Masked true"]]) |>
      summarise(ymin = min(.data[[x]]), ymax = max(.data[[x]]), .groups = "drop")
    max_val <- max(data[[x]])
    min_val <- min(data[[x]])

    return(list(join_data = join_data, line_data = line_data, max_val = max_val, min_val = min_val))
  }


  train_data <- .transform_data(data = plot_data$train$all_dt, x)
  if(!is.null(plot_data$test)){
    test_data <- .transform_data(data = plot_data$test$all_dt, x)
    min_val <- min(train_data$min_val, test_data$min_val)
    max_val <- max(train_data$max_val,test_data$max_val)
  }else{
    min_val <- train_data$min_val
    max_val <- train_data$max_val
  }


  train_plot <- ggplot(train_data$join_data, aes(x = .data[["Masked true"]], y = .data[[x]], color = Group)) +
    geom_point(size = 0.5) +
    geom_segment(
      data = train_data$line_data, aes(
        x = .data[["Masked true"]], xend = .data[["Masked true"]],
        y = .data$ymin, yend = .data$ymax
      ),
      inherit.aes = FALSE, color = "gray40", linetype = "solid", linewidth = 0.1
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "gray20") +
    scale_color_manual(values = train_color_pal) +
    labs(
      title = "Training Data",
      x = paste("Masked true:", x), y = paste("Imputed:", x)
    )
  train_plot <- .ggplot_overimp_theme(train_plot, showlegend = TRUE)


  if (!is.null(xlim)) train_plot <- train_plot + xlim(xlim) else train_plot <- train_plot + xlim(c(min_val, max_val))
  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim) else train_plot <- train_plot + ylim(c(min_val, max_val))

  if (!is.null(plot_data$test)) {


    test_plot <- ggplot(test_data$join_data, aes(x = .data[["Masked true"]], y = .data[[x]], color = Group)) +
      geom_point(size = 0.5) +
      geom_segment(
        data = test_data$line_data, aes(
          x = .data[["Masked true"]], xend = .data[["Masked true"]],
          y = .data$ymin, yend = .data$ymax
        ),
        inherit.aes = FALSE, color = "gray40", linetype = "solid", linewidth = 0.1
      ) +
      geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "gray20") +
      scale_color_manual(values = test_color_pal) +
      labs(
        title = "Test Data",
        x = paste("Masked true:", x), y = paste("Imputed:", x)
      )
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)

    if (!is.null(xlim)) test_plot <- test_plot + xlim(xlim) else test_plot <- test_plot + xlim(c(min_val, max_val))
    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim) else test_plot <- test_plot + ylim(c(min_val, max_val))

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}


#  plot imputed vs masked true for a single fac variable ------------------
overimp1D_cv_fac <- function(plot_data, x, comb_title, point_size, xlim, ylim, train_color_pal, test_color_pal, stack_y, diag_color, seed) {
  .transform_data <- function(data, x) {
    mt_data <- data |>
      filter(Group == "Masked true") |>
      dplyr::select(row_index, `Masked true` = all_of(x))

    join_data <- data |>
      filter(Group != "Masked true") |>
      left_join(mt_data, by = "row_index")


    n_groups <- length(unique(join_data$Group))
    spacing <- 0.8 / n_groups # 0.8 is the total width available for all groups

    join_data <- join_data |>
      mutate(
        x_num = as.numeric(as.factor(.data$`Masked true`)),
        # Create an offset: Group 1 moves left, Group 2 stays center, Group 3 moves right
        group_idx = as.numeric(as.factor(Group)),
        group_offset = (.data$group_idx - (max(.data$group_idx) + 1) / 2) * spacing
      )

    join_data
  }


  # two different version of plots
  train_data <- .transform_data(data = plot_data$train$all_dt, x)
  x_labels <- levels(train_data$`Masked true`)

  if (stack_y) {
    if (!is.null(diag_color)) {
      n <- length(x_labels) # or however many levels you have

      bg <- expand.grid(
        x = seq_len(n),
        y = seq_len(n)
      ) |>
        mutate(
          xmin = .data$x - 0.5,
          xmax = .data$x + 0.5,
          ymin = .data$y - 0.5,
          ymax = .data$y + 0.5,
          bg_color = ifelse(.data$x == .data$y, diag_color, "transparent")
        )

      train_plot <- ggplot() +
        geom_rect(
          data = bg,
          aes(
            xmin = .data$xmin, xmax = .data$xmax,
            ymin = .data$ymin, ymax = .data$ymax,
            fill = .data$bg_color
          ),
          inherit.aes = FALSE
        ) +
        scale_fill_identity()
    } else {
      train_plot <- ggplot()
    }


    train_data <- train_data |>
      group_by(.data$x_num, Group, .data[[x]]) |>
      mutate(
        n = n(),
        row_id = row_number(),
        y = as.numeric(.data[[x]])
      )
    max_n <- max(train_data$n)

    train_data <- train_data |>
      mutate(y_offset = -0.4 + .data$y + (.data$row_id - 1) * 0.8 / max_n)


    train_plot <- train_plot +
      geom_point(
        data = train_data, aes(x = .data$x_num + .data$group_offset, y = .data$y_offset, color = Group),
        size = point_size
      ) +
      scale_x_continuous(
        breaks = 1:length(x_labels), # The integer positions
        labels = x_labels # The original names
      ) +
      scale_color_manual(values = train_color_pal) +
      labs(
        title = "Training Data",
        x = paste("Masked true:", x), y = paste("Imputed:", x)
      ) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white")
      )
  } else {
    train_plot <- ggplot(train_data, aes(x = .data$x_num + .data$group_offset, y = .data[[x]], color = Group)) +
      geom_point(
        position = position_jitter(width = 0, height = 0.3, seed = seed),
        size = point_size
      ) +
      scale_x_continuous(
        breaks = 1:length(x_labels), # The integer positions
        labels = x_labels # The original names
      ) +
      scale_color_manual(values = train_color_pal) +
      labs(
        title = "Training Data",
        x = paste("Masked true:", x), y = paste("Imputed:", x)
      )
  }
  train_plot <- .ggplot_overimp_theme(train_plot, showlegend = TRUE)


  if (!is.null(plot_data$test)) {
    test_data <- .transform_data(data = plot_data$test$all_dt, x)

    if (stack_y) {
      if (!is.null(diag_color)) {
        n <- length(x_labels) # or however many levels you have

        bg <- expand.grid(
          x = seq_len(n),
          y = seq_len(n)
        ) |>
          mutate(
            xmin = .data$x - 0.5,
            xmax = .data$x + 0.5,
            ymin = .data$y - 0.5,
            ymax = .data$y + 0.5,
            bg_color = ifelse(.data$x == .data$y, diag_color, "transparent")
          )

        test_plot <- ggplot() +
          geom_rect(
            data = bg,
            aes(
              xmin = .data$xmin, xmax = .data$xmax,
              ymin = .data$ymin, ymax = .data$ymax,
              fill = .data$bg_color
            ),
            inherit.aes = FALSE
          ) +
          scale_fill_identity()
      } else {
        test_plot <- ggplot()
      }


      test_data <- test_data |>
        group_by(.data$x_num, Group, .data[[x]]) |>
        mutate(
          n = n(),
          row_id = row_number(),
          y = as.numeric(.data[[x]])
        )
      max_n <- max(test_data$n)

      test_data <- test_data |>
        mutate(y_offset = -0.4 + .data$y + (.data$row_id - 1) * 0.8 / max_n)


      test_plot <- test_plot +
        geom_point(
          data = test_data, aes(x = .data$x_num + .data$group_offset, y = .data$y_offset, color = Group),
          size = point_size
        ) +
        scale_x_continuous(
          breaks = 1:length(x_labels), # The integer positions
          labels = x_labels # The original names
        ) +
        scale_color_manual(values = test_color_pal) +
        labs(
          title = "Training Data",
          x = paste("Masked true:", x), y = paste("Imputed:", x)
        ) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "white")
        )
    } else {
      test_plot <- ggplot(test_data, aes(x = .data$x_num + .data$group_offset, y = .data[[x]], color = Group)) +
        geom_point(
          position = position_jitter(width = 0, height = 0.3, seed = seed),
          size = point_size
        ) +
        scale_x_continuous(
          breaks = 1:length(x_labels), # The integer positions
          labels = x_labels # The original names
        ) +
        scale_color_manual(values = test_color_pal) +
        labs(
          title = "Test Data",
          x = paste("Masked true:", x), y = paste("Imputed:", x)
        )
    }


    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)


    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}


# overimp1D_qq ------------------------------------------------------------
overimp1D_qq <- function(plot_data, x, comb_title, point_size, xlim, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(sample = .data[[x]], color = Group)) +
    stat_qq(size = point_size) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data")
  train_plot <- .ggplot_overimp_theme(train_plot)

  if (!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if (!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(sample = .data[[x]], color = Group)) +
      stat_qq(size = point_size) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data")
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)

    if (!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}

# overimp1D_qqline --------------------------------------------------------
overimp1D_qqline <- function(plot_data, x, comb_title, linewidth, xlim, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(sample = .data[[x]], color = Group)) +
    stat_qq_line(linewidth = linewidth) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data")
  train_plot <- .ggplot_overimp_theme(train_plot)

  if (!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if (!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(sample = .data[[x]], color = Group)) +
      stat_qq_line(linewidth = linewidth) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data")
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)

    if (!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }


  class(combined) <- c("overimp_plot", class(combined))
  combined
}


# #overimp1D_ridge --------------------------------------------------------

overimp1D_ridge <- function(plot_data, x, comb_title, alpha, xlim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]])) +
    geom_density_ridges(alpha = alpha, aes(y = Group, fill = Group)) +
    scale_fill_manual(values = train_color_pal) +
    scale_y_discrete(limits = rev) +
    labs(title = "Training Data", x = x) +
    guides(fill = "none")
  train_plot <- .ggplot_overimp_theme(train_plot)

  if (!is.null(xlim)) train_plot <- train_plot + xlim(xlim)

  if (!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]])) +
      geom_density_ridges(alpha = alpha, aes(y = Group, fill = Group)) +
      coord_cartesian(xlim = xrange) +
      scale_fill_manual(values = test_color_pal) +
      scale_y_discrete(limits = rev) +
      labs(title = "Test Data", x = x) +
      guides(fill = "none")
    test_plot <- .ggplot_overimp_theme(test_plot)

    if (!is.null(xlim)) test_plot <- test_plot + xlim(xlim)

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}


# #overimp1D_density --------------------------------------------------------
overimp1D_density <- function(plot_data, x, comb_title, alpha, linewidth, xlim, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]], color = Group)) +
    geom_density(alpha = alpha, linewidth = linewidth) +
    scale_color_manual(values = train_color_pal) +
    labs(title = "Training Data", x = x)
  train_plot <- .ggplot_overimp_theme(train_plot)

  if (!is.null(xlim)) train_plot <- train_plot + xlim(xlim)
  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if (!is.null(plot_data$test)) {
    xrange <- ggplot_build(train_plot)$layout$panel_params[[1]]$x.range
    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]], color = Group)) +
      geom_density(alpha = alpha, linewidth = linewidth) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = test_color_pal) +
      labs(title = "Test Data", x = x)
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)

    if (!is.null(xlim)) test_plot <- test_plot + xlim(xlim)
    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }

  class(combined) <- c("overimp_plot", class(combined))
  combined
}


# overimp1D_bar --------------------------------------------------------
overimp1D_bar <- function(plot_data, x, comb_title, position, alpha, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]])) +
    geom_bar(aes(fill = Group)) +
    facet_grid(vars(Group), switch = "y") +
    scale_fill_manual(values = train_color_pal) +
    labs(title = "Training Data") +
    guides(fill = "none")
  train_plot <- .ggplot_overimp_theme(train_plot)

  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if (!is.null(plot_data$test)) {
    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]])) +
      geom_bar(aes(fill = Group)) +
      facet_grid(vars(Group), switch = "y") +
      scale_fill_manual(values = test_color_pal) +
      labs(title = "Test Data") +
      guides(fill = "none")
    test_plot <- .ggplot_overimp_theme(test_plot)

    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }


  class(combined) <- c("overimp_plot", class(combined))
  combined
}


# overimp1D_dodge --------------------------------------------------------
overimp1D_dodge <- function(plot_data, x, comb_title, position, alpha, ylim, train_color_pal, test_color_pal) {
  train_plot <- ggplot(plot_data$train$all_dt, aes(x = .data[[x]], fill = Group)) +
    geom_bar(alpha = alpha, position = position) +
    # scale_color_manual(values = train_color_pal) +
    scale_fill_manual(values = train_color_pal) +
    labs(title = "Training Data")
  train_plot <- .ggplot_overimp_theme(train_plot)

  if (!is.null(ylim)) train_plot <- train_plot + ylim(ylim)

  if (!is.null(plot_data$test)) {
    test_plot <- ggplot(plot_data$test$all_dt, aes(x = .data[[x]], fill = Group)) +
      geom_bar(alpha = alpha, position = position) +
      # scale_color_manual(values = train_color_pal) +
      scale_fill_manual(values = train_color_pal) +
      labs(title = "Test Data")
    test_plot <- .ggplot_overimp_theme(test_plot, showlegend = FALSE)

    if (!is.null(ylim)) test_plot <- test_plot + ylim(ylim)

    combined <- train_plot + test_plot +
      plot_layout(ncol = 2) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  } else {
    combined <- train_plot +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = comb_title,
        theme = theme(
          plot.title = element_text(
            size = 14, # Font size
            face = "bold", # "plain", "italic", "bold", "bold.italic"
            family = "sans", # Font family (e.g., "sans", "serif", "mono")
            hjust = 0.5, # Center the title (0 = left, 1 = right)
            color = "black" # Font color
          )
        )
      )
  }


  class(combined) <- c("overimp_plot", class(combined))
  combined
}
