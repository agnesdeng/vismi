ggplot_2num <- function(all_dt, x, y, color_pal, point_size, alpha, marginal_x, marginal_y, title, subtitle) {
  fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = alpha, aes(color = Group, fill = Group), size = point_size) +
    facet_grid(cols = vars(Group)) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    labs(x = x, y = y, title = title, subtitle = subtitle)

  if (!is.null(marginal_x)) {
    if (marginal_x == "box+rug") {
      fig <- .ggplot_box_rug_x(fig, all_dt, x, y, color_pal)
    } else if (marginal_x == "box") {
      fig <- .ggplot_box_x(fig, all_dt, x, y, color_pal)
    } else if (marginal_x == "rug") {
      fig <- .ggplot_rug_x(fig, all_dt, x, y, color_pal)
    } else if (marginal_x == "hist") {
      stop("Static histogram marginal plots are not supported. Please set interactive = TRUE.")
    } else {
      stop("Invalid marginal_x option. Please choose from 'box+rug', 'box', or 'rug'.")
    }
  }

  if (!is.null(marginal_y)) {
    if (marginal_y == "box+rug") {
      fig <- .ggplot_box_rug_y(fig, all_dt, x, y, color_pal)
    } else if (marginal_y == "box") {
      fig <- .ggplot_box_y(fig, all_dt, x, y, color_pal)
    } else if (marginal_y == "rug") {
      fig <- .ggplot_rug_y(fig, all_dt, x, y, color_pal)
    } else if (marginal_y == "hist") {
      stop("Static histogram marginal plots are not supported. Please set interactive = TRUE.")
    } else {
      stop("Invalid marginal_y option. Please choose from 'box+rug', 'box', or 'rug'.")
    }
  }

  fig+
    .ggplot_theme()

}


ggplot_1fac1num <- function(all_dt, x, y, color_pal, point_size, alpha, boxpoints, title, subtitle) {
  if (isFALSE(boxpoints)) {
    fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group, color = Group), outlier.shape = NA)
  } else if (boxpoints == "all") {
    fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_jitter(alpha = alpha, position = position_jitter(seed = 2025), aes(color = Group, fill = Group), size = point_size) +
      geom_boxplot(alpha = alpha, aes(fill = Group, color = Group), outlier.shape = NA)
  } else if (boxpoints == "outliers") {
    fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group, color = Group), outlier.size = point_size, outlier.alpha = alpha)
  }

  fig +
    facet_grid(cols = vars(Group)) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    labs(x = x, y = y, title = title, subtitle = subtitle)+
    .ggplot_theme()


}


ggplot_2fac <- function(all_dt, x, y, color_pal, alpha, width, title, subtitle) {
  all_sum <- all_dt |>
    group_by(Group, .data[[x]], .data[[y]]) |>
    summarise(count = n(), .groups = "drop") |>
    tidyr::complete(
      Group = unique(all_dt$Group),
      !!sym(x) := unique(all_dt[[x]]),
      !!sym(y) := unique(all_dt[[y]]),
      fill = list(count = 0)
    ) |>
    group_by(Group) |>
    mutate(prop = count / sum(count)) |>
    ungroup() |>
    filter(prop > 0)


  fig <- ggplot(all_sum, aes(x = .data[[x]], y = prop)) +
    geom_bar(stat = "identity", position = "dodge2", aes(fill = Group), alpha = alpha, width = width) +
    facet_grid(.data[[y]] ~ Group, labeller = labeller(
      .rows = label_value,
      # .rows = function(level) paste(y, "=", level),   # only the row facet gets "z = level"
      .cols = label_value # column facet just shows values, no "Group = "
    )) +
    scale_fill_manual(values = color_pal) +
    # scale_y_continuous(sec.axis = sec_axis(~., name = y, breaks = NULL, labels = NULL)) +
    labs(x = x, y = "Proportion", title = title, subtitle = subtitle)+
    .ggplot_theme()


  gridExtra::arrangeGrob(fig, right = y)
}
