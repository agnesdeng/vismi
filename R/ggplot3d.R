ggplot_3num <- function(all_dt, x, y, z, color_pal, point_size, alpha, plot_title) {
  GGally::ggpairs(
    all_dt,
    columns = c(x, y, z),
    mapping = ggplot2::aes(
      color = Group,
      alpha = alpha
    ),
  ) + scale_color_manual(values = as.vector(color_pal)) +
    labs(title = plot_title)
}


ggplot_3fac <- function(all_dt, x, y, z, color_pal, plot_title) {
  all_sum <- all_dt |>
    group_by(Group, .data[[x]], .data[[y]], .data[[z]]) |>
    summarise(count = n(), .groups = "drop") |>
    tidyr::complete(
      Group = unique(all_dt$Group),
      !!sym(x) := unique(all_dt[[x]]),
      !!sym(y) := unique(all_dt[[y]]),
      !!sym(z) := unique(all_dt[[z]]),
      fill = list(count = 0)
    ) |>
    group_by(Group) |>
    mutate(prop = count / sum(count)) |>
    ungroup()


  all_sum$comb <- paste0(
    "**", x, "**: ", all_sum[[x]], "  ",
    "**", y, "**: ", all_sum[[y]], "  ",
    "**", z, "**: ", all_sum[[z]]
  )
  fig <- ggplot(all_sum, aes(x = prop, y = .data$comb, alpha = .data[[z]], fill = Group, color = Group)) +
    geom_col(stat = "identity") +
    facet_grid(~Group) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    scale_alpha_manual(values = seq(0.3, 1, length.out = length(unique(all_dt[[z]])))) +
    labs(x = "Proportion", y = "Combination", title = plot_title)+
    guides(fill = "none", color = "none",alpha = guide_legend(override.aes = list(size = 3)))
    .ggplot_theme_3fac(fig)
}


ggplot_1fac2num <- function(all_dt, x, y, z, color_pal, point_size, alpha, plot_title) {
  fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = alpha, aes(color = Group, fill = Group), size = point_size) +
    facet_grid(.data[[z]] ~ Group, labeller = labeller(
      # .rows = function(level) paste(z, "=", level),   # only the row facet gets "z = level"
      .rows = label_value,
      .cols = label_value # column facet just shows values, no "Group = "
    )) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    # scale_y_continuous(sec.axis = sec_axis(~., name = z, breaks = NULL, labels = NULL))+
    labs(x = x, y = y, title = plot_title)

  fig <- .ggplot_theme(fig)
  gridExtra::arrangeGrob(fig, right = z)
}


ggplot_2fac1num <- function(all_dt, x, y, z, color_pal, point_size, alpha, boxpoints, plot_title) {
  if (isFALSE(boxpoints)) {
    fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
  } else if (boxpoints == "all") {
    fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_jitter(alpha = alpha, position = position_jitter(seed = 2025), aes(color = Group, fill = Group), size = point_size) +
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
  } else if (boxpoints == "outliers") {
    fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group, color = Group), outlier.size = point_size, outlier.alpha = alpha)
  }
  fig <- fig +
    facet_grid(.data[[z]] ~ Group, labeller = labeller(
      .rows = label_value,
      # .rows = function(level) paste(z, ":\n", level),   # only the row facet gets "z = level"
      .cols = label_value # column facet just shows values, no "Group = "
    )) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    # scale_y_continuous(sec.axis = sec_axis(~., name = z, breaks = NULL, labels = NULL))+
    labs(x = x, y = y, title = plot_title)
  fig <- .ggplot_theme(fig)


  gridExtra::arrangeGrob(fig, right = z)
}
