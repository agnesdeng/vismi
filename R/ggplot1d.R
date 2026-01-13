ggplot_1num <- function(all_dt, x, color_pal, point_size, alpha, nbins, marginal_x, title, subtitle) {
  if (is.null(nbins)) {
    breaks <- pretty(range(all_dt[[x]]),
      n = grDevices::nclass.Sturges(all_dt[[x]]),
      min.n = 1
    )
  } else {
    breaks <- seq(min(all_dt[[x]], na.rm = TRUE),
      max(all_dt[[x]], na.rm = TRUE),
      length.out = nbins + 1
    )
  }


  fig <- ggplot(all_dt, aes(x = .data[[x]])) +
    geom_histogram(aes(y = after_stat(density), fill = Group, color = Group), # scale histogram to density
      linewidth = 0.2, alpha = 0.2, position = "identity", breaks = breaks
    ) +
    geom_density(aes(y = after_stat(density), color = Group), # density scaled to match histogram
      linewidth = 1, alpha = 1
    ) +
    facet_grid(~Group) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    # paste("Observed vs multiply-imputed values for", x)
    labs(
      title = title, subtitle = subtitle,
      x = x, y = "Density"
    )

  if (!is.null(marginal_x)) {
    if (marginal_x == "box+rug") {
      fig <- .ggplot_box_1d(fig, all_dt, x, color_pal, y_box_offset = -0.02, box_width = 0.01)
      fig <- .ggplot_rug_1d(fig, all_dt, x, color_pal, y_rug_offset = -0.05)
    } else if (marginal_x == "box") {
      fig <- .ggplot_box_1d(fig, all_dt, x, color_pal, y_box_offset = -0.02, box_width = 0.01)
    } else if (marginal_x == "rug") {
      fig <- .ggplot_rug_1d(fig, all_dt, x, color_pal, y_rug_offset = -0.02)
    } else {
      stop("Invalid marginal_y option. Please choose from 'box+rug', 'box', or 'rug'.")
    }
  }

   fig+
    .ggplot_theme()

}


ggplot_1fac <- function(all_dt, x, color_pal = NULL, alpha = 0.8, width = 0.8, title, subtitle) {
  fig <- ggplot(all_dt, aes(x = .data[[x]])) +
    geom_bar(stat = "count", alpha = alpha, width = width, aes(color = Group, fill = Group, y = after_stat(prop), group = Group)) +
    scale_y_continuous(labels = scales::percent) +
    ylab("proportion") +
    facet_grid(cols = vars(Group)) +
    labs(title = title, subtitle = subtitle) +
    scale_fill_manual(values = color_pal) +
    scale_color_manual(values = color_pal)

  fig+
    .ggplot_theme()
}
