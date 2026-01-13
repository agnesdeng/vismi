plotly_2num <- function(all_dt, imp_list, x, y, color_pal,
                        point_size, alpha, marginal_x, marginal_y, nbins, title, subtitle) {
  # set the canva: left 0 right 1 bottom 0 top 1
  main_x <- c(0, 0.8)
  main_y <- c(0, 0.7)
  mx_x <- main_x
  mx_y <- c(0.72, 1)
  my_x <- c(0.82, 1)
  my_y <- main_y

  n_levels <- nlevels(all_dt$Group)
  names_levels <- levels(all_dt$Group)

  fig <- plot_ly()


  fig <- fig |> add_trace(
    data = all_dt,
    x = ~ get(x),
    y = ~ get(y),
    color = ~Group,
    colors = color_pal,
    type = "scatter",
    mode = "markers",
    marker = list(size = point_size, opacity = alpha),
    legendgroup = ~Group,
    showlegend = TRUE,
    text = ~ paste(
      "Index:", row_index, "<br>",
      x, ":", get(x), "<br>",
      y, ":", get(y), "<br>",
      "Group:", Group
    ),
    hoverinfo = "text",
    xaxis = "x", yaxis = "y"
  )


  # --- Top marginal boxplots (x variable) ---
  if (!is.null(marginal_x)) {
    if (marginal_x == "box+rug") {
      fig <- .plotly_box_rug_x(fig, all_dt, x, color_pal, alpha)
    } else if (marginal_x == "box") {
      fig <- .plotly_box_x(fig, all_dt, x, color_pal, alpha)
    } else if (marginal_x == "rug") {
      mx_offsets <- stats::setNames(
        seq(mx_y[1], mx_y[2], length.out = n_levels),
        names_levels
      )
      fig <- .plotly_rug_x(fig, all_dt, x, color_pal, mx_offsets, alpha)
    } else if (marginal_x == "hist") {
      fig <- .plotly_hist_x(fig, all_dt, x, color_pal, nbins, alpha)
    }
  }


  # --- Right marginal boxplots (y variable) ---
  if (!is.null(marginal_y)) {
    if (marginal_y == "box+rug") {
      fig <- .plotly_box_rug_y(fig, all_dt, y, color_pal, alpha)
    } else if (marginal_y == "box") {
      fig <- .plotly_box_y(fig, all_dt, y, color_pal, alpha)
    } else if (marginal_y == "rug") {
      my_offsets <- stats::setNames(
        seq(my_x[1], my_x[2], length.out = n_levels),
        names_levels
      )
      fig <- .plotly_rug_y(fig, all_dt, y, color_pal, my_offsets, alpha)
    } else if (marginal_y == "hist") {
      fig <- .plotly_hist_y(fig, all_dt, y, color_pal, nbins, alpha)
    }
  }

  if (is.null(marginal_x) & is.null(marginal_y)) {
    fig <- fig |> layout(
      xaxis = list(title = "", automargin = FALSE),
      # xaxis = list(title = x),
      yaxis = list(title = list(text = y))
    )
  } else {
    fig <- fig |> layout(
      # scatter
      xaxis = list(domain = main_x, title = "", automargin = FALSE, gridcolor = "white"),
      # xaxis = list(domain = main_x,title=x, gridcolor='white'),
      yaxis = list(domain = main_y, title = list(text = y), gridcolor = "white"),

      # margin_x top-left
      xaxis2 = list(domain = mx_x, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
      yaxis2 = list(domain = mx_y, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),

      # margin_y right-bottom
      xaxis3 = list(domain = my_x, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
      yaxis3 = list(domain = my_y, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
      # showlegend = TRUE,
    )
  }


  fig <- .plotly_layout_common(fig, title, subtitle, x)
  fig
}


plotly_1fac1num <- function(all_dt, imp_list, x, y, color_pal,
                            point_size, alpha, boxpoints, title, subtitle) {
  fig <- plot_ly()

  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      data = df_group,
      x = ~ get(x),
      y = ~ get(y),
      color = ~Group,
      colors = color_pal,
      type = "box",
      boxpoints = boxpoints,
      marker = list(size = point_size, opacity = alpha, color = color_pal[group]),
      name = group,
      legendgroup = group,
      showlegend = TRUE,
      text = ~ paste(
        "Index:", row_index, "<br>",
        x, ":", get(x), "<br>",
        y, ":", get(y), "<br>",
        "Group:", Group
      ),
      hoverinfo = "text",
      xaxis = "x", yaxis = "y"
    )
  }

  fig <- fig |> layout(
    boxmode = "group",
    xaxis = list(title = "", automargin = FALSE),
    yaxis = list(title = list(text = y), showticklabels = TRUE, tickangle = -90)
  )


  fig <- .plotly_layout_common(fig, title, subtitle, x)
  fig
}


plotly_2fac <- function(all_dt, imp_list, x, y, color_pal, alpha, title, subtitle) {
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
    ungroup()

  groups <- levels(all_sum$Group)

  y_levels <- levels(all_sum[[y]])


  plots <- lapply(y_levels, function(y_level) {
    df_sub <- all_sum |> filter(.data[[y]] == y_level)
    plot_ly(
      data = df_sub,
      x = ~ get(x),
      y = ~prop,
      color = ~Group,
      colors = color_pal,
      type = "bar",
      opacity = alpha,
      legendgroup = ~Group,
      showlegend = FALSE,
      # showlegend = ifelse(y_level == y_levels[1], TRUE, FALSE),
      text = ~ paste(
        x, ":", get(x), "<br>",
        y, ":", get(y), "<br>",
        "Proportion:", prop, "<br>"
      ),
      textposition = "none",
      hovertext = ~ paste(
        x, ":", get(x), "<br>",
        y, ":", get(y), "<br>",
        "Proportion:", prop, "<br>",
        "Group:", Group
      ),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(
          title = "", automargin = FALSE,
          type = "category",
          categoryorder = "array",
          categoryarray = levels(all_sum[[x]])
        ),
        yaxis = list(title = "", range = c(0, 1), dtick = 0.1, side = "left")
      )
  })


  # Add dummy traces for legend
  x_dummy <- levels(all_sum[[x]])[1]

  for (i in seq_along(plots)) {
    for (g in groups) {
      plots[[i]] <- plots[[i]] |>
        add_trace(
          x = x_dummy,
          y = 0,
          type = "bar",
          name = g,
          legendgroup = g,
          color = I(color_pal[g]),
          hoverinfo = "none",
          showlegend = (i == 1) # legend entries only once
        )
    }
  }


  combine <- subplot(
    plots,
    nrows = length(plots),
    shareX = TRUE,
    shareY = TRUE,
    titleY = TRUE,
    titleX = TRUE
  )
  combine <- combine |> layout(
    barmode = "group"
  )


  fig <- .plotly_layout_2fac(combine, title, subtitle, y_levels, y, x)
  fig
}
