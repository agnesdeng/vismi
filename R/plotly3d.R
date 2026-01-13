plotly_3num <- function(all_dt, x, y, z, color_pal, point_size, alpha, title, subtitle) {
  fig <- plot_ly(all_dt,
    x = as.formula(paste0("~", x)),
    y = as.formula(paste0("~", y)),
    z = as.formula(paste0("~", z)),
    color = ~Group, colors = color_pal,
    type = "scatter3d", mode = "markers",
    marker = list(
      size = point_size,
      opacity = alpha
    ),
    text = ~ paste(
      "Index:", row_index, "<br>",
      x, ":", get(x), "<br>",
      y, ":", get(y), "<br>",
      z, ":", get(z), "<br>",
      "Group: ", Group
    ),
    hoverinfo = "text"
  ) |>
    layout(scene = list(
      xaxis = list(title = list(text = x)),
      yaxis = list(title = list(text = y)),
      zaxis = list(title = list(text = z))
    ))


  fig <- .plotly_layout_scatter3d(fig, title, subtitle)
  fig
}


plotly_3fac <- function(all_dt, x, y, z, color_pal, alpha, title, subtitle) {
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
    "<b>", x, "</b>: ", all_sum[[x]], "  ",
    "<b>", y, "</b>: ", all_sum[[y]], "  ",
    "<b>", z, "</b>: ", all_sum[[z]]
  )

  fig <- plot_ly(
    data = all_sum,
    x = ~prop,
    y = ~comb,
    type = "bar",
    orientation = "h",
    color = ~Group,
    colors = color_pal,
    opacity = alpha,
    text = ~ paste0(
      "<b>Combination</b>: ", all_sum$comb, "<br>",
      "<b>Proportion</b>: ", all_sum$prop, "<br>",
      "<b>Group</b>: ", all_sum$Group
    ),
    hoverinfo = "text",
    textposition = "none"
  ) |>
    layout(
      xaxis = list(title = "", automargin = FALSE),
      # xaxis = list(title = list(text="Proportion",standoff = 10),automarigin="height"),
      yaxis = list(title = list(text = "Combination", standoff = 10))
    )


  fig <- .plotly_layout_common(fig, title, subtitle, x)
  fig
}


plotly_1fac2num <- function(all_dt, x, y, z, color_pal,
                            point_size, alpha, boxpoints, title, subtitle) {
  z_levels <- levels(all_dt[[z]])

  plots <- lapply(z_levels, function(z_level) {
    df_sub <- all_dt |> filter(.data[[z]] == z_level)
    plot_ly(
      data = df_sub,
      x = df_sub[[x]],
      y = df_sub[[y]],
      color = ~Group,
      colors = color_pal,
      type = "scatter",
      mode = "markers",
      marker = list(size = point_size, opacity = alpha),
      legendgroup = ~Group,
      showlegend = ifelse(z_level == z_levels[1], TRUE, FALSE),
      text = ~ paste(
        "Index:", row_index, "<br>",
        x, ":", get(x), "<br>",
        y, ":", get(y), "<br>",
        "Group:", Group
      ),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "", automargin = FALSE),
        yaxis = list(title = "", side = "left")
      )
  })


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


  fig <- .plotly_layout_facet3d(combine, title, subtitle, z_levels, y, z, x)
  fig
}


plotly_2fac1num <- function(all_dt, x, y, z, color_pal,
                            point_size, alpha, boxpoints, title, subtitle) {
  z_levels <- levels(all_dt[[z]])

  plots <- lapply(z_levels, function(z_level) {
    df_sub <- all_dt |> filter(.data[[z]] == z_level)
    fig <- plot_ly()
    for (group in levels(df_sub$Group)) {
      df_group <- df_sub |> filter(Group == group)
      fig <- fig |> add_trace(
        data = df_group,
        x = df_group[[x]],
        y = df_group[[y]],
        color = ~Group,
        colors = color_pal,
        type = "box",
        boxpoints = boxpoints,
        marker = list(size = point_size, opacity = alpha, color = color_pal[group]),
        name = group,
        legendgroup = group,
        offsetgroup = group,
        showlegend = (z_level == z_levels[1]),
        text = ~ paste(
          "Index:", row_index, "<br>",
          x, ":", get(x), "<br>",
          y, ":", get(y), "<br>",
          "Group:", Group
        ),
        hoverinfo = "text"
      )
    }

    fig |>
      layout(
        xaxis = list(title = "", automargin = FALSE),
        yaxis = list(title = "", side = "left")
      )
  })

  combine <- subplot(
    plots,
    nrows = length(plots),
    shareX = TRUE,
    shareY = TRUE,
    titleY = TRUE,
    titleX = TRUE
  )


  combine <- combine |> layout(
    boxmode = "group"
  )


  fig <- .plotly_layout_facet3d(combine, title, subtitle, z_levels, y, z, x)
  fig
}
