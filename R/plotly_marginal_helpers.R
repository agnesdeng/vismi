# box+rug -----------------------------------------------------------------
.plotly_box_rug_x <- function(fig, all_dt, x, color_pal, alpha) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      x = df_group[[x]],
      type = "box",
      orientation = "h",
      boxpoints = "all",
      jitter = 0,
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        symbol = "line-ns-open",
        size = 5, # size of the points
        color = color_pal[group], # points color
        opacity = alpha # transparency
      ),
      inherit = FALSE,
      hoverinfo = "x+name",
      xaxis = "x2",
      yaxis = "y2" # separate y-axis for top boxplot
    )
  }
  fig
}


.plotly_box_rug_y <- function(fig, all_dt, y, color_pal, alpha) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      y = df_group[[y]],
      type = "box",
      orientation = "v",
      boxpoints = "all",
      jitter = 0,
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        symbol = "line-ew-open",
        size = 5, # size of the points
        color = color_pal[group], # points color
        opacity = alpha # transparency
      ),
      # width=0.3,
      inherit = FALSE,
      hoverinfo = "y+name",
      xaxis = "x3", # separate x-axis for right boxplot
      yaxis = "y3"
    )
  }
  fig
}


# box ---------------------------------------------------------------------


.plotly_box_x <- function(fig, all_dt, x, color_pal, alpha) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      x = df_group[[x]],
      type = "box",
      orientation = "h",
      boxpoints = "outliers",
      jitter = 0,
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        # symbol = "line-ns-open",
        size = 5, # size of the points
        color = color_pal[group], # points color
        opacity = alpha # transparency
      ),
      inherit = FALSE,
      hoverinfo = "x+name",
      xaxis = "x2",
      yaxis = "y2" # separate y-axis for top boxplot
    )
  }
  fig
}


.plotly_box_y <- function(fig, all_dt, y, color_pal, alpha) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      y = df_group[[y]],
      type = "box",
      orientation = "v",
      boxpoints = "outliers",
      jitter = 0,
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        # symbol = "line-ew-open",
        size = 5, # size of the points
        color = color_pal[group], # points color
        opacity = alpha # transparency
      ),
      # width=0.3,
      inherit = FALSE,
      hoverinfo = "y+name",
      xaxis = "x3", # separate x-axis for right boxplot
      yaxis = "y3"
    )
  }
  fig
}


#


# rug---------------------------------------------------------------------
.plotly_rug_x <- function(fig, all_dt, x, color_pal, mx_offsets, alpha) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> dplyr::filter(Group == group)
    rug_y <- rep(mx_offsets[group], length(df_group[[x]]))

    fig <- fig |> add_trace(
      x = df_group[[x]],
      y = rug_y, # collapse vertically
      type = "scatter",
      mode = "markers",
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      marker = list(
        symbol = "line-ns-open", # vertical ticks
        size = 6,
        color = color_pal[group],
        opacity = alpha
      ),
      hoverinfo = "x+name",
      inherit = FALSE,
      xaxis = "x2",
      yaxis = "y2" # top rug axis
    )
  }
  fig
}


.plotly_rug_y <- function(fig, all_dt, y, color_pal, my_offsets, alpha) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> dplyr::filter(Group == group)
    rug_x <- rep(my_offsets[group], length(df_group[[y]]))

    fig <- fig |> add_trace(
      x = rug_x, # collapse horizontally
      y = df_group[[y]],
      type = "scatter",
      mode = "markers",
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      marker = list(
        symbol = "line-ew-open", # horizontal ticks
        size = 6,
        color = color_pal[group],
        opacity = alpha
      ),
      hoverinfo = "y+name",
      inherit = FALSE,
      xaxis = "x3", # right rug axis
      yaxis = "y3"
    )
  }
  fig
}


# histogram ---------------------------------------------------------------------


.plotly_hist_x <- function(fig, all_dt, x, color_pal, nbins, alpha) {
  if (is.null(nbins)) {
    breaks <- pretty(range(all_dt[[x]]),
      n = grDevices::nclass.Sturges(all_dt[[x]]),
      min.n = 1
    )
    nbins <- length(breaks)
  }


  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      x = df_group[[x]],
      type = "histogram",
      histnorm = "probability density",
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        color = color_pal[group],
        opacity = alpha
      ),
      nbinsx = nbins,
      inherit = FALSE,
      hoverinfo = "x+name",
      xaxis = "x2",
      yaxis = "y2" # separate y-axis for top boxplot
    )
  }
  fig
}


.plotly_hist_y <- function(fig, all_dt, y, color_pal, nbins, alpha) {
  if (is.null(nbins)) {
    breaks <- pretty(range(all_dt[[y]]),
      n = grDevices::nclass.Sturges(all_dt[[y]]),
      min.n = 1
    )
    nbins <- length(breaks)
  }

  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      y = df_group[[y]],
      type = "histogram",
      histnorm = "probability density",
      orientation = "v",
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        color = color_pal[group],
        opacity = alpha
      ),
      nbinsy = nbins,
      inherit = FALSE,
      hoverinfo = "y+name",
      xaxis = "x3", # separate x-axis for right boxplot
      yaxis = "y3"
    )
  }
  fig
}
