plotly_1num <- function(data,imp_list,x,color_pal=NULL){
  pre <- preprocess(data, imp_list, vars = x)
  all_dt <- pre$all_dt
  if(is.null(color_pal)){
    color_pal <- pre$color_pal
  }

  k<-length(pre$color_pal)
  box_offsets <- stats::setNames(seq(0.3, 0, length.out = k),
                                 names(pre$color_pal))

  breaks <- pretty(range(all_dt[[x]]),
                   n = grDevices::nclass.Sturges(all_dt[[x]]),
                   min.n = 1
  )


  # Compute density per group
  all_density <- all_dt |>
    group_by(Group) |>
    reframe({
      out <- density(.data[[x]], na.rm = TRUE)
      tibble(x = out$x, y = out$y)
    })


  # Initialize plotly figure
  fig_hist <- plot_ly()
  fig_box <- plot_ly()

  # Add histogram per group, scaled to density
  for(group in levels(all_dt$Group)){
    df_group <- all_dt |> filter(Group == group)

    fig_hist  <-  fig_hist   |> add_histogram(
      x = df_group[[x]],
      histnorm = "probability density",   # <-- this scales histogram to density
      nbinsx = length(breaks),
      name = group,
      legendgroup = group,
      opacity = 0.5,
      marker = list(color = color_pal[group]),
      hoverinfo = "x+name"
    )

    # Add density line
    #dens_df <- dens_list$df[[which(dens_list$Group == group)]]
    dens_group <- all_density |> filter(Group==group)
    fig_hist   <-  fig_hist   |> add_lines(
      x = dens_group$x,
      y = dens_group$y,
      name = group,
      legendgroup = group,
      line = list(width = 2, color=color_pal[group]),
      inherit = FALSE,
      showlegend = FALSE,
      hoverinfo = "x+name"
    )


    # Add boxplot
    box_y <- rep(box_offsets[group], length(df_group[[x]]))
    fig_box <- fig_box |> add_trace(
      x = df_group[[x]],
      orientation = "h",
      y = box_y,
      type = "box",
      #fillcolor = color_pal[group],
      # fillcolor=list(color = color_pal[group]),
      #line = list(color = color_pal[group]),
      #boxpoints = "outliers",
      boxpoints = "all",
      jitter=0,
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        symbol = "line-ns-open",
        size = 5,                    # size of the points
        color = color_pal[group],    # points color
        opacity = 0.5                # transparency
      ),
      inherit = FALSE,
      hoverinfo = "x+name"
    )


  }

  fig <- subplot(fig_hist, fig_box, nrows = 2, heights = c(0.7,0.3),shareX = TRUE, titleY = TRUE)


  fig <- fig |> layout(
    barmode = "overlay",
    title = list(
      text = paste("Observed vs multiply-imputed values for", x),
      x = 0.5,          # horizontal position (0 = left, 0.5 = center, 1 = right)
      y = 0.98,         # vertical position (1 = top, 0 = bottom)
      xanchor = "center", # anchor for x position ("left", "center", "right")
      yanchor = "top",    # anchor for y position ("top", "middle", "bottom")
      font = list(
        size = 18,
        color = "black"
        #family = "Arial"
      )
    ),
    xaxis = list(title = x),
    yaxis = list(title = "Density", showticklabels= TRUE),
    yaxis2 = list(visible=FALSE),
    title = paste("Histogram with density curve for", x),
    legend = list(x = 1,         # x position (0 = left, 1 = right)
                  y = 0.7,         # y position (0 = bottom, 1 = top)
                  xanchor = "center",  # "left", "center", "right"
                  yanchor = "middle",    # "top", "middle", "bottom"
                  orientation = "v",  # "v" = vertical, "h" = horizontal
                  itemsizing = "constant")
  )

  fig

}
