# 1D histogram-----------------------------------------------------------------
.plotly_histogram_density<-function(fig, all_dt,x,nbins,alpha,color_pal){

  if(is.null(nbins)){
    breaks <- pretty(range(all_dt[[x]]),
                     n = grDevices::nclass.Sturges(all_dt[[x]]),
                     min.n = 1
    )
    nbins<-length(breaks)
  }

  # Compute density per group
  all_density <- all_dt |>
    group_by(Group) |>
    reframe({
      out <- density(.data[[x]], na.rm = TRUE)
      tibble(x = out$x, y = out$y)
    })

  for(group in levels(all_dt$Group)){
    df_group <- all_dt |> filter(Group == group)

    fig  <-  fig  |> add_histogram(
      x = df_group[[x]],
      histnorm = "probability density",   # <-- this scales histogram to density
      nbinsx = nbins,
      name = group,
      legendgroup = group,
      opacity = alpha,
      marker = list(color = color_pal[group]),
      hoverinfo = "x+name",
      xaxis = "x", yaxis = "y"
    )

    dens_group <- all_density |> filter(Group==group)
    fig  <-  fig  |> add_lines(
      x = dens_group$x,
      y = dens_group$y,
      name = group,
      legendgroup = group,
      line = list(width = 2, color=color_pal[group]),
      inherit = FALSE,
      showlegend = FALSE,
      hoverinfo = "x+name",
      xaxis = "x", yaxis = "y"
    )
  }

  fig



}

# 1D bar----------------------------------------------------------------
.plotly_bar<-function(fig, all_dt,x,alpha,color_pal){

  all_sum <- all_dt |>
    group_by(Group, .data[[x]]) |>
    summarise(count = n(), .groups = "drop") |>
    tidyr::complete(
      Group = unique(all_dt$Group),
      !!sym(x) := unique(all_dt[[x]]),
      fill = list(count = 0)
    ) |>
    group_by(Group) |>
    mutate(prop = count / sum(count)) |>
    ungroup()

  for (level in levels(all_sum[[x]])){
    df_class <- all_sum |> filter((.data[[x]] == level))
    fig<- fig |> add_trace(
      data = df_class,
      x = ~get(x),
      #y= ~count,
      y = ~prop,
      color = ~Group,
      colors = color_pal,
      type = "bar",
      opacity = alpha,
      legendgroup = ~Group,
      showlegend = ifelse(level == levels(all_sum[[x]])[1], TRUE, FALSE),
      text = ~paste(x, "count:", get(x), "<br>",
                    "Group:", Group),
      hoverinfo = "text",
      xaxis = "x",
      yaxis = "y"
    )
  }
  fig


  fig <- fig |> layout(
    barmode = "group",
    xaxis = list(title = x),
    yaxis = list(title = "Proportion")
  )
  fig
}





# 2D scatter-----------------------------------------------------------------
.plotly_scatter<-function(fig, all_dt,x,y,point_size,alpha,color_pal){
  fig<-fig |> add_trace(
    data = all_dt,
    x = ~get(x),
    y = ~get(y),
    color = ~Group,
    colors = color_pal,
    type = "scatter",
    mode = "markers",
    marker = list(size = point_size, opacity = alpha),
    legendgroup =~Group,
    showlegend = TRUE,
    text = ~paste("Index:", row_index, "<br>",
                  x, ":", get(x), "<br>",
                  y, ":", get(y), "<br>",
                  "Group:", Group),
    hoverinfo = "text",
    xaxis = "x", yaxis = "y"
  )
  fig
}


# 2D box ------------------------------------------------------------------
.plotly_box<-function(fig, all_dt,x,y,point_size,alpha,color_pal,boxpoints){
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig<-fig |> add_trace(
      data = df_group,
      x = ~get(x),
      y = ~get(y),
      color = ~Group,
      colors = color_pal,
      type = "box",
      boxpoints = boxpoints,
      marker = list(size = point_size, opacity = alpha, color = color_pal[group]),
      name=group,
      legendgroup =group,
      showlegend = TRUE,
      text = ~paste("Index:", row_index, "<br>",
                    x, ":", get(x), "<br>",
                    y, ":", get(y), "<br>",
                    "Group:", Group),
      hoverinfo = "text",
      xaxis = "x", yaxis = "y"
    )
  }
  fig
}


# 2D bar----------------------------------------------------------------
.plotly_bar_facet1<-function(all_dt,x,y,color_pal,alpha){

  all_sum <- all_dt |> group_by(Group,.data[[x]], .data[[y]]) |>
    summarise(count = n(), .groups = "drop")  |>
    tidyr::complete(
      Group = unique(all_dt$Group),
      !!sym(x) := unique(all_dt[[x]]),
      !!sym(y) := unique(all_dt[[y]]),
      fill = list(count = 0)
    ) |>
    group_by(Group) |>
    mutate(prop = count / sum(count)) |>
    ungroup()


  y_levels <- levels(all_sum[[y]])

  plots <- lapply(y_levels, function(y_level) {
    df_sub <- all_sum |> filter(.data[[y]] == y_level)
    plot_ly(
      data = df_sub,
      x = ~get(x),
      y = ~prop,
      color = ~Group,
      colors = color_pal,
      type = "bar",
      opacity = alpha,
      legendgroup = ~Group,
      showlegend = ifelse(y_level == y_levels[1], TRUE, FALSE),
      text = ~paste(x, ":", get(x), "<br>",
                    y, ":", get(y), "<br>",
                    "Proportion:", prop, "<br>"),
      textposition="none",
      hovertext = ~paste(x, ":", get(x), "<br>",
                    y, ":", get(y), "<br>",
                    "Proportion:", prop, "<br>",
                    "Group:", Group),
      hoverinfo = "text"
    )|>
      layout(xaxis=list(title=x),
             yaxis = list(title = "Proportion", range = c(0, 1), dtick = 0.1, side = "left")
      )

  })



  combine <- subplot(
    plots,
    nrows = length(plots),
    #shareX = TRUE,
    #shareY = TRUE,
    titleY=TRUE,
    titleX=TRUE
  )
  combine <- combine|>layout(
    barmode = "group")


  k <- length(y_levels)

  annotations <- lapply(seq_along(y_levels), function(i) {
    list(
      xref = "paper",
      yref = "paper",
      x = 1,
      y = (i - 0.5) / k,
      text = paste(y, "=", y_levels[i]),
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      textangle = -90,
      font = list(size = 12)
    )
  })

  combine <-combine |> layout(annotations = annotations)
  combine



}

# 3d scatter facet----------------------------------------------------------------
.plotly_scatter_facet<-function(all_dt,x,y,z,color_pal,alpha,point_size){

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
      legendgroup =~Group,
      showlegend = ifelse(z_level == z_levels[1], TRUE, FALSE),
      text = ~paste("Index:", row_index, "<br>",
                    x, ":", get(x), "<br>",
                    y, ":", get(y), "<br>",
                    "Group:", Group),
      hoverinfo = "text"
    )|>
      layout(xaxis=list(title=x),
             yaxis = list(title = y, side = "left")
      )
  })


  combine <- subplot(
    plots,
    nrows = length(plots),
    shareX = TRUE,
    shareY = TRUE,
    titleY=TRUE,
    titleX=TRUE
  )
  combine <- combine|>layout(
    barmode = "group")


  k <- length(z_levels)

  annotations <- lapply(seq_along(z_levels), function(i) {
    list(
      xref = "paper",
      yref = "paper",
      x = 1,
      y = (i - 0.5) / k,
      text = paste(z, "=", z_levels[i]),
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      textangle = -90,
      font = list(size = 12)
    )
  })

  combine <-combine |> layout(annotations = annotations)
  combine

}



# 3d box facet----------------------------------------------------------------
.plotly_box_facet<-function(all_dt,x,y,z,color_pal,alpha,point_size,boxpoints){

  z_levels <- levels(all_dt[[z]])

  plots <- lapply(z_levels, function(z_level) {
    df_sub <- all_dt |> filter(.data[[z]] == z_level)
    fig<-plot_ly()
    for (group in levels(df_sub$Group)) {
      df_group <- df_sub |> filter(Group == group)
      fig<-fig |> add_trace(
        data = df_group,
        x = df_group[[x]],
        y = df_group[[y]],
        color = ~Group,
        colors = color_pal,
        type = "box",
        boxpoints = boxpoints,
        marker = list(size = point_size, opacity = alpha, color = color_pal[group]),
        name=group,
        legendgroup =group,
        showlegend = (z_level == z_levels[1]),
        text = ~paste("Index:", row_index, "<br>",
                      x, ":", get(x), "<br>",
                      y, ":", get(y), "<br>",
                      "Group:", Group),
        hoverinfo = "text"
      )
    }
    fig|>
      layout(xaxis=list(title=x),
             yaxis = list(title = y, side = "left")
      )
  })

  combine <- subplot(
    plots,
    nrows = length(plots),
    shareX = TRUE,
    shareY = TRUE,
    titleY=TRUE,
    titleX=TRUE
  )

  title <-paste("Observed vs multiply-imputed values:", y, "vs", x)
  subtitle = paste("Faceted by", z)

  combine <- combine|>layout(
    boxmode = "group"
  )


  k <- length(z_levels)

  annotations <- lapply(seq_along(z_levels), function(i) {
    list(
      xref = "paper",
      yref = "paper",
      x = 1,
      y = (i - 0.5) / k,
      text = paste(z, "=", z_levels[i]),
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      textangle = -90,
      font = list(size = 12)
    )
  })

  combine <-combine |> layout(annotations = annotations)
  combine


}



