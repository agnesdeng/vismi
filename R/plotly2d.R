
plotly_2num<- function(all_dt, imp_list, x, y, color_pal,
                       point_size, alpha, marginal_x, marginal_y,nbins, plot_title) {


  #set the canva: left 0 right 1 bottom 0 top 1
  main_x<-c(0,0.8)
  main_y<-c(0,0.7)
  mx_x <-main_x
  mx_y <-c(0.72,1)
  my_x<-c(0.82,1)
  my_y <-main_y

  n_levels<-nlevels(all_dt$Group)
  names_levels<-levels(all_dt$Group)

  fig <- plot_ly()


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


  # --- Top marginal boxplots (x variable) ---
  if(!is.null(marginal_x)){
    if(marginal_x=="box+rug"){
      fig<-.plotly_box_rug_x(fig, all_dt,x,color_pal)
    }else if(marginal_x=="box"){
      fig<-.plotly_box_x(fig, all_dt,x,color_pal)
    }else if(marginal_x=="rug"){
      mx_offsets <- stats::setNames(seq(mx_y[1], mx_y[2], length.out = n_levels),
                                    names_levels)
      fig<-.plotly_rug_x(fig, all_dt,x,color_pal,mx_offsets)
    }else if(marginal_x=="hist"){
      fig<-.plotly_hist_x(fig, all_dt,x,color_pal,nbins)
    }
  }


  # --- Right marginal boxplots (y variable) ---
  if(!is.null(marginal_y)){
    if(marginal_y=="box+rug"){
      fig<-.plotly_box_rug_y(fig, all_dt,y,color_pal)
    }else if(marginal_y=="box"){
      fig<-.plotly_box_y(fig, all_dt,y,color_pal)
    }else if(marginal_y=="rug"){
      my_offsets <- stats::setNames(seq(my_x[1], my_x[2], length.out = n_levels),
                                    names_levels)
      fig<-.plotly_rug_y(fig, all_dt,y,color_pal,my_offsets)
    }else if(marginal_y=="hist"){
      fig<-.plotly_hist_y(fig, all_dt,y,color_pal,nbins)
    }
  }

  if(is.null(marginal_x) & is.null(marginal_y)){
    fig<-fig |> layout(
      xaxis = list(title = x),
      yaxis = list(title = y))
  }else{
    fig <- fig |> layout(
      # scatter
      xaxis = list(domain = main_x,title=x, gridcolor='white'),
      yaxis = list(domain = main_y,title=y, gridcolor='white'),

      # margin_x top-left
      xaxis2 = list(domain = mx_x,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE),
      yaxis2 = list(domain = mx_y,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE),

      # margin_y right-bottom
      xaxis3 = list(domain = my_x,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE),
      yaxis3 = list(domain = my_y,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE)
      #showlegend = TRUE,
    )

  }

  fig<-.plotly_layout_common(fig, plot_title)
  fig
}



plotly_1fac1num<- function(all_dt, imp_list, x, y, color_pal,
                       point_size, alpha,boxpoints, plot_title) {

  fig <- plot_ly()

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

  fig<-fig |> layout(
   boxmode = "group",
    xaxis = list(title = x),
    yaxis = list(title = y,showticklabels= TRUE)
  )

  fig <-.plotly_layout_common(fig, plot_title)
  fig
}



plotly_2fac<- function(all_dt,imp_list,x,y,color_pal,alpha, plot_title) {

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
      #layout(xaxis=list(title=list(text=x,standoff = 10), automargin=FALSE),
      layout(xaxis=list(title="", automargin=FALSE),
             yaxis = list(title = "", range = c(0, 1), dtick = 0.1, side = "left")
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





  k <- length(y_levels)

  annotations <- lapply(seq_along(y_levels), function(i) {
    list(
      xref = "paper",
      yref = "paper",
      x = 1,
      y = (i - 0.5) / k,
      text = y_levels[i],
      #text = paste(y, "=", y_levels[i]),
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      textangle = -90,
      font = list(size = 12)
    )
  })



  prop_axis_title <- list(
    text = "Proportion",
    xref = "paper",
    yref = "paper",
    xanchor = "right",
    yanchor = "middle",
    x = 0,
    y = 0.5,
    xshift = -30,
    showarrow = FALSE,
    textangle = -90,
    font = list(size = 14)
  )

  y_axis_title <- list(
    text = y,
    xref = "paper",
    yref = "paper",
    xanchor = "left",
    yanchor = "middle",
    x = 1,
    y = 0.5,
    xshift = 30,
    showarrow = FALSE,
    textangle = -90,
    font = list(size = 14)
  )

  x_axis_title <- list(
    text = x,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "top",
    x = 0.5,
    y = -0.05,
    #yshift = -30,
    showarrow = FALSE,
    font = list(size = 14)
  )


  combine <-combine |> layout(annotations = c(annotations, list(prop_axis_title, y_axis_title,x_axis_title)))


  fig <-.plotly_layout_common(combine, plot_title)
  fig
}





