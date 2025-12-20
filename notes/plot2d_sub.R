
plotly_2num<- function(data, imp_list, x, y, color_pal = NULL,
                           point_size = 3, alpha = 0.8, marginal_x="box+rug", marginal_y="box+rug") {
  #subplot version
  pre <- preprocess(data, imp_list, vars = c(x, y))
  all_dt <- pre$all_dt
  if (is.null(color_pal)) color_pal <- pre$color_pal



  fig_scatter <- plot_ly()


  fig_scatter<-.plotly_scatter(fig_scatter, all_dt,x,y,point_size,alpha,color_pal)


  # --- Top marginal boxplots (x variable) ---
  if(!is.null(marginal_x)){
    if(marginal_x=="box+rug"){
      fig_mx <- plot_ly()
      fig_mx<-.plotly_box_rug_x(fig_mx, all_dt,x,color_pal)
    }else if(marginal_x=="rug"){
      fig_mx <- plot_ly()
      fig_mx<-.plotly_rug_x(fig_mx, all_dt,x,color_pal)
    }
  }


  # --- Right marginal boxplots (y variable) ---
  if(!is.null(marginal_y)){
    if(marginal_y=="box+rug"){
      fig_my <- plot_ly()
      fig_my <-.plotly_box_rug_y(fig_my, all_dt,y,color_pal)
    }else if(marginal_y=="rug"){
      fig_my <- plot_ly()
      fig_my<-.plotly_rug_y(fig_my, all_dt,y,color_pal)
    }
  }

  if(is.null(marginal_x) & is.null(marginal_y)){
    fig<-fig_scatter |> layout(
      title = paste("Observed vs multiply-imputed values:", y, "vs", x),
      xaxis = list(title = x),
      yaxis = list(title = y),
      legend = list(
        itemsizing = "constant",
        orientation = "h",   # horizontal
        x = 0.5,             # center horizontally
        y = -0.2,            # below the plot (negative moves it down)
        xanchor = "center",
        yanchor = "top"      # top of legend box aligns with y coordinate
      ))
  }else{
    fig_empty <- plot_ly() %>%
      add_trace(
        x = 0,
        y = 0,
        type = "scatter",
        mode = "markers",
        opacity = 0,        # completely invisible
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    fig_top <- subplot(fig_mx, fig_empty, widths = c(0.8, 0.2))

    fig_bottom<-subplot(fig_scatter, fig_my, widths = c(0.8, 0.2))

    fig<-subplot(fig_top, fig_bottom, nrows = 2, heights = c(0.3, 0.7))

    fig <- fig |>
      layout(
        title = paste("Observed vs multiply-imputed values:", y, "vs", x),
        xaxis = list(showticklabels = FALSE),
        xaxis2 = list(showticklabels = FALSE),
        xaxis4 = list(showticklabels = FALSE),
        yaxis = list(showticklabels = FALSE),
        yaxis2 = list(showticklabels = FALSE),
        yaxis3 = list(showticklabels = FALSE),
        showlegend = TRUE,
        legend = list(
          itemsizing = "constant",
          orientation = "h",   # horizontal
          x = 0.5,             # center horizontally
          y = -0.2,            # below the plot (negative moves it down)
          xanchor = "center",
          yanchor = "top"      # top of legend box aligns with y coordinate
        )
      )


  }

  fig
}




plotly_2num_scatter <- function(data,imp_list,x,y,color_pal=NULL,point_size = 3, alpha=0.8){
  pre <- preprocess(data, imp_list, vars = c(x, y))
  all_dt <- pre$all_dt
  if(is.null(color_pal)){
    color_pal <- pre$color_pal
  }
  fig<-plot_ly(all_dt,
               x = as.formula(paste0("~", x)),
               y = as.formula(paste0("~", y)),
               color=~Group, colors=color_pal,
               type="scatter",mode="markers",
               marker = list(
                 size = point_size,
                 opacity = alpha
               ),
               text = ~paste("Index:", row_index, "<br>",
                             x,":", get(x), "<br>",
                             y,":",  get(y), "<br>",
                             "Group: ", Group),
               hoverinfo = "text")
  fig<-fig |>
    layout(
      title = list(
        text = paste("Observed vs multiply-imputed values:",  y, "vs", x),
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
      legend = list(x = 1,         # x position (0 = left, 1 = right)
                    y = 0.7,         # y position (0 = bottom, 1 = top)
                    xanchor = "center",  # "left", "center", "right"
                    yanchor = "middle",    # "top", "middle", "bottom"
                    orientation = "v",  # "v" = vertical, "h" = horizontal
                    itemsizing = "constant")
    )
  fig
}



ggplot_2num <- function(data,imp_list,x,y,color_pal=NULL,point_size = 1, alpha=0.8){
  pre <- preprocess(data, imp_list, vars = c(x, y))
  all_dt <- pre$all_dt
  if(is.null(color_pal)){
    color_pal <- pre$color_pal
  }
  fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = alpha, aes(color = Group , fill = Group), size = point_size) +
    facet_grid(cols = vars(Group)) +
    labs(x=x,y=y,title=paste("Observed vs multiply-imputed values:",  y, "vs", x)) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3))) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold"),
      legend.background = element_rect(linewidth = 0.5),
      legend.title = element_text(color = "black", size = 12, face = "bold"),
      legend.text = element_text(color = "black", size = 10)
    )
  fig
}


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
    hoverinfo = "text"
    #xaxis = "x1", yaxis = "y1"
  )
  fig
}



# box+rug -----------------------------------------------------------------



.plotly_box_rug_x<-function(fig,all_dt, x,color_pal){
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      x = df_group[[x]],
      type = "box",
      orientation = "h",
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
      hoverinfo = "x+name",
      xaxis = "x1",
      yaxis = "y2"  # separate y-axis for top boxplot
    )
  }
  fig
}


.plotly_box_rug_y<-function(fig,all_dt, y,color_pal){
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      y = df_group[[y]],
      type = "box",
      orientation = "v",
      boxpoints = "all",
      jitter=0,
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        symbol = "line-ew-open",
        size = 5,                    # size of the points
        color = color_pal[group],    # points color
        opacity = 0.5                # transparency
      ),
      #width=0.3,
      inherit = FALSE,
      hoverinfo = "y+name",
      xaxis = "x2",  # separate x-axis for right boxplot
      yaxis = "y1"
    )
  }
  fig
}

plotly_2num_sub<- function(data, imp_list, x, y, color_pal = NULL,
                           point_size = 3, alpha = 0.8, marginal_x="box+rug", marginal_y="box+rug") {
  #subplot version
  pre <- preprocess(data, imp_list, vars = c(x, y))
  all_dt <- pre$all_dt
  if (is.null(color_pal)) color_pal <- pre$color_pal

  fig_scatter <- plot_ly()


  fig_scatter<-.plotly_scatter(fig_scatter, all_dt,x,y,point_size,alpha,color_pal)


  # --- Top marginal boxplots (x variable) ---
  if(!is.null(marginal_x)){
    if(marginal_x=="box+rug"){
      fig_mx <- plot_ly()
      fig_mx<-.plotly_box_rug_x(fig_mx, all_dt,x,color_pal)
    }else if(marginal_x=="rug"){
      fig_mx <- plot_ly()
      fig_mx<-.plotly_rug_x(fig_mx, all_dt,x,color_pal)
    }
  }



  # --- Right marginal boxplots (y variable) ---
  if(!is.null(marginal_y)){
    if(marginal_y=="box+rug"){
      fig_my <- plot_ly()
      fig_my <-.plotly_box_rug_y(fig_my, all_dt,y,color_pal)
    }else if(marginal_y=="rug"){
      fig_my <- plot_ly()
      fig_my<-.plotly_rug_y(fig_my, all_dt,y,color_pal)
    }
  }



  if(is.null(marginal_x) & is.null(marginal_y)){
    fig<-fig_scatter |> layout(
      title = paste("Observed vs multiply-imputed values:", y, "vs", x),
      xaxis = list(title = x),
      yaxis = list(title = y),
      legend = list(
        itemsizing = "constant",
        orientation = "h",   # horizontal
        x = 0.5,             # center horizontally
        y = -0.2,            # below the plot (negative moves it down)
        xanchor = "center",
        yanchor = "top"      # top of legend box aligns with y coordinate
      ))
  }else{
    fig_empty <- plot_ly() %>%
      add_trace(
        x = 0,
        y = 0,
        type = "scatter",
        mode = "markers",
        opacity = 0,        # completely invisible
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    fig_top <- subplot(fig_mx, fig_empty, widths = c(0.8, 0.2))

    fig_bottom<-subplot(fig_scatter, fig_my, widths = c(0.8, 0.2))

    fig<-subplot(fig_top, fig_bottom, nrows = 2, heights = c(0.3, 0.7))

    fig <- fig |>
      layout(
        #paper_bgcolor="rgb(250,250,250)",
        plot_bgcolor = "rgb(245,245,245)",
        title = paste("Observed vs multiply-imputed values:", y, "vs", x),
        #scatter plot xaxis3 and yaxis4
        xaxis3 = list(linecolor ='#aaaaaa', gridcolor="#cccccc", title=x),
        yaxis4 = list(linecolor ='#aaaaaa', gridcolor="#cccccc", title=y),
        #others
        xaxis = list(showticklabels = FALSE, showgrid = FALSE),
        xaxis2 = list(showticklabels = FALSE, showgrid = FALSE),
        xaxis4 = list(showticklabels = FALSE, showgrid = FALSE),
        yaxis = list(showticklabels = FALSE, showgrid = FALSE),
        yaxis2 = list(showticklabels = FALSE, showgrid = FALSE),
        yaxis3 = list(showticklabels = FALSE, showgrid = FALSE),
        showlegend = TRUE,
        legend = list(
          itemsizing = "constant",
          orientation = "h",   # horizontal
          x = 0.5,             # center horizontally
          y = -0.2,            # below the plot (negative moves it down)
          xanchor = "center",
          yanchor = "top"      # top of legend box aligns with y coordinate
        )
      )


  }

  fig
}




plotly_2num_scatter <- function(data,imp_list,x,y,color_pal=NULL,point_size = 3, alpha=0.8){
  pre <- preprocess(data, imp_list, vars = c(x, y))
  all_dt <- pre$all_dt
  if(is.null(color_pal)){
    color_pal <- pre$color_pal
  }
  fig<-plot_ly(all_dt,
               x = as.formula(paste0("~", x)),
               y = as.formula(paste0("~", y)),
               color=~Group, colors=color_pal,
               type="scatter",mode="markers",
               marker = list(
                 size = point_size,
                 opacity = alpha
               ),
               text = ~paste("Index:", row_index, "<br>",
                             x,":", get(x), "<br>",
                             y,":",  get(y), "<br>",
                             "Group: ", Group),
               hoverinfo = "text")
  fig<-fig |>
    layout(
      title = list(
        text = paste("Observed vs multiply-imputed values:",  y, "vs", x),
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
      legend = list(x = 1,         # x position (0 = left, 1 = right)
                    y = 0.7,         # y position (0 = bottom, 1 = top)
                    xanchor = "center",  # "left", "center", "right"
                    yanchor = "middle",    # "top", "middle", "bottom"
                    orientation = "v",  # "v" = vertical, "h" = horizontal
                    itemsizing = "constant")
    )
  fig
}



ggplot_2num <- function(data,imp_list,x,y,color_pal=NULL,point_size = 1, alpha=0.8){
  pre <- preprocess(data, imp_list, vars = c(x, y))
  all_dt <- pre$all_dt
  if(is.null(color_pal)){
    color_pal <- pre$color_pal
  }
  fig <- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = alpha, aes(color = Group , fill = Group), size = point_size) +
    facet_grid(cols = vars(Group)) +
    labs(x=x,y=y,title=paste("Observed vs multiply-imputed values:",  y, "vs", x)) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3))) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold"),
      legend.background = element_rect(linewidth = 0.5),
      legend.title = element_text(color = "black", size = 12, face = "bold"),
      legend.text = element_text(color = "black", size = 10)
    )
  fig
}


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
    hoverinfo = "text"
    #xaxis = "x1", yaxis = "y1"
  )
  fig
}



# box+rug -----------------------------------------------------------------



.plotly_box_rug_x<-function(fig,all_dt, x,color_pal){
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      x = df_group[[x]],
      type = "box",
      orientation = "h",
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
  fig
}


.plotly_box_rug_y<-function(fig,all_dt, y,color_pal){
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> filter(Group == group)
    fig <- fig |> add_trace(
      y = df_group[[y]],
      type = "box",
      orientation = "v",
      boxpoints = "all",
      jitter=0,
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      color = I(color_pal[group]),
      marker = list(
        symbol = "line-ew-open",
        size = 5,                    # size of the points
        color = color_pal[group],    # points color
        opacity = 0.5                # transparency
      ),
      #width=0.3,
      inherit = FALSE,
      hoverinfo = "y+name"
    )
  }
  fig
}


# rug ---------------------------------------------------------------------
.plotly_rug_x <- function(fig, all_dt, x, color_pal) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> dplyr::filter(Group == group)

    fig <- fig |> add_trace(
      x = df_group[[x]],
      y = rep(0, nrow(df_group)),     # collapse vertically
      type = "scatter",
      mode = "markers",
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      marker = list(
        symbol = "line-ns-open",      # vertical ticks
        size   = 6,
        color  = color_pal[group],
        opacity = 0.6
      ),
      hoverinfo = "x+name",
      inherit = FALSE
      #xaxis = "x1",
      #yaxis = "y2"                     # top rug axis
    )
  }
  fig
}



.plotly_rug_y <- function(fig, all_dt, y, color_pal) {
  for (group in levels(all_dt$Group)) {
    df_group <- all_dt |> dplyr::filter(Group == group)

    fig <- fig |> add_trace(
      x = rep(0, nrow(df_group)),     # collapse horizontally
      y = df_group[[y]],
      type = "scatter",
      mode = "markers",
      name = group,
      showlegend = FALSE,
      legendgroup = group,
      marker = list(
        symbol = "line-ew-open",      # horizontal ticks
        size   = 6,
        color  = color_pal[group],
        opacity = 0.6
      ),
      hoverinfo = "y+name",
      inherit = FALSE
      #xaxis = "x2",                    # right rug axis
      #yaxis = "y1"
    )
  }
  fig
}


