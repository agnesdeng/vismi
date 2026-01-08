plotly_1num <- function(all_dt,x,color_pal,point_size, alpha,nbins,marginal_x, plot_title){

  n_levels<-nlevels(all_dt$Group)
  names_levels<-levels(all_dt$Group)

  #box_offsets <- stats::setNames(seq(0.3, 0, length.out = n_levels),names_levels)


  #set the canva: left 0 right 1 bottom 0 top 1
  main_x<-c(0,1)
  main_y<-c(0,0.65)
  mx_x <-main_x
  mx_y <-c(0.7,1)

  #main fig
  fig<- plot_ly()
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
    dens_group <- all_density |> filter(Group==group)

    if(nrow(df_group)==0){
      #cli::cli_inform("Skipping group {.val {group}}: no data available for plotting histogram.")
      next
    }

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

  #marginals
  if(!is.null(marginal_x)){
    if(marginal_x=="box+rug"){
      fig<-.plotly_box_rug_x(fig,all_dt, x,color_pal)
    }else if(marginal_x=="box"){
      fig<-.plotly_box_x(fig, all_dt,x,color_pal)
    }else if(marginal_x=="rug"){
      mx_offsets <- stats::setNames(seq(mx_y[1], mx_y[2], length.out = n_levels),
                                    names_levels)

      fig<-.plotly_rug_x(fig, all_dt,x,color_pal,mx_offsets)
    }else{
      stop("Invalid marginal_y option. Please choose from 'box+rug', 'box', or 'rug'.")
    }
  }


  if(is.null(marginal_x)){
    fig<-fig |> layout(
      xaxis = list(title = x),
      yaxis = list(title = 'Density',showticklabels= TRUE)
    )
  }else{
    fig <- fig |> layout(
      #
      xaxis = list(domain = main_x,title=x, gridcolor='white'),
      yaxis = list(domain = main_y,title='Density', gridcolor='white'),

      # margin_x bottom
      xaxis2 = list(domain = mx_x,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE),
      yaxis2 = list(domain = mx_y,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE)

      #showlegend = TRUE,
    )

  }

  fig<-.plotly_layout_common(fig,plot_title)
  fig

}


plotly_1fac <- function(all_dt,x,color_pal,alpha, plot_title){

  fig<- plot_ly()
  all_sum <- all_dt |>
    group_by(Group, .data[[x]]) |>
    summarise(count = n(), .groups = "drop") |>
    tidyr::complete(
      Group = unique(all_dt$Group),
      !!sym(x) := unique(all_dt[[x]]),
      fill = list(count = 0)
    ) |>
    group_by(Group) |>
    mutate(prop = count / sum(count),
           prop = ifelse(is.nan(prop),0,prop)) |>
    ungroup()

  for (level in levels(all_sum[[x]])){
    df_class <- all_sum |> filter((.data[[x]] == level))
    df_class_nonzero <- df_class |> filter(prop > 0)

    fig<- fig |> add_trace(
      data = df_class_nonzero,
      x = ~get(x),
      #y= ~count,
      y = ~prop,
      color = ~Group,
      colors = color_pal,
      type = "bar",
      textposition = "none",
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


  fig<-fig |> layout(
    barmode = "group",
    xaxis = list(title = x),
    yaxis = list(title = 'Proportion',showticklabels= TRUE)
  )

  fig<-.plotly_layout_common(fig, plot_title)

  fig

}


