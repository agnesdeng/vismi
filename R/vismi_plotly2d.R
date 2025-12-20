
plotly_2num<- function(all_dt, imp_list, x, y, color_pal,
                       point_size, alpha, marginal_x, marginal_y,nbins) {


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


  fig<-.plotly_scatter(fig=fig, all_dt=all_dt,x=x,y=y,point_size=point_size,alpha=alpha,color_pal=color_pal)


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

  fig<-.plotly_layout_common(fig,x=x,y=y)
  fig
}



plotly_1fac1num<- function(all_dt, imp_list, x, y, color_pal,
                       point_size, alpha,boxpoints) {



  fig <- plot_ly()


  fig<-.plotly_box(fig=fig, all_dt=all_dt,x=x,y=y,point_size=point_size,alpha=alpha,color_pal=color_pal,boxpoints=boxpoints)

  fig<-fig |> layout(
   boxmode = "group",
    xaxis = list(title = x),
    yaxis = list(title = y,showticklabels= TRUE)
  )

  fig <-.plotly_layout_common(fig,x=x,y=y)
  fig
}



plotly_2fac<- function(all_dt,imp_list,x,y,color_pal,alpha) {

  fig <- .plotly_bar_facet1(all_dt=all_dt,x=x,y=y,color_pal=color_pal,alpha=alpha)

  fig <-.plotly_layout_common(fig,x=x,y=y)
  fig
}





