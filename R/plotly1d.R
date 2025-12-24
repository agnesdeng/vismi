plotly_1num <- function(all_dt,x,color_pal,point_size, alpha,nbins,marginal_x){

  n_levels<-nlevels(all_dt$Group)
  names_levels<-levels(all_dt$Group)

  #box_offsets <- stats::setNames(seq(0.3, 0, length.out = n_levels),names_levels)


  #set the canva: left 0 right 1 bottom 0 top 1
  main_x<-c(0,1)
  main_y<-c(0,0.65)
  mx_x <-main_x
  mx_y <-c(0.7,1)

  fig<- plot_ly()
  fig<-.plotly_histogram_density(fig=fig, all_dt=all_dt, x=x,nbins=nbins,alpha=alpha,color_pal=color_pal)

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
      # scatter
      xaxis = list(domain = main_x,title=x, gridcolor='white'),
      yaxis = list(domain = main_y,title='Density', gridcolor='white'),

      # margin_x bottom
      xaxis2 = list(domain = mx_x,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE),
      yaxis2 = list(domain = mx_y,showticklabels = FALSE, showgrid=FALSE, zeroline=FALSE)

      #showlegend = TRUE,
    )

  }

  fig<-.plotly_layout_common(fig,x=x)
  fig

}


plotly_1fac <- function(all_dt,x,color_pal,alpha){

  fig<- plot_ly()
  fig<-.plotly_bar(fig=fig, all_dt=all_dt, x=x,alpha=alpha,color_pal=color_pal)

  fig<-fig |> layout(
    barmode = "group",
    xaxis = list(title = x),
    yaxis = list(title = 'Proportion',showticklabels= TRUE)
  )

  fig<-.plotly_layout_common(fig,x=x)

  fig

}


