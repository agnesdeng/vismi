

ggplot_2num <- function(all_dt,x,y,color_pal,point_size, alpha,marginal_x, marginal_y){

  fig <- .ggplot_scatter(all_dt,x,y,point_size,alpha,color_pal)

  if(!is.null(marginal_x)){
    if(marginal_x=="box+rug"){
      fig<-.ggplot_box_rug_x(fig, all_dt,x,y,color_pal)
    }else if(marginal_x=="box"){
      fig<-.ggplot_box_x(fig, all_dt,x,y,color_pal)
    }else if(marginal_x=="rug"){
      fig<-.ggplot_rug_x(fig, all_dt,x,y,color_pal)
    }else if(marginal_x=="hist"){
      stop("Static histogram marginal plots are not supported. Please set interactive = TRUE.")
    }else{
      stop("Invalid marginal_x option. Please choose from 'box+rug', 'box', or 'rug'.")
    }
  }

  if(!is.null(marginal_y)){
    if(marginal_y=="box+rug"){
      fig<-.ggplot_box_rug_y(fig, all_dt,x,y,color_pal)
    }else if(marginal_y=="box"){
      fig<-.ggplot_box_y(fig, all_dt,x,y,color_pal)
    }else if(marginal_y=="rug"){
      fig<-.ggplot_rug_y(fig, all_dt,x,y,color_pal)
    }else if(marginal_y=="hist"){
      stop("Static histogram marginal plots are not supported. Please set interactive = TRUE.")
    }else{
      stop("Invalid marginal_y option. Please choose from 'box+rug', 'box', or 'rug'.")
    }
  }

  fig<-.ggplot_theme(fig)
  fig

}


ggplot_1fac1num <- function(all_dt,x,y,color_pal,point_size, alpha, boxpoints){

  fig <- .ggplot_box(all_dt,x,y,point_size,alpha,color_pal,boxpoints)

  fig<-.ggplot_theme(fig)
  fig

}



ggplot_2fac <- function(all_dt,x,y,color_pal,alpha, width){

  fig <- .ggplot_bar_facet1(all_dt,x,y,color_pal,alpha, width)

  fig<-.ggplot_theme(fig)
  fig

}

