
ggplot_3num <-function(){
  stop("3D static plots are not supported yet. Please set interactive = TRUE.")
}


ggplot_1fac2num <- function(all_dt,x,y,z,color_pal,point_size, alpha){


  fig <- .ggplot_scatter_facet(all_dt=all_dt,x=x,y=y,z=z,color_pal=color_pal, point_size=point_size,alpha=alpha)

  fig<-.ggplot_theme(fig)
  fig

}



ggplot_2fac1num <- function(all_dt,x,y,z,color_pal,point_size, alpha,boxpoints){


  fig <- .ggplot_box_facet(all_dt=all_dt,x=x,y=y,z=z,color_pal=color_pal, point_size=point_size,alpha=alpha,boxpoints=boxpoints)

  fig<-.ggplot_theme(fig)
  fig

}


ggplot_3fac <-function(){
  stop("3D factor plots are not supported yet.")
}

