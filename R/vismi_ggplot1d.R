ggplot_1num <- function(all_dt,x,color_pal,point_size, alpha,nbins,marginal_x){

  fig<-.ggplot_histogram_density(all_dt=all_dt, x=x, color_pal=color_pal,nbins=nbins)

  if(!is.null(marginal_x)){
    if(marginal_x=="box+rug"){
      fig<-.ggplot_box_1d(fig, all_dt,x,color_pal,y_box_offset= - 0.02, box_width = 0.01)
      fig<-.ggplot_rug_1d(fig, all_dt,x,color_pal,y_rug_offset= -0.05)
    }else if(marginal_x=="box"){
      fig<-.ggplot_box_1d(fig, all_dt,x,color_pal,y_box_offset= - 0.02, box_width = 0.01)
    }else if(marginal_x=="rug"){
      fig<-.ggplot_rug_1d(fig, all_dt,x,color_pal,y_rug_offset= -0.02)
    }else{
      stop("Invalid marginal_y option. Please choose from 'box+rug', 'box', or 'rug'.")
    }
  }

  fig<-.ggplot_theme(fig)
  fig

}




ggplot_1fac <- function(all_dt,x,color_pal=NULL,alpha=0.8,width=0.8){

  fig<-.ggplot_bar(all_dt=all_dt, x=x, color_pal=color_pal,alpha=alpha, width=width)


  fig<-.ggplot_theme(fig)
  fig

}




