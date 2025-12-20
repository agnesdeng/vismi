


plotly_3num <- function(all_dt, x,y,z,color_pal,point_size, alpha){

  fig<-plot_ly(all_dt,
               x = as.formula(paste0("~", x)),
               y = as.formula(paste0("~", y)),
               z = as.formula(paste0("~", z)),
               color=~Group, colors=color_pal,
               type="scatter3d",mode="markers",
               marker = list(
                 size = point_size,
                 opacity = alpha
               ),
               text = ~paste("Index:", row_index, "<br>",
                             x,":", get(x), "<br>",
                             y,":",  get(y), "<br>",
                             z, ":", get(z), "<br>",
                             "Group: ", Group),
               hoverinfo = "text")
  fig<-.plotly_layout_scatter3d(fig,x=x,y=y,z=z)
  fig

}





plotly_1fac2num<- function(all_dt, x, y, z, color_pal,
                           point_size, alpha,boxpoints) {

  fig<-.plotly_scatter_facet(all_dt=all_dt,x=x,y=y,z=z,color_pal=color_pal, point_size=point_size,alpha=alpha)

  fig <-.plotly_layout_common(fig,x=x,y=y,z=z)
  fig
}



plotly_2fac1num<- function(all_dt, x, y, z, color_pal,
                           point_size, alpha, boxpoints) {

  fig<-.plotly_box_facet(all_dt=all_dt,x=x,y=y,z=z,color_pal=color_pal, point_size=point_size,alpha=alpha, boxpoints = boxpoints)

  fig<-.plotly_layout_common(fig,x=x,y=y,z=z)
  fig
}



plotly_3fac<- function() {
   stop("3D factor plots are not supported yet.")
}


