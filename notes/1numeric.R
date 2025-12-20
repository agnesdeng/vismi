#1 numeric
if(types=="numeric"){



  if(interactive){
    fig<-plotly_1num(data=data,imp_list=imp_list,x=vars[1],marginal_x=marginal_x,color_pal=color_pal, point_size = point_size, alpha = alpha,nbins=nbins)
  }else{
    fig<-ggplot_1num(data=data,imp_list=imp_list,x=vars[1],marginal_x=marginal_x,color_pal=color_pal, point_size = point_size, alpha = alpha,nbins=nbins)
  }
}else if(types=="factor"){

  if(interactive){
    fig<-plotly_1cat(data=data,imp_list=imp_list,x=vars[1],color_pal=color_pal, alpha = alpha)
  }else{
    fig<-ggplot_1cat(data=data,imp_list=imp_list,x=vars[1],color_pal=color_pal, alpha = alpha, width = width)
  }


}
