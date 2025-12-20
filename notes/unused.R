## ggplotly(ggplot_2fac()) version - not ideal

plotly_2fac_v0<- function(all_dt,imp_list,x,y,color_pal,alpha, width) {

  fig <- .ggplot_bar_facet1(all_dt=all_dt,x=x,y=y,color_pal=color_pal,alpha=alpha,width=width)
  fig<-plotly::ggplotly(fig)

  fig<-fig |> layout(
    boxmode = "group",
    annotations=list(x=1,y=0.5, text = y,         # The title text
                     showarrow = FALSE,                      # Hide the arrow if not needed
                     textangle = -90,                        # Rotate text vertically
                     xref = "paper",                         # Reference the whole figure area
                     yref = "paper",
                     xanchor = "left",                      # Anchor the text's right side to the x position
                     yanchor = "middle")
  )
  fig <- .plotly_layout_common(fig,x=x,y=y)
  fig
}

