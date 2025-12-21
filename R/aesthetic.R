vismi_colors<-function(N_imp){
  color_pal <- c("#666666", hue_pal()(N_imp))
  #names(color_pal)<-levels(all_dt$Group)
  color_pal
}


vismi_overimp_colors<-function(N_imp){
  colors <-c("#666666",hue_pal()(N_imp))
  colors
}


vismi_overimp_train_colors<-function(N_imp){
  int_colors = c("#002cb3","#85aeff")
  colfunc <- grDevices::colorRampPalette(int_colors)
  colors <-c(colfunc(N_imp), "gray40")
  colors
}


vismi_overimp_test_colors<-function(N_imp){
  int_colors = c("#cc7700","#ffd24d")
  colfunc <- grDevices::colorRampPalette(int_colors)
  colors <-c(colfunc(N_imp), "gray40")
  colors
}









.ggplot_theme<-function(fig){
  fig+
  #guides(fill = "none", color = "none",shape = guide_legend(override.aes = list(size = 3)))
  #guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3)))+
  theme(panel.background = element_rect(fill = "gray95", colour = NA),
        #strip.background = element_rect(fill = "white", colour = NA),
        # soft grid lines
        panel.grid.major = element_line(colour = "white", linewidth = 0.3),
        panel.grid.minor = element_line(colour = "white", linewidth = 0.2),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "bold"),
        legend.position = "none")
}




.plotly_overimp_theme<-function(fig,x,y=NULL,z=NULL){

  if(is.null(y) & is.null(z)){
    title_text <- paste("Masked true vs multiply-imputed values:", x)
  }else if(is.null(z)){
    title_text <- paste("Masked true vs multiply-imputed values:", y, "vs", x)
  }else{
    title_text <- paste0(
      "<span style='font-size:18px; font-weight:600;'>",
      "Masked true  vs multiply-imputed values: ", y, " vs ", x,
      "</span><br>",  # <- extra <br> adds space between title and subtitle
      "<span style='font-size:13px;'>",
      "Faceted by ", z,
      "</span>"
    )
  }


}

.ggplot_overimp_theme<-function(fig, showlegend=TRUE){

  if(showlegend==FALSE){
    fig<-fig+ guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3)))
  }

  fig+
    theme(panel.background = element_rect(fill = "gray95", colour = NA),
          panel.grid.major = element_line(colour = "white", linewidth = 0.3),
          panel.grid.minor = element_line(colour = "white", linewidth = 0.2),
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10),
          axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
          axis.title.y = element_text(size = 10, margin = margin(0, r = 5, 0, l = 0)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "right",   # or "bottom", "top", "left", "none"
          legend.title = element_text(size = 10),#face="bold"
          legend.text = element_text(size = 8),
          strip.text.y.left = element_text(angle = 0))
}

.ggplot_overimp_theme2<-function(fig){
  fig+
    theme(
    #plot.title = element_blank(),
    #plot.subtitle = element_blank(),
    plot.title = element_text(size=28),
    plot.subtitle = element_text(size=26),
    strip.text = element_text(size = 23, face = "plain"),
    axis.title.x = element_text(size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
    axis.title.y = element_text(size = 23, margin = margin(0, r = 5, 0, l = 0)),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    panel.spacing.x = unit(0.4, "cm")
  )


}



.plotly_layout_common<- function(fig, x, y=NULL, z=NULL) {

  if(is.null(y) & is.null(z)){
    title_text <- paste("Observed vs multiply-imputed values:", x)
  }else if(is.null(z)){
    title_text <- paste("Observed vs multiply-imputed values:", y, "vs", x)
  }else{
    title_text <- paste0(
      "<span style='font-size:18px; font-weight:600;'>",
      "Observed vs multiply-imputed values: ", y, " vs ", x,
      "</span><br>",  # <- extra <br> adds space between title and subtitle
      "<span style='font-size:13px;'>",
      "Faceted by ", z,
      "</span>"
    )

  }



  fig<-fig |> layout(
    plot_bgcolor="#f2f7fc",
    title = list(
      text = title_text,
      x = 0.5,          # horizontal position (0 = left, 0.5 = center, 1 = right)
      y = 0.98,         # vertical position (1 = top, 1 = bottom)
      xanchor = "center", # anchor for x position ("left", "center", "right")
      yanchor = "top",    # anchor for y position ("top", "middle", "bottom")
      font = list(
        size = 18,
        color = "black"
        #family = "Arial"
      )),
    margin = list(r=50, t=50), # increase right and top margin to avoid cutoffs
    legend = list(x = 0.5,         # x position (0 = left, 1 = right)
                  y = -0.2,         # y position (0 = bottom, 1 = top)
                  xanchor = "center",  # "left", "center", "right"
                  yanchor = "top",    # "top", "middle", "bottom"
                  orientation = "h",  # "v" = vertical, "h" = horizontal
                  itemsizing = "constant")
  )

  fig
}



.plotly_layout_scatter3d<- function(fig, x, y, z) {

  title_text <- paste("Observed vs multiply-imputed values:",  y, "vs", x, "vs", z)

  fig<-fig |> layout(
    scene = list(
      #bgcolor = "#f2f7fc",  # canvas rectangle box area color
      #change the background of the 3D box
      xaxis = list(showbackground = TRUE,backgroundcolor = "#f2f7fc",gridcolor = "gray60"),
      yaxis = list(showbackground = TRUE,backgroundcolor = "#f2f7fc",gridcolor = "gray60"),
      zaxis = list(showbackground = TRUE,backgroundcolor = "#f2f7fc",gridcolor = "gray60")
    ),
    paper_bgcolor = "#fff", # the whole paper
    #plot_bgcolor="#f2f7fc", # has no effect here
    title = list(
      text = title_text,
      x = 0.5,          # horizontal position (0 = left, 0.5 = center, 1 = right)
      y = 0.98,         # vertical position (1 = top, 1 = bottom)
      xanchor = "center", # anchor for x position ("left", "center", "right")
      yanchor = "top",    # anchor for y position ("top", "middle", "bottom")
      font = list(
        size = 18,
        color = "black"
        #family = "Arial"
      )),
    margin = list(r=50, t=50), # increase right and top margin to avoid cutoffs
    legend = list(x = 0.5,         # x position (0 = left, 1 = right)
                  y = -0.2,         # y position (0 = bottom, 1 = top)
                  xanchor = "center",  # "left", "center", "right"
                  yanchor = "top",    # "top", "middle", "bottom"
                  orientation = "h",  # "v" = vertical, "h" = horizontal
                  itemsizing = "constant")
  )

  fig
}


