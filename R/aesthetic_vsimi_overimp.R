.vismi_overimp_train_colors <- function(N_imp) {
  int_colors <- c("#002cb3", "#85aeff")
  colfunc <- grDevices::colorRampPalette(int_colors)
  colors <- c(colfunc(N_imp), "gray40")
  colors
}


.vismi_overimp_test_colors <- function(N_imp) {
  int_colors <- c("#cc7700", "#ffd24d")
  colfunc <- grDevices::colorRampPalette(int_colors)
  colors <- c(colfunc(N_imp), "gray40")
  colors
}


# overimp theme -----------------------------------------------------------
.vismi_overimp_style <- function() {
  list(
    title_color = "#242429",
    title_size = 14,
    title_face = "plain",
    subtitle_color = "#242429",
    subtitle_size = 14,
    subtitle_face = "plain",
    axis_title_color = "#35353d",
    axis_title_size = 10,
    axis_title_face = "bold",
    axis_text_size = 9,
    panel_bg_fill = "gray95",
    panel_bg_color = NA,
    strip_bg_fill = "gray85",
    strip_bg_color = NA,
    strip_text_size = 8,
    grid_major_color = "white",
    grid_major_linewidth = 0.3,
    grid_minor_color = "white",
    grid_minor_linewidth = 0.2
  )
}

.ggplot_overimp_theme <- function(fig, showlegend = TRUE, markdown_axis_titles = FALSE) {

  style <- .vismi_overimp_style()

  if (showlegend == FALSE) {
    fig <- fig + guides(fill = "none", color = "none")
  } else {
    fig <- fig + guides(color = guide_legend(override.aes = list(size = 3)))
  }

  fig<-fig +
    theme(
      #panel.background = element_rect(fill = "#EBEBEB", colour = NA),
      #panel.grid.major = element_line(color = "white"),
      #panel.grid.minor = element_line(color = "white", linewidth = 0.25),
      panel.background = element_rect(fill = style$panel_bg_fill, colour = style$panel_bg_color),
      panel.grid.major = element_line(colour = style$grid_major_color, linewidth = style$grid_major_linewidth),
      panel.grid.minor = element_line(colour = style$grid_minor_color, linewidth = style$grid_minor_linewidth),
      #plot.title = element_text(size = 12),
      #plot.subtitle = element_text(size = 10),
      #axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
      #axis.title.y = element_text(size = 10, margin = margin(0, r = 5, 0, l = 0)),
      plot.title = element_text(size = style$title_size, colour = style$title_color, face = style$title_face),
      plot.subtitle = element_text(size = style$subtitle_size, colour = style$subtitle_color, face = style$subtitle_face),
      axis.text.x = element_text(size = style$axis_text_size),
      axis.text.y = element_text(size = style$axis_text_size),
      legend.position = "right", # or "bottom", "top", "left", "none"
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 8),
      strip.text.x = element_text(size = style$strip_text_size),
      strip.text.y = element_text(size = style$strip_text_size)
      #strip.text.y.left = element_text(angle = 0)
    )

  if(markdown_axis_titles){
    fig+theme(
      axis.title.x = element_markdown(size = style$axis_title_size, colour = style$axis_title_color),
      axis.title.y = element_markdown(size = style$axis_title_size, colour = style$axis_title_color),
    )
  }else{
    fig+theme(
      axis.title.x = element_text(size = style$axis_title_size, colour = style$axis_title_color, face = style$axis_title_face),
      axis.title.y = element_text(size = style$axis_title_size, colour = style$axis_title_color, face = style$axis_title_face),
    )
  }

}


.vismi_overimp_combine_title <- function() {
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5,
      color = "#242429"
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0.5,
      color = "#242429"
      #margin = margin(b = 10) # Adds space before the plots start
    )
  )
}



#not used

.ggplot_overimp_theme1 <- function(fig, showlegend = TRUE) {
  if (showlegend == FALSE) {
    fig <- fig + guides(fill = "none", color = "none")
  } else {
    fig <- fig + guides(color = guide_legend(override.aes = list(size = 3)))
  }

  fig +
    theme(
      panel.background = element_rect(fill = "gray95", colour = NA),
      panel.grid.major = element_line(colour = "white", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "white", linewidth = 0.2),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10),
      axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
      axis.title.y = element_text(size = 10, margin = margin(0, r = 5, 0, l = 0)),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "right", # or "bottom", "top", "left", "none"
      legend.title = element_text(size = 10), # face="bold"
      legend.text = element_text(size = 8),
      strip.text.y.left = element_text(angle = 0)
    )
}

.ggplot_overimp_theme2 <- function(fig) {
  fig +
    theme(
      # plot.title = element_blank(),
      # plot.subtitle = element_blank(),
      plot.title = element_text(size = 28),
      plot.subtitle = element_text(size = 26),
      strip.text = element_text(size = 23, face = "plain"),
      axis.title.x = element_text(size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
      axis.title.y = element_text(size = 23, margin = margin(0, r = 5, 0, l = 0)),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      panel.spacing.x = unit(0.4, "cm")
    )
}


# unused ------------------------------------------------------------------


.plotly_overimp_theme <- function(fig, x, y = NULL, z = NULL) {
  if (is.null(y) & is.null(z)) {
    title_text <- paste("Masked true vs multiply-imputed values:", x)
  } else if (is.null(z)) {
    title_text <- paste("Masked true vs multiply-imputed values:", y, "vs", x)
  } else {
    title_text <- paste0(
      "<span style='font-size:18px; font-weight:600;'>",
      "Masked true  vs multiply-imputed values: ", y, " vs ", x,
      "</span><br>", # <- extra <br> adds space between title and subtitle
      "<span style='font-size:13px;'>",
      "Faceted by ", z,
      "</span>"
    )
  }
}
