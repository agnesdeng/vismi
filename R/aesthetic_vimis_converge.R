
.vismi_converge_style <- function() {
  list(
    title_color = "#242429",
    gg_title_size = 14,
    gg_title_face = "bold",
    subtitle_color = "#242429",
    gg_subtitle_size = 14,
    gg_subtitle_face = "plain",
    axis_title_color = "#35353d",
    gg_axis_title_size = 10,
    gg_axis_title_face = "bold",
    gg_axis_text_size = 9,
    panel_bg_fill = "gray95",
    #panel_bg_fill = NA,
    panel_bg_color = NA,
    strip_bg_fill = "gray85",
    strip_bg_color = NA,
    grid_major_color = "gray92",
    grid_major_linewidth = 0.5,
    grid_minor_color = "gray92",
    grid_minor_linewidth = 0.2
  )
}


# vismi converge theme -------------------------------------------------------------
.vismi_converge_theme <- function() {
  style <- .vismi_converge_style()

  theme(
    axis.title.x = element_text(size = style$gg_axis_title_size, colour = style$axis_title_color, face = style$gg_axis_title_face),
    axis.title.y = element_markdown(size = style$gg_axis_title_size),
    axis.text.x = element_text(size = style$gg_axis_text_size),
    axis.text.y = element_text(size = style$gg_axis_text_size),
    panel.background = element_rect(fill = style$panel_bg_fill, colour = style$panel_bg_color),
    panel.grid.major = element_line(colour = style$grid_major_color, linewidth = style$grid_major_linewidth),
    panel.grid.minor = element_line(colour = style$grid_minor_color, linewidth = style$grid_minor_linewidth),
  )


}


.vismi_converge_combine_title <- function() {
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
