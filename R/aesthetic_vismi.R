# default global aesthetics

.vismi_colors <- function(N_imp, observed = TRUE) {
  if (N_imp <= 10) {
    # "#8B7355" #8B5A2B "#b0fc38","#234f1e"
    vismi_pal <- c("#333333", "#4E72FF", "#00CD66", "#FFA500", "#fd7fff", "#A08A6A", "#E30022", "#b4f002", "#896deb", "#7FD2FF", "#669460")
    color_pal <- vismi_pal[1:(N_imp + 1)]
  } else {
    color_pal <- c("#666666", hue_pal()(N_imp))
  }

  if (!observed) {
    color_pal <- color_pal[-1]
  }
  color_pal
}


.vismi_gg_style <- function() {
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
    panel_bg_color = NA,
    strip_bg_fill = "gray85",
    strip_bg_color = NA,
    grid_major_color = "white",
    grid_major_linewidth = 0.3,
    grid_minor_color = "white",
    grid_minor_linewidth = 0.2
  )
}


# vismi gg theme -------------------------------------------------------------
.ggplot_base_theme <- function() {
  style <- .vismi_gg_style()

    # guides(fill = "none", color = "none",shape = guide_legend(override.aes = list(size = 3)))
    # guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3)))+
    theme(
      panel.background = element_rect(fill = style$panel_bg_fill, colour = style$panel_bg_color),
      strip.background = element_rect(fill = style$strip_bg_fill, colour = style$strip_bg_color),
      panel.grid.major = element_line(colour = style$grid_major_color, linewidth = style$grid_major_linewidth),
      panel.grid.minor = element_line(colour = style$grid_minor_color, linewidth = style$grid_minor_linewidth),
      plot.title = element_text(size = style$gg_title_size, colour = style$title_color, face = style$gg_title_face),
      plot.subtitle = element_text(size = style$gg_subtitle_size, colour = style$subtitle_color, face = style$gg_subtitle_face),
      axis.title.x = element_text(size = style$gg_axis_title_size, colour = style$axis_title_color, face = style$gg_axis_title_face),
      axis.title.y = element_text(size = style$gg_axis_title_size, colour = style$axis_title_color, face = style$gg_axis_title_face),
      axis.text.x = element_text(size = style$gg_axis_text_size),
      axis.text.y = element_text(size = style$gg_axis_text_size),
    )
}

.ggplot_theme <- function() {

 .ggplot_base_theme()+
    theme(legend.position = "none")
}


.ggplot_theme_3fac <- function() {

  .ggplot_base_theme()+
    theme(
      axis.text.x = element_markdown(),
      axis.text.y = element_markdown(),
      legend.position = "bottom"
    )
}


# vismi plotly theme -------------------------------------------------------------
.vismi_plotly_style <- function() {
  list(
    title_color = "#242429",
    title_font = "Helvetica, Arial, sans-serif",
    title_size = 20,
    subtitle_color = "#242429",
    axis_title_color = "#35353d",
    axis_title_font = "Arial Black",
    axis_title_size = 14,
    plot_bgcolor = "#f2f7fc",
    paper_bgcolor = "#fff",
    gridcolor = "#999"
  )
}


.plotly_layout_common <- function(fig, title, subtitle, x) {
  style <- .vismi_plotly_style()

  title_parts <- list(
    title = if (!is.null(title)) paste0("<b>", title, "</b>"),
    subtitle = if (!is.null(subtitle)) {
      paste0(
        "<span style='font-family: Helvetica, Arial, sans-serif; font-size:18px;color:#242429;'>",
        subtitle, "</span>"
      )
    }
  )


  title_text <- paste(Filter(Negate(is.null), title_parts), collapse = "<br>")


  x_axis_title <- list(
    text = x,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "top",
    x = 0.5,
    y = -0.05,
    showarrow = FALSE,
    font = list(size = style$axis_title_size, family = style$axis_title_font, color = style$axis_title_color)
  )


  fig <- fig |> layout(
    annotations = c(list(x_axis_title))
  )

  fig <- fig |> layout(
    plot_bgcolor = style$plot_bgcolor,
    title = list(
      text = title_text,
      x = 0, # horizontal position (0 = left, 0.5 = center, 1 = right)
      y = 0.96, # vertical position (1 = top, 1 = bottom)
      xanchor = "left", # anchor for x position ("left", "center", "right")
      yanchor = "top", # anchor for y position ("top", "middle", "bottom")
      xref = "paper",
      font = list(
        size = style$title_size,
        color = style$title_color,
        family = style$title_font
      )
    ),
    margin = list(l = 70, r = 70, t = 70, b = 70), # increase right and top margin to avoid cutoffs
    legend = list(
      yref = "container",
      xref = "paper",
      x = 0.5, # x position (0 = left, 1 = right)
      y = -0.15, # y position (0 = bottom, 1 = top)
      xanchor = "center", # "left", "center", "right"
      yanchor = "top", # "top", "middle", "bottom"
      orientation = "h", # "v" = vertical, "h" = horizontal
      itemsizing = "constant"
    ),
    yaxis = list(
      title = list(
        font = list(
          family = style$axis_title_font,
          size = style$axis_title_size,
          color = style$axis_title_color
        )
      )
    )
  )

  fig
}


.plotly_layout_2fac <- function(fig, title, subtitle, y_levels, y, x) {
  style <- .vismi_plotly_style()

  title_parts <- list(
    title = if (!is.null(title)) paste0("<b>", title, "</b>"),
    subtitle = if (!is.null(subtitle)) {
      paste0(
        "<span style='font-family: Helvetica, Arial, sans-serif; font-size:18px;color:#242429;'>",
        subtitle, "</span>"
      )
    }
  )


  title_text <- paste(Filter(Negate(is.null), title_parts), collapse = "<br>")

  k <- length(y_levels)

  annotations <- lapply(seq_along(y_levels), function(i) {
    list(
      xref = "paper",
      yref = "paper",
      x = 1,
      y = 1 - (i - 0.5) / k,
      text = y_levels[i],
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      textangle = -90,
      font = list(size = 12)
    )
  })

  prop_axis_title <- list(
    text = "Proportion",
    xref = "paper",
    yref = "paper",
    xanchor = "right",
    yanchor = "middle",
    x = 0,
    y = 0.5,
    xshift = -30,
    showarrow = FALSE,
    textangle = -90,
    font = list(size = style$axis_title_size, family = style$axis_title_font, color = style$axis_title_color)
  )

  y_axis_title <- list(
    text = y,
    xref = "paper",
    yref = "paper",
    xanchor = "left",
    yanchor = "middle",
    x = 1,
    y = 0.5,
    xshift = 30,
    showarrow = FALSE,
    textangle = -90,
    font = list(size = style$axis_title_size, family = style$axis_title_font, color = style$axis_title_color)
  )

  x_axis_title <- list(
    text = x,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "top",
    x = 0.5,
    y = -0.05,
    showarrow = FALSE,
    font = list(size = style$axis_title_size, family = style$axis_title_font, color = style$axis_title_color)
  )


  fig <- fig |> layout(annotations = c(annotations, list(prop_axis_title, y_axis_title, x_axis_title)))

  fig <- fig |> layout(
    plot_bgcolor = style$plot_bgcolor,
    title = list(
      text = title_text,
      x = 0, # horizontal position (0 = left, 0.5 = center, 1 = right)
      y = 0.96, # vertical position (1 = top, 1 = bottom)
      xanchor = "left", # anchor for x position ("left", "center", "right")
      yanchor = "top", # anchor for y position ("top", "middle", "bottom")
      xref = "paper",
      font = list(
        size = style$title_size,
        color = style$title_color,
        family = style$title_font
      )
    ),
    margin = list(l = 70, r = 70, t = 70, b = 70), # increase right and top margin to avoid cutoffs
    legend = list(
      yref = "container",
      xref = "paper",
      x = 0.5, # x position (0 = left, 1 = right)
      y = -0.15, # y position (0 = bottom, 1 = top)
      xanchor = "center", # "left", "center", "right"
      yanchor = "top", # "top", "middle", "bottom"
      orientation = "h", # "v" = vertical, "h" = horizontal
      itemsizing = "constant"
    )
  )

  fig
}


.plotly_layout_facet3d <- function(fig, title, subtitle, z_levels, y, z, x) {
  style <- .vismi_plotly_style()

  title_parts <- list(
    title = if (!is.null(title)) paste0("<b>", title, "</b>"),
    subtitle = if (!is.null(subtitle)) {
      paste0(
        "<span style='font-family: Helvetica, Arial, sans-serif; font-size:18px;color:#242429;'>",
        subtitle, "</span>"
      )
    }
  )


  title_text <- paste(Filter(Negate(is.null), title_parts), collapse = "<br>")


  k <- length(z_levels)

  annotations <- lapply(seq_along(z_levels), function(i) {
    list(
      xref = "paper",
      yref = "paper",
      x = 1,
      y = 1 - (i - 0.5) / k,
      text = z_levels[i],
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      textangle = -90,
      font = list(size = 12)
    )
  })

  y_axis_title <- list(
    text = y,
    xref = "paper",
    yref = "paper",
    xanchor = "right",
    yanchor = "middle",
    x = 0,
    y = 0.5,
    xshift = -30,
    showarrow = FALSE,
    textangle = -90,
    font = list(size = style$axis_title_size, family = style$axis_title_font, color = style$axis_title_color)
  )

  z_axis_title <- list(
    text = z,
    xref = "paper",
    yref = "paper",
    xanchor = "left",
    yanchor = "middle",
    x = 1,
    y = 0.5,
    xshift = 30,
    showarrow = FALSE,
    textangle = -90,
    font = list(size = style$axis_title_size, family = style$axis_title_font, color = style$axis_title_color)
  )

  x_axis_title <- list(
    text = x,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "top",
    x = 0.5,
    y = -0.05,
    showarrow = FALSE,
    font = list(size = style$axis_title_size, family = style$axis_title_font, color = style$axis_title_color)
  )

  fig <- fig |> layout(annotations = c(annotations, list(y_axis_title, z_axis_title, x_axis_title)))


  fig <- fig |> layout(
    plot_bgcolor = style$plot_bgcolor,
    title = list(
      text = title_text,
      x = 0, # horizontal position (0 = left, 0.5 = center, 1 = right)
      y = 0.96, # vertical position (1 = top, 1 = bottom)
      xanchor = "left", # anchor for x position ("left", "center", "right")
      yanchor = "top", # anchor for y position ("top", "middle", "bottom")
      xref = "paper",
      font = list(
        size = style$title_size,
        color = style$title_color,
        family = style$title_font
      )
    ),
    margin = list(l = 70, r = 70, t = 70, b = 70), # increase right and top margin to avoid cutoffs
    legend = list(
      yref = "container",
      xref = "paper",
      x = 0.5, # x position (0 = left, 1 = right)
      y = -0.15, # y position (0 = bottom, 1 = top)
      xanchor = "center", # "left", "center", "right"
      yanchor = "top", # "top", "middle", "bottom"
      orientation = "h", # "v" = vertical, "h" = horizontal
      itemsizing = "constant"
    )
  )

  fig
}


.plotly_layout_scatter3d <- function(fig, title, subtitle) {
  style <- .vismi_plotly_style()

  title_parts <- list(
    title = if (!is.null(title)) paste0("<b>", title, "</b>"),
    subtitle = if (!is.null(subtitle)) {
      paste0(
        "<span style='font-family: Helvetica, Arial, sans-serif; font-size:18px;color:#242429;'>",
        subtitle, "</span>"
      )
    }
  )


  title_text <- paste(Filter(Negate(is.null), title_parts), collapse = "<br>")


  fig <- fig |> layout(
    scene = list(
      # bgcolor = "#f2f7fc",  # canvas rectangle box area color
      # change the background of the 3D box
      xaxis = list(title = list(font = list(
        size = style$axis_title_size,
        family = style$axis_title_font, color = style$axis_title_color
      )), showbackground = TRUE, backgroundcolor = style$plot_bgcolor, gridcolor = style$gridcolor),
      yaxis = list(title = list(font = list(
        size = style$axis_title_size,
        family = style$axis_title_font, color = style$axis_title_color
      )), showbackground = TRUE, backgroundcolor = style$plot_bgcolor, gridcolor = style$gridcolor),
      zaxis = list(title = list(font = list(
        size = style$axis_title_size,
        family = style$axis_title_font, color = style$axis_title_color
      )), showbackground = TRUE, backgroundcolor = style$plot_bgcolor, gridcolor = style$gridcolor)
    ),
    paper_bgcolor = style$paper_bgcolor, # the whole paper
    # plot_bgcolor="#f2f7fc", # has no effect here
    title = list(
      text = title_text,
      x = 0, # horizontal position (0 = left, 0.5 = center, 1 = right)
      y = 0.96, # vertical position (1 = top, 1 = bottom)
      xanchor = "left", # anchor for x position ("left", "center", "right")
      yanchor = "top", # anchor for y position ("top", "middle", "bottom")
      xref = "paper",
      font = list(
        size = style$title_size,
        color = style$title_color,
        family = style$title_font
      )
    ),
    margin = list(r = 50, t = 50), # increase right and top margin to avoid cutoffs
    legend = list(
      x = 0.5, # x position (0 = left, 1 = right)
      y = -0.2, # y position (0 = bottom, 1 = top)
      xanchor = "center", # "left", "center", "right"
      yanchor = "top", # "top", "middle", "bottom"
      orientation = "h", # "v" = vertical, "h" = horizontal
      itemsizing = "constant"
    )
  )

  fig
}
