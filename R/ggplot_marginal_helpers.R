# box+rug -----------------------------------------------------------------

.ggplot_box_rug_x <- function(fig, all_dt, x, y, color_pal) {
  y_min <- min(all_dt[[y]])
  y_box_offset <- y_min - 0.05 * (max(all_dt[[y]]) - y_min)
  y_rug_offset <- y_box_offset - 0.05 * (max(all_dt[[y]]) - y_min)


  fig +
    geom_boxplot(
      data = all_dt,
      aes(
        x = .data[[x]],
        y = y_box_offset, # place at offset
        group = Group, color = Group
      ),
      inherit.aes = FALSE,
      # width =  box_width,
      alpha = 0.5,
      outlier.shape = NA,
      outlier.size = 1,
      outlier.alpha = 0.5,
      show.legend = FALSE
    ) +
    geom_point(aes(y = y_rug_offset, x = .data[[x]], color = Group),
      shape = "|", # The pipe shape looks like a rug tick
      size = 2, # Adjust height of the tick
      alpha = 0.4
    )
}


.ggplot_box_rug_y <- function(fig, all_dt, x, y, color_pal) {
  x_max <- max(all_dt[[x]])
  x_box_offset <- x_max + 0.05 * (x_max - min(all_dt[[x]]))
  x_rug_offset <- x_box_offset + 0.1 * (x_max - min(all_dt[[x]]))
  fig +
    geom_boxplot(
      data = all_dt,
      aes(
        x = x_box_offset,
        y = .data[[y]], # place at offset
        group = Group, color = Group
      ),
      inherit.aes = FALSE,
      # width =  box_width,
      alpha = 0.5,
      outlier.shape = NA,
      outlier.size = 1,
      outlier.alpha = 0.5,
      show.legend = FALSE
    ) +
    geom_point(aes(y = .data[[y]], x = x_rug_offset, color = Group),
      shape = 95, # "—"
      size = 2, # Adjust height of the tick
      alpha = 0.4
    )
}

# box 1d -----------------------------------------------------------------
.ggplot_box_1d <- function(fig, all_dt, x, color_pal, y_box_offset = -0.02, box_width = 0.01) {
  fig +
    geom_boxplot(
      data = all_dt,
      aes(
        x = .data[[x]],
        y = y_box_offset, # place at offset
        group = Group, color = Group
      ),
      inherit.aes = FALSE,
      width = box_width,
      alpha = 0.5,
      outlier.shape = NA,
      outlier.size = 1,
      outlier.alpha = 0.5,
      show.legend = FALSE
    )
}

# rug 1d-----------------------------------------------------------------
.ggplot_rug_1d <- function(fig, all_dt, x, color_pal, y_rug_offset = -0.05) {
  fig +
    geom_point(aes(y = y_rug_offset, x = .data[[x]], color = Group),
      shape = 124, # The pipe shape looks like a rug tick
      size = 3, # Adjust height of the tick
      alpha = 0.8
    )
}


# box 2d -----------------------------------------------------------------

.ggplot_box_x <- function(fig, all_dt, x, y, color_pal) {
  y_min <- min(all_dt[[y]])
  y_box_offset <- y_min - 0.05 * (max(all_dt[[y]]) - y_min)
  y_rug_offset <- y_box_offset - 0.05 * (max(all_dt[[y]]) - y_min)


  fig +
    geom_boxplot(
      data = all_dt,
      aes(
        x = .data[[x]],
        y = y_box_offset, # place at offset
        group = Group, color = Group
      ),
      inherit.aes = FALSE,
      # width =  box_width,
      alpha = 0.5,
      outlier.shape = NA,
      outlier.size = 1,
      outlier.alpha = 0.5,
      show.legend = FALSE
    )
}


.ggplot_box_y <- function(fig, all_dt, x, y, color_pal) {
  x_max <- max(all_dt[[x]])
  x_box_offset <- x_max + 0.05 * (x_max - min(all_dt[[x]]))
  x_rug_offset <- x_box_offset + 0.1 * (x_max - min(all_dt[[x]]))
  fig +
    geom_boxplot(
      data = all_dt,
      aes(
        x = x_box_offset,
        y = .data[[y]], # place at offset
        group = Group, color = Group
      ),
      inherit.aes = FALSE,
      # width =  box_width,
      alpha = 0.5,
      outlier.shape = NA,
      outlier.size = 1,
      outlier.alpha = 0.5,
      show.legend = FALSE
    )
}


# rug 2d-----------------------------------------------------------------


.ggplot_rug_x <- function(fig, all_dt, x, y, color_pal) {
  y_min <- min(all_dt[[y]])
  y_box_offset <- y_min - 0.05 * (max(all_dt[[y]]) - y_min)
  y_rug_offset <- y_box_offset - 0.05 * (max(all_dt[[y]]) - y_min)


  fig +
    geom_point(aes(y = y_rug_offset, x = .data[[x]], color = Group),
      shape = 124, # The pipe shape looks like a rug tick
      size = 2, # Adjust height of the tick
      alpha = 0.4
    )
}


.ggplot_rug_y <- function(fig, all_dt, x, y, color_pal) {
  x_max <- max(all_dt[[x]])
  x_box_offset <- x_max + 0.05 * (x_max - min(all_dt[[x]]))
  x_rug_offset <- x_box_offset + 0.1 * (x_max - min(all_dt[[x]]))

  fig +
    geom_point(aes(y = .data[[y]], x = x_rug_offset, color = Group),
      shape = 95, # "—"
      size = 2, # Adjust height of the tick
      alpha = 0.4
    )
}
