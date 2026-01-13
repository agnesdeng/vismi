#' Visualise onvergence diagnostics
#' @param obj A 'mixgb' object returned by \code{mixgb()} function or a 'mids' object returned by the \code{mice()} function.
#' @param var The name of the variable to plot convergence for.
#' @param title A string specifying the title of the plot. If NULL, no title is shown. If "auto", a title will be generated based on the input. Default is "auto".
#' @param subtitle A string specifying the subtitle of the plot. If NULL, no subtitle is shown. If "auto", a title will be generated based on the input. Default is "auto".
#' @param tick_vals Optional numeric vector specifying x-axis tick values for iterations.
#' @param color_pal A vector of m color codes (e.g., hex codes). If NULL, default colors will be used.
#' @param linewidth The line width for the plot lines. Default is 0.8.
#' @param ... Additional arguments.
#' @return A ggplot2 object showing the convergence plot for the specified variable.
#' @export
vismi_converge <- function(obj, var, title = "auto", subtitle = "auto", tick_vals = NULL, color_pal = NULL, linewidth = 0.8, ...) {
  UseMethod("vismi_converge")
}



#' @export
vismi_converge.mixgb <- function(obj, var, title = "auto", subtitle = "auto", tick_vals = NULL, color_pal = NULL, linewidth = 0.8, ...) {
  if (!inherits(obj, "mixgb")) stop("obj must be a mixgb object returned by mixgb() with `save.models` set to TRUE.")

  .validate_incomplete_variable(obj = obj, var = var)


  mean_mat <- obj$IMP.MEAN[, var, , drop = TRUE]
  sd_mat <- obj$IMP.VAR[, var, , drop = TRUE]
  m <- obj$params$m

  .vismi_converge_universal(
    mean_mat = mean_mat,
    sd_mat = sd_mat,
    m = m,
    var = var,
    method = "mixgb",
    title = title,
    tick_vals = tick_vals,
    color_pal = color_pal,
    linewidth = linewidth
  )
}



#' @export
vismi_converge.mids <- function(obj, var, title = "auto", subtitle = "auto", tick_vals = NULL, color_pal = NULL, linewidth = 0.8, ...) {
  if (!inherits(obj, "mids")) stop("obj must be a mids object returned by mice().")

  .validate_incomplete_variable(obj = obj, var = var)


  mean_mat <- obj$chainMean[var, , , drop = TRUE]
  sd_mat <- obj$chainVar[var, , , drop = TRUE]
  m <- obj$m

  # mice mat requires adding names
  imp_levels <- paste0("Set", seq_len(m))
  colnames(mean_mat) <- imp_levels
  colnames(sd_mat) <- imp_levels

  .vismi_converge_universal(
    mean_mat = mean_mat,
    sd_mat = sd_mat,
    m = m,
    var = var,
    method = "MICE",
    title = title,
    tick_vals = tick_vals,
    color_pal = color_pal,
    linewidth = linewidth
  )
}



.vismi_converge_universal <- function(mean_mat, sd_mat, m, var, method,
                                      title = "auto", subtitle = "auto", tick_vals = NULL,
                                      color_pal = NULL, linewidth = 0.8) {
  if (is.null(color_pal)) {
    color_pal <- .vismi_colors(N_imp = m, observed = FALSE)
  } else {
    if (length(color_pal) < m) {
      stop(paste0("color_pal must have ", m, " colors."))
    }
  }

  # transform data
  transform_data <- function(mat, label) {
    as.data.frame(mat) |>
      mutate(iteration = row_number()) |>
      pivot_longer(cols = starts_with("Set"), names_to = "Imputation", values_to = label) |>
      mutate(Imputation = factor(.data$Imputation, levels = paste0("Set", seq_len(m))))
  }

  mean_df <- transform_data(mean_mat, "Mean")
  sd_df <- transform_data(sd_mat, "SD")


  if (is.null(tick_vals)) {
    maxit <- nrow(mean_mat)
    n_ticks <- 5
    tick_vals <- unique(round(seq(0, maxit, length.out = n_ticks + 1)))
  }


  mean_plot <- ggplot(mean_df, aes(x = .data$iteration, y=.data$Mean, color = .data$Imputation)) +
    geom_line(linewidth = linewidth) +
    scale_x_continuous(breaks = tick_vals) +
    labs(x = "Iteration", y = paste0("Mean of **", var, "**")) +
    scale_color_manual(values = color_pal) +
    guides(color = "none")+
    .vismi_converge_theme()

  sd_plot <- ggplot(sd_df, aes(x=.data$iteration, y=.data$SD, color = .data$Imputation)) +
    geom_line(linewidth = linewidth) +
    scale_x_continuous(breaks = tick_vals) +
    labs(x = "Iteration", y = paste0("SD of **", var, "**")) +
    scale_color_manual(values = color_pal)+
    .vismi_converge_theme()

  if (identical(title, "auto")) {
    title <- paste0("Convergence Diagnostics for missing values imputed by ", method)
  }

  if(identical(subtitle, "auto")){
    subtitle <- paste0("Variable: ", var)
  }

  # Combine using patchwork
  mean_plot + sd_plot +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(title = title,
                    subtitle = subtitle,
                    theme = .vismi_converge_combine_title()
    )

}
