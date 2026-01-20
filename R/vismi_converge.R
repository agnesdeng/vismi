#' Visualise convergence diagnostics
#' @param obj A 'mixgb' object returned by \code{mixgb()} function or a 'mids' object returned by the \code{mice()} function.
#' @param x The name of the variable to plot convergence for.
#' @param xlim Optional numeric vector of length 2 specifying the x-axis limits for iterations.
#' @param mean_lim Optional numeric vector of length 2 specifying the y-axis limits for mean values of the variable.
#' @param sd_lim Optional numeric vector of length 2 specifying the y-axis limits for standard deviation values of the variable.
#' @param title A string specifying the title of the plot. If NULL, no title is shown. If "auto", a title will be generated based on the input. Default is "auto".
#' @param subtitle A string specifying the subtitle of the plot. If NULL, no subtitle is shown. If "auto", a title will be generated based on the input. Default is "auto".
#' @param tick_vals Optional numeric vector specifying x-axis tick values for iterations.
#' @param color_pal A vector of m color codes (e.g., hex codes). If NULL, default colors will be used.
#' @param linewidth The line width for the plot lines. Default is 0.8.
#' @param ... Additional arguments.
#' @return A ggplot2 object showing the convergence plot for the specified variable.
#' @export
#' @examples
#' \dontrun{
#' library(mixgb)
#' set.seed(2026)
#' mixgb_obj <- mixgb(data = newborn, m = 5, maxit = 5, pmm.type = "auto", save.models = TRUE)
#' vismi_converge(obj = mixgb_obj, x = "recumbent_length_cm")
#' }
vismi_converge <- function(obj, x, xlim = NULL, mean_lim = NULL, sd_lim = NULL,title = "auto", subtitle = "auto", tick_vals = NULL, color_pal = NULL, linewidth = 0.8, ...) {
  UseMethod("vismi_converge")
}





#' @export
vismi_converge.mixgb <- function(obj, x, xlim = NULL, mean_lim = NULL, sd_lim = NULL, title = "auto", subtitle = "auto", tick_vals = NULL, color_pal = NULL, linewidth = 0.8, ...) {
  if (!inherits(obj, "mixgb")) stop("obj must be a mixgb object returned by mixgb() with `save.models` set to TRUE.")

  .validate_incomplete_variable(obj = obj, x = x)


  mean_mat <- obj$IMP.MEAN[, x, , drop = TRUE]
  sd_mat <- sqrt(obj$IMP.VAR[, x, , drop = TRUE])
  m <- obj$params$m


  .vismi_converge_universal(
    mean_mat = mean_mat,
    sd_mat = sd_mat,
    m = m,
    x = x,
    method = "mixgb",
    title = title,
    tick_vals = tick_vals,
    color_pal = color_pal,
    linewidth = linewidth,
    xlim = xlim,
    mean_lim = mean_lim,
    sd_lim = sd_lim
  )
}



#' @export
vismi_converge.mids <- function(obj, x, xlim = NULL, mean_lim = NULL, sd_lim = NULL, title = "auto", subtitle = "auto", tick_vals = NULL, color_pal = NULL, linewidth = 0.8, ...) {
  if (!inherits(obj, "mids")) stop("obj must be a mids object returned by mice().")

  .validate_incomplete_variable(obj = obj, x = x)


  mean_mat <- obj$chainMean[x, , , drop = TRUE]
  sd_mat <- sqrt(obj$chainVar[x, , , drop = TRUE])
  m <- obj$m

  # mice mat requires adding names
  #imp_levels <- paste("Set", seq_len(m))
  #colnames(mean_mat) <- imp_levels
  #colnames(sd_mat) <- imp_levels

  .vismi_converge_universal(
    mean_mat = mean_mat,
    sd_mat = sd_mat,
    m = m,
    x = x,
    method = "mice",
    title = title,
    tick_vals = tick_vals,
    color_pal = color_pal,
    linewidth = linewidth,
    xlim = xlim,
    mean_lim = mean_lim,
    sd_lim = sd_lim
  )
}



.vismi_converge_universal <- function(mean_mat, sd_mat, m, x, method,
                                      title = "auto", subtitle = "auto", tick_vals = NULL,
                                      color_pal = NULL, linewidth = 0.8, xlim, mean_lim = NULL, sd_lim = NULL) {

  #ensure consistency across different methods
  imp_levels <- paste("Set", seq_len(m))
  colnames(mean_mat) <- imp_levels
  colnames(sd_mat) <- imp_levels


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
      mutate(Imputation = factor(.data$Imputation, levels = paste("Set", seq_len(m))))
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
    labs(x = "Iteration", y = paste0("Mean of **", x, "**")) +
    scale_color_manual(values = color_pal) +
    guides(color = "none")+
    .vismi_converge_theme()


  sd_plot <- ggplot(sd_df, aes(x=.data$iteration, y=.data$SD, color = .data$Imputation)) +
    geom_line(linewidth = linewidth) +
    scale_x_continuous(breaks = tick_vals) +
    labs(x = "Iteration", y = paste0("SD of **", x, "**")) +
    scale_color_manual(values = color_pal)+
    .vismi_converge_theme()

  if(!is.null(xlim)){
    mean_plot <- mean_plot + xlim(xlim)
    sd_plot <- sd_plot + xlim(xlim)
  }
  if(!is.null(mean_lim)){
    mean_plot <- mean_plot + ylim(mean_lim)
  }

  if(!is.null(sd_lim)){
    sd_plot <- sd_plot + ylim(sd_lim)
  }

  if (identical(title, "auto")) {
    title <- paste0("Convergence Diagnostics for missing values imputed by ", method)
  }

  if(identical(subtitle, "auto")){
    subtitle <- paste0("Variable: ", x)
  }

  # Combine using patchwork
  mean_plot + sd_plot +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(title = title,
                    subtitle = subtitle,
                    theme = .vismi_converge_combine_title()
    )

}
