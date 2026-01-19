#' Trelliscope Visualisation of Convergence Diagnostics
#' @description Generates a Trelliscope display for convergence diagnostics across all variables.
#' @param obj An object of class 'mixgb' or 'mids' containing intermediate imputed result for each iteration.
#' @param tick_vals A numeric vector specifying the tick values for the x-axis (iterations). If NULL, default tick values will be used.
#' @param color_pal A vector of colors to use for the imputation lines. If NULL, default colors will be used.
#' @param title A string specifying the title of the plot. If NULL, no title is shown. If "auto", a title will be generated based on the input. Default is "auto".
#' @param subtitle A string specifying the subtitle of the plot. If NULL, no subtitle is shown. If "auto", a title will be generated based on the input. Default is "auto".
#' @param nrow Number of rows in the Trelliscope display. Default is 2.
#' @param ncol Number of columns in the Trelliscope display. Default is 4.
#' @param path Optional path to save the Trelliscope display. If NULL, the display will not be saved to disk.
#' @param ... Additional arguments to customize the Trelliscope display.
#' @return A Trelliscope display object visualising convergence diagnostics for all variables.
#' @export
trellis_vismi_converge <- function(obj, tick_vals = NULL, color_pal = NULL, title = "auto", subtitle = "auto", nrow = 2, ncol = 4, path = NULL, ...) {
  if (inherits(obj, "mixgb")) {
    mis_vars <- obj$params$missing.vars
  } else if (inherits(obj, "mids")) {
    mis_vars <- names(obj$nmis)[obj$nmis != 0]
  }

  IncompleteVariable <- sort(mis_vars)

  mis_vars_df <- tibble(IncompleteVariable = IncompleteVariable) |>
    group_by(IncompleteVariable) |>
    mutate(panel = purrr::map(IncompleteVariable, ~ vismi_converge(obj, x = .x, title = title, subtitle = subtitle, tick_vals = tick_vals, color_pal = color_pal))) |>
    ungroup()

  if (!is.null(path)) {
    trelliscopejs::trelliscope(mis_vars_df, name = "Convergence diagnostic across all incomplete variables", panel_col = "panel", self_contained = FALSE, nrow = nrow, ncol = ncol, path = path)
  } else {
    trelliscopejs::trelliscope(mis_vars_df, name = "Convergence diagnostic across all incomplete variables", panel_col = "panel", self_contained = FALSE, nrow = nrow, ncol = ncol)
  }
}
