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
#' @param verbose A logical value indicating whether to print extra information. Default is FALSE.
#' @param ... Additional arguments to customize the Trelliscope display.
#' @return A Trelliscope display object visualising convergence diagnostics for all variables.
#' @export
#' @examples
#' library(mixgb)
#' set.seed(2026)
#' mixgb_obj <- mixgb(data = nhanes3, m = 3, maxit = 4, pmm.type = "auto", save.models = TRUE)
#' trellis_vismi_converge(obj = mixgb_obj)
trellis_vismi_converge <- function(obj, tick_vals = NULL, color_pal = NULL, title = "auto", subtitle = "auto", nrow = 2, ncol = 4, path = NULL, verbose = FALSE, ...) {
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

  old_opt <- getOption("progress_enabled")

  on.exit(
    options(progress_enabled = old_opt),
    add = TRUE
  )

  if (isFALSE(verbose)) {
    options(progress_enabled = FALSE)
  }


  if (!is.null(path)) {
    trelliscopejs::trelliscope(mis_vars_df, name = "Convergence diagnostic across all incomplete variables", panel_col = "panel", self_contained = FALSE, nrow = nrow, ncol = ncol, path = path)
  } else {
    trelliscopejs::trelliscope(mis_vars_df, name = "Convergence diagnostic across all incomplete variables", panel_col = "panel", self_contained = FALSE, nrow = nrow, ncol = ncol)
  }
}
