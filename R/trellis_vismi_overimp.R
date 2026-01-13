#' Trelliscope Visualisation of Overimputation Diagnostics
#' @description Generates a Trelliscope display for overimputation diagnostics across all variables.
#' @param obj An object of class 'overimp' containing imputed datasets and parameters.
#' @param m A single positive integer specifying the number of imputed datasets to plot. It should be smaller than the total number of imputed datasets in the object. Default is NULL (
#' plot all).
#' @param imp_idx A vector of integers specifying the indices of imputed datasets to plot. Default is NULL (plot all).
#' @param integerAsFactor A logical indicating whether integer variables should be treated as factors. Default is FALSE (treated as numeric).
#' @param num_plot A character string specifying the type of plot for numeric variables. Options are "cv" (cross-validation), "ridge", or "density". Default is "cv".
#' @param fac_plot A character string specifying the type of plot for categorical variables. Options are "cv" (cross-validation), "bar", or "dodge". Default is "cv".
#' @param train_color_pal A vector of colors for the training data. If NULL, default colors will be used.
#' @param test_color_pal A vector of colors for the test data. If NULL, default colors will be used.
#' @param stack_y A logical indicating whether to stack y-values in the plots. Default is FALSE.
#' @param diag_color A color specification for the diagonal line in the plots. Default is NULL.
#' @param seed An integer seed for reproducibility. Default is 2025.
#' @param nrow Number of rows in the Trelliscope display. Default is 2.
#' @param ncol Number of columns in the Trelliscope display. Default is 4.
#' @param path Optional path to save the Trelliscope display. If NULL, the display will not be saved to disk.
#' @param ... Additional arguments to customize the plots, such as point_size, xlim, ylim.
#' @return A Trelliscope display object visualising overimputation diagnostics for all variables.
#' @export
trellis_vismi_overimp <- function(obj, m = NULL, imp_idx = NULL, integerAsFactor = FALSE, num_plot = "cv", fac_plot = "cv", train_color_pal = NULL, test_color_pal = NULL, stack_y = FALSE, diag_color = NULL, seed = 2025, nrow = 2, ncol = 4, path = NULL, ...) {
  Variable <- obj$params$Names

  Types <- obj$params$Types
  Types[Types == "integer"] <- if (isTRUE(integerAsFactor)) "factor" else "numeric"

  users_params <- list(...)
  params <- modifyList(.vismi_overimp_params(), users_params)

  plot_map <- list(
    numeric = list(cv = overimp1D_cv_num, ridge = overimp1D_ridge, density = overimp1D_density),
    factor  = list(cv = overimp1D_cv_fac, bar = overimp1D_bar, dodge = overimp1D_dodge)
  )


  # Create a "Control Table" for Trelliscope. This table has one row per variable
  all_vars_df <- tibble(Variable = Variable) |>
    group_by(Variable) |>
    mutate(panel = purrr::map(Variable, function(var) {
      # for each Variable
      var_df <- .overimp_postprocess(
        obj = obj, vars = var, m = m, imp_idx = imp_idx,
        integerAsFactor = integerAsFactor
      )
      var_vec <- obj$imputed_train[[1]][[var]]
      var_type <- Types[[var]]
      plot_which <- if (var_type == "numeric") num_plot else fac_plot
      plot_fun <- plot_map[[var_type]][[plot_which]]

      args_list <- list(
        plot_data = var_df,
        x = var,
        comb_title = paste("Masked true vs impute:", var),
        train_color_pal = if (is.null(train_color_pal)) var_df$train$color_pal else train_color_pal,
        test_color_pal = if (is.null(test_color_pal)) var_df$test$color_pal else test_color_pal,
        point_size = params$point_size,
        xlim = params$xlim,
        ylim = params$ylim,
        stack_y = stack_y,
        diag_color = diag_color,
        seed = seed
      )

      do.call(plot_fun, args_list[names(args_list) %in% names(formals(plot_fun))])
    })) |>
    ungroup()
  if (!is.null(path)) {
    trelliscopejs::trelliscope(all_vars_df, name = "Overimputation diagnostic across all variables", panel_col = "panel", nrow = nrow, ncol = ncol, path = path)
  } else {
    trelliscopejs::trelliscope(all_vars_df, name = "Overimputation diagnostic across all variables", panel_col = "panel", nrow = nrow, ncol = ncol)
  }
}
