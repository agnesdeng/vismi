#' Trelliscope Visualisation of Distributional Characteristics
#' @description Generates a Trelliscope display for distributional characteristics across all variables.
#' @param data A data frame containing the original data with missing values.
#' @param imp_list A list of imputed data frames.
#' @param m An integer specifying the number of imputed datasets to plot. It should be smaller than \code{length(imp_list)}. Default is NULL (plot all).
#' @param imp_idx A vector of integers specifying the indices of imputed datasets to plot. Default is NULL (plot all).
#' @param integerAsFactor A logical value indicating whether to treat integer variables as factors
#' (TRUE) or numeric (FALSE). Default is FALSE.
#' @param title A string specifying the title of the plot. Default is "auto" (automatic title based on \code{x,y,z} input). If NULL, no title is shown.
#' @param subtitle A string specifying the subtitle of the plot. Default is "auto" (automatic subtitle based on \code{x,y,z} input). If NULL, no subtitle is shown.
#' @param color_pal A named vector of colors for different imputation sets. If NULL
#' (default), a default color palette is used.
#' @param marginal_x A character string specifying the type of marginal plot to add
#' for the x variable in 2D plots. Options are "hist", "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = TRUE. Options are "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = FALSE.
#' @param verbose A logical value indicating whether to print extra information. Default is TRUE.
#' @param nrow Number of rows in the Trelliscope display. Default is 2.
#' @param ncol Number of columns in the Trelliscope display. Default is 4.
#' @param path Optional path to save the Trelliscope display. If NULL, the display will not be saved to disk.
#' @param ... Additional arguments passed to the underlying plotting functions, such as point_size, alpha, nbins, width, and boxpoints.
#' @return A Trelliscope display object visualising distributional characteristics for all variables.
#' @export
trellis_vismi <- function(data, imp_list, m = NULL, imp_idx = NULL, integerAsFactor = FALSE, title = "auto", subtitle = "auto", color_pal = NULL, marginal_x = "box+rug", verbose = FALSE, nrow = 2, ncol = 3, path = NULL, ...) {
  # current option - can provide more later
  num_plot <- "hist"
  fac_plot <- "bar"

  # check data
  data <- .validate_data(data = data, verbose = verbose, integerAsFactor = integerAsFactor, max_levels = 20)
  #data <- out$data
  #Types <- out$Types

  Types <- attr(data, "Types")
  attr(data, "Types") <- NULL

  Variable <- colnames(data)

  # Types <- obj$params$Types
  # Types[Types == "integer"] <- if (isTRUE(integerAsFactor)) "factor" else "numeric"

  users_params <- list(...)
  params <- modifyList(.vismi_static_params(), users_params)


  point_size <- params$point_size
  alpha <- params$alpha
  nbins <- params$nbins
  width <- params$width
  boxpoints <- params$boxpoints


  plot_map <- list(
    numeric = list(hist = ggplot_1num),
    factor  = list(bar = ggplot_1fac)
  )

  # Create a "Control Table" for Trelliscope. This table has one row per variable
  all_vars_df <- tibble(Variable = Variable) |>
    group_by(Variable) |>
    mutate(panel = purrr::map(Variable, function(var) {
      # for each of the item in Variable

      # preprocess data
      pre <- preprocess(data, imp_list, m = m, imp_idx = imp_idx, vars = var, integerAsFactor = integerAsFactor, verbose = verbose)
      all_dt <- pre$all_dt

      if (is.null(color_pal)) {
        color_pal <- pre$color_pal
      }

      no_missing <- pre$no_missing

      no_NA_title <- "Observed values:"
      with_NA_title <- "Observed vs multiply-imputed values:"

      if (no_missing) {
        if (identical(title, "auto")) {
          title <- no_NA_title
        }
        if (identical(subtitle, "auto")) {
          subtitle <- var
        }
      } else {
        if (identical(title, "auto")) {
          title <- with_NA_title
        }
        if (identical(subtitle, "auto")) {
          subtitle <- var
        }
      }

      var_type <- Types[var]
      plot_which <- if (var_type == "numeric") num_plot else fac_plot
      plot_fun <- plot_map[[var_type]][[plot_which]]

      args_list <- list(
        all_dt = all_dt,
        x = var,
        title = title,
        subtitle = subtitle,
        marginal_x = marginal_x,
        color_pal = color_pal,
        point_size = point_size,
        alpha = alpha,
        nbins = nbins,
        width = width,
        boxpoints = boxpoints
      )

      do.call(plot_fun, args_list[names(args_list) %in% names(formals(plot_fun))])
    })) |>
    ungroup()

  if (!is.null(path)) {
    trelliscopejs::trelliscope(all_vars_df, name = "Distributional characteristics for multiply-imputed values across all variables", panel_col = "panel", nrow = nrow, ncol = ncol, path = path)
  } else {
    trelliscopejs::trelliscope(all_vars_df, name = "Distributional characteristics for multiply-imputed values across all variables", panel_col = "panel", nrow = nrow, ncol = ncol)
  }
}
