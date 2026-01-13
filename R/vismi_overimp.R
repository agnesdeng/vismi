#' Visualise Multiple Imputation Through Overimputations
#' @description This function provides visual diagnostic tools for assessing multiply imputed datasets imputed with 'mixgb' or other imputers through overimputations. It supports visualisation for both training and test set.
#' @param obj Overimputation object of class 'overimp' created by the \code{overimp()} function.
#' @param x A character string specifying the name of the variable to plot on the x
#' @param y A character string specifying the name of the variable to plot on the y
#' @param z A character string specifying the name of the variable to plot on the z
#' @param m A single positive integer specifying the number of imputed datasets to plot. It should be smaller than the total number of imputed datasets in the object.
#' @param imp_idx A vector of integers specifying the indices of imputed datasets to plot.
#' @param integerAsFactor A logical indicating whether integer variables should be treated as factors. Default is FALSE (treated as numeric).
#' @param title A string specifying the title of the plot. Default is "auto" (automatic title based on \code{x,y,z} input). If NULL, no title is shown.
#' @param subtitle A string specifying the subtitle of the plot. Default is "auto" (automatic subtitle based on \code{x,y,z} input). If NULL, no subtitle is shown.
#' @param num_plot A character string specifying the type of plot for numeric variables.
#' @param fac_plot A character string specifying the type of plot for categorical variables.
#' @param train_color_pal A vector of colors for the training data. If NULL, default colors will be used.
#' @param test_color_pal A vector of colors for the test data. If NULL, default colors will be used.
#' @param stack_y A logical indicating whether to stack y values in certain plots. Default is FALSE.
#' @param diag_color A character string specifying the color of the diagonal line in scatter plots. Default is NULL.
#' @param seed An integer specifying the random seed for reproducibility. Default is 2025.
#' @param ... Additional arguments to customize the plots, such as position, point_size, linewidth, alpha, xlim, ylim, boxpoints, width.
#' @export
vismi_overimp <- function(obj, x = NULL, y = NULL, z = NULL, m = NULL, imp_idx = NULL, integerAsFactor = FALSE, title = "auto", subtitle = "auto", num_plot = "cv", fac_plot = "cv", train_color_pal = NULL, test_color_pal = NULL, stack_y = FALSE, diag_color = NULL, seed = 2025, ...) {
  # checking
  if (!inherits(obj, "overimp")) stop("Object must be of class 'overimp'")

  if (!is.null(x) && !is.character(x)) {
    stop("x must be a variable name in character format.")
  }
  if (!is.null(y) && !is.character(y)) {
    stop("y must be a variable name in character format.")
  }
  if (!is.null(z) && !is.character(z)) {
    stop("z must be a variable name in character format.")
  }


  vars <- c(x, y, z)
  # remove any NULL
  vars <- vars[!sapply(vars, is.null)]

  p <- obj$params$p
  extra_vars <- names(p)
  if (!is.null(extra_vars)) {
    vars_with_p <- paste0(extra_vars, "=", p, collapse = ", ")
    cli::cli_inform("Extra missing values were added to the following variables before overimputation: {vars_with_p}")
    cli::rule()

    if (!vars %in% extra_vars) {
      cli::cli_abort(c(
        "No extra missing values were added to the specified {cli::qty(vars)} variable{?s} ({.var {cli::cli_vec(vars)}}).",
        "i" = "Please choose from: {cli::cli_vec(extra_vars)}."
      ))
    }
  }


  # Names <- obj$params$Names
  nonexist_vars <- setdiff(vars, obj$params$Names)
  if (length(nonexist_vars > 0)) {
    stop(
      "Please check your spelling. Variable(s) not found in data: ",
      paste(nonexist_vars, collapse = ", ")
    )
  }


  # users_params<-list()
  users_params <- list(...)

  params <- modifyList(.vismi_overimp_params(), users_params)

  position <- params$position
  point_size <- params$point_size
  linewidth <- params$linewidth
  alpha <- params$alpha
  xlim <- params$xlim
  ylim <- params$ylim
  boxpoints <- params$boxpoints
  width <- params$width


  Types <- obj$params$Types
  types <- Types[vars]
  types[types == "integer"] <- if (isTRUE(integerAsFactor)) "factor" else "numeric"


  # number of variables
  D <- length(vars)
  default_title <- "Masked true vs multiply-imputed values:"

  if (D == 1) {
    x <- vars[1]
    if (identical(title, "auto")) {
      title <- default_title
    }
    if (identical(subtitle, "auto")) {
      subtitle <- x
    }
    #comb_title <- paste("Masked true vs multiply-imputed values:", x)
    # comb_title <-grid::textGrob(paste("Masked true vs multiply-imputed values:", x),
    # gp = grid::gpar(fontsize = 14, fontface = "bold"))


    plot_map <- list(
      numeric = list(
        cv = overimp1D_cv_num,
        qq = overimp1D_qq,
        qqline = overimp1D_qqline,
        ridge = overimp1D_ridge,
        density = overimp1D_density
      ),
      factor = list(
        cv = overimp1D_cv_fac,
        bar = overimp1D_bar,
        dodge = overimp1D_dodge
      )
    )

    plot_which <- switch(types,
      numeric = num_plot,
      factor = fac_plot,
    )

    plot_fun <- plot_map[[types]][[plot_which]]

    if (is.null(plot_fun)) {
      stop("The specified plot type is not valid for the variable type.")
    }
  } else if (D == 2) {
    x <- vars[1]
    y <- vars[2]

    if (identical(title, "auto")) {
      title <- default_title
    }
    if (identical(subtitle, "auto")) {
      subtitle <- paste(y, "vs", x)
    }
    #comb_title <- paste("Masked true vs multiply-imputed values:", y, "vs", x)

    # comb_title <-grid::textGrob(paste("Masked true vs multiply-imputed values:", y, "vs", x),
    # gp = grid::gpar(fontsize = 14, fontface = "bold"))

    type_comb <- paste0(sort(types), collapse = "_")

    plot_fun <- switch(type_comb,
      "numeric_numeric" = overimp2D_scatter,
      "factor_numeric" = overimp2D_box,
      "factor_factor" = overimp2D_bar
    )
  } else if (D == 3) {
    x <- vars[1]
    y <- vars[2]
    z <- vars[3]

   # comb_title <- grid::textGrob(
    #  paste0(
      #  "Masked true vs multiply-imputed values: ", y, " vs ", x,
      #  "\nFaceted by ", z
     # ),
     # gp = grid::gpar(fontsize = 14, fontface = "bold")
    #)
    if (identical(title, "auto")) {
      title <- default_title
    }
    if (identical(subtitle, "auto")) {
      subtitle <- paste(y, "vs", x, "faceted by", z)
    }

    stop("Overimputation plots with 3 variables are not implemented yet.")
  }


  plot_data <- .overimp_postprocess(obj = obj, vars = vars, m = m, imp_idx = imp_idx, integerAsFactor = integerAsFactor)

  if (is.null(train_color_pal)) {
    train_color_pal <- plot_data$train$color_pal
  }

  if (is.null(test_color_pal)) {
    test_color_pal <- plot_data$test$color_pal
  }


  call_plot_fun <- function(plot_fun, args_list) {
    fun_args <- names(formals(plot_fun))
    args_list <- args_list[names(args_list) %in% fun_args]
    do.call(plot_fun, args_list)
  }

  # all possible arguments
  args_list <- list(
    plot_data = plot_data,
    x = vars[1],
    y = vars[2],
    z = vars[3],
    train_color_pal = train_color_pal,
    test_color_pal = test_color_pal,
    #comb_title = comb_title,
    title = title,
    subtitle = subtitle,
    alpha = alpha,
    xlim = xlim,
    ylim = ylim,
    position = position,
    point_size = point_size,
    linewidth = linewidth,
    boxpoints = boxpoints,
    width = width,
    stack_y = stack_y,
    diag_color = diag_color,
    seed = seed
  )

  # Call the plotting function
  call_plot_fun(plot_fun, args_list)
}
