#' Visualise Multiple Imputations Through Distributional Characteristics
#' @description This function provides visual diagnostic tools for assessing multiply imputed datasets created with 'mixgb' or other imputers through inspecting the distributional characteristics of imputed variables. It supports 1D, 2D, and 3D visualisations for numeric and categorical variables using either interactive or static plots.
#' @param data A data frame containing the original data with missing values.
#' @param imp_list A list of imputed data frames.
#' @param x A character string specifying the name of the variable to plot on the x
#' axis. Default is NULL.
#' @param y A character string specifying the name of the variable to plot on the y
#' axis. Default is NULL.
#' @param z A character string specifying the name of the variable to plot on the z
#' axis. Default is NULL.
#' @param m An integer specifying the number of imputed datasets to plot. It should be smaller than \code{length(imp_list)}. Default is NULL (plot all).
#' @param imp_idx A vector of integers specifying the indices of imputed datasets to plot. Default is NULL (plot all).
#' @param interactive A logical value indicating whether to create an interactive plotly plot (TRUE
#' by default) or a static ggplot2 plot (FALSE).
#' @param title A string specifying the title of the plot. Default is "auto" (automatic title based on \code{x,y,z} input). If NULL, no title is shown.
#' @param subtitle A string specifying the subtitle of the plot. Default is "auto" (automatic subtitle based on \code{x,y,z} input). If NULL, no subtitle is shown.
#' @param integerAsFactor A logical value indicating whether to treat integer variables as factors
#' (TRUE) or numeric (FALSE). Default is FALSE.
#' @param color_pal A named vector of colors for different imputation sets. If NULL
#' (default), a default color palette is used.
#' @param marginal_x A character string specifying the type of marginal plot to add
#' for the x variable in 2D plots. Options are "hist", "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = TRUE. Options are "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = FALSE.
#' @param marginal_y A character string specifying the type of marginal plot to add
#' for the y variable in 2D plots. Options are "hist", "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = TRUE. Options are "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = FALSE.
#' @param verbose A logical value indicating whether to print extra information. Default is FALSE.
#' @param ... Additional arguments passed to the underlying plotting functions, such as point_size, alpha, nbins, width, and boxpoints.
#' @return A plotly or ggplot2 object visualizing the imputed data.
#' @export
vismi <- function(data, imp_list, x = NULL, y = NULL, z = NULL, m = NULL, imp_idx = NULL, interactive = FALSE, integerAsFactor = FALSE, title = "auto", subtitle = "auto", color_pal = NULL, marginal_x = NULL, marginal_y = NULL, verbose = FALSE, ...) {
  # check data
  out <- .validate_data(data = data, verbose = verbose, integerAsFactor = integerAsFactor, max_levels = 20)
  data <- out$data
  Types <- out$Types

  # check input variables
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

  # type of variables
  types <- Types[vars]


  nonexist_vars <- setdiff(vars, names(data))
  if (length(nonexist_vars > 0)) {
    stop(
      "Please check your spelling. Variable(s) not found in data: ",
      paste(nonexist_vars, collapse = ", ")
    )
  }


  # users_params<-list()

  users_params <- list(...)

  if (interactive) {
    params <- modifyList(.vismi_interactive_params(), users_params)
  } else {
    params <- modifyList(.vismi_static_params(), users_params)
  }

  point_size <- params$point_size
  alpha <- params$alpha
  nbins <- params$nbins
  width <- params$width
  boxpoints <- params$boxpoints

  # preprocess data
  pre <- preprocess(data, imp_list, m = m, imp_idx = imp_idx, vars = vars, integerAsFactor = integerAsFactor, verbose = verbose)
  all_dt <- pre$all_dt
  if (is.null(color_pal)) {
    color_pal <- pre$color_pal
  }
  no_missing <- pre$no_missing

  # number of variables
  D <- length(vars)

  no_NA_title <- "Observed values:"
  with_NA_title <- "Observed vs multiply-imputed values:"


  if (D == 1) {
    if (no_missing) {
      if (identical(title, "auto")) {
        title <- no_NA_title
      }
      if (identical(subtitle, "auto")) {
        subtitle <- x
      }
    } else {
      if (identical(title, "auto")) {
        title <- with_NA_title
      }
      if (identical(subtitle, "auto")) {
        subtitle <- x
      }
    }

    if (!is.null(marginal_y)) {
      warning("marginal_y is ignored for 1D diganostics plot.")
    }

    plot_fun <- switch(types,
      "numeric" = if (interactive) plotly_1num else ggplot_1num,
      "factor" = if (interactive) plotly_1fac else ggplot_1fac
    )
  }


  if (D == 2) {
    if (no_missing) {
      if (identical(title, "auto")) {
        title <- no_NA_title
      }
      if (identical(subtitle, "auto")) {
        subtitle <- paste(y, "vs", x)
      }
    } else {
      if (identical(title, "auto")) {
        title <- with_NA_title
      }
      if (identical(subtitle, "auto")) {
        subtitle <- paste(y, "vs", x)
      }
    }

    type_comb <- paste0(sort(types), collapse = "_")

    plot_fun <- switch(type_comb,
      "numeric_numeric"   = if (interactive) plotly_2num else ggplot_2num,
      "factor_numeric"    = if (interactive) plotly_1fac1num else ggplot_1fac1num,
      "factor_factor"     = if (interactive) plotly_2fac else ggplot_2fac
    )
  }


  if (D == 3) {
    type_comb <- paste0(sort(types), collapse = "_")

    if (type_comb == "factor_numeric_numeric") {
      # z: factor
      fac_idx <- which(types == "factor")
      if (fac_idx != 3) {
        fac <- vars[fac_idx]
        vars[fac_idx] <- vars[3]
        vars[3] <- fac
      }
    } else if (type_comb == "factor_factor_numeric") {
      # z: factor
      num_idx <- which(types == "numeric")
      if (num_idx == 3) {
        num <- vars[num_idx]
        vars[3] <- vars[2]
        vars[2] <- num
      }
    }

    x <- vars[1]
    y <- vars[2]
    z <- vars[3]


    # plot_title
    if (no_missing) {
      if (identical(title, "auto")) {
        title <- no_NA_title
      }
      if (identical(subtitle, "auto")) {
        subtitle <- paste(y, "vs", x, "faceted by", z)
      }
    } else {
      if (type_comb == "numeric_numeric_numeric" | type_comb == "factor_factor_factor") {
        if (identical(title, "auto")) {
          title <- with_NA_title
        }
        if (identical(subtitle, "auto")) {
          subtitle <- paste(z, "vs", y, "vs", x)
        }
      } else {
        if (identical(title, "auto")) {
          title <- with_NA_title
        }
        if (identical(subtitle, "auto")) {
          subtitle <- paste(y, "vs", x, "faceted by", z)
        }
      }
    }


    plot_fun <- switch(type_comb,
      "numeric_numeric_numeric"   = if (interactive) plotly_3num else ggplot_3num,
      "factor_numeric_numeric"    = if (interactive) plotly_1fac2num else ggplot_1fac2num,
      "factor_factor_numeric"     = if (interactive) plotly_2fac1num else ggplot_2fac1num,
      "factor_factor_factor"      = if (interactive) plotly_3fac else ggplot_3fac
    )
  }


  call_plot_fun <- function(plot_fun, args_list) {
    fun_args <- names(formals(plot_fun))
    args_list <- args_list[names(args_list) %in% fun_args]
    do.call(plot_fun, args_list)
  }

  # all possible arguments
  args_list <- list(
    all_dt = all_dt,
    imp_list = imp_list,
    x = x,
    y = y,
    z = z,
    title = title,
    subtitle = subtitle,
    # plot_title = plot_title,
    # integerAsFactor = integerAsFactor,
    marginal_x = marginal_x,
    marginal_y = marginal_y,
    color_pal = color_pal,
    point_size = point_size,
    alpha = alpha,
    nbins = nbins,
    width = width,
    boxpoints = boxpoints
  )

  # Call the plotting function
  if (interactive) {
    call_plot_fun(plot_fun, args_list)
  } else {
    fig <- call_plot_fun(plot_fun, args_list)
    class(fig) <- c("vismi", class(fig))
    fig
  }
}


#' print method for vismi objects
#' @description vismi Print method for vismi objects
#' @param x An object of class 'vismi' created by the \code{vismi.data.frame()} function.
#' @param ... Additional arguments (not used).
#' @exportS3Method
print.vismi <- function(x, ...) {
  # Check if it's a gtable/grob (from arrangeGrob)
  if (inherits(x, "gtable") || inherits(x, "grob")) {
    grid::grid.newpage() # Clear the plot area
    grid::grid.draw(x) # Draw the plot
  } else if (inherits(x, "ggplot")) {
    NextMethod("print")
  } else if (inherits(x, "ggmatrix") || inherits(x, "GGally:ggmatrix")) {
    NextMethod("print")
  }
  invisible(x)
}
