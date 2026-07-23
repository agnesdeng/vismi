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
#' @param m An integer specifying the number of imputed datasets used for visualisation. It should be smaller than \code{length(imp_list)}. Default is NULL (plot all).
#' @param imp_idx A vector of integers specifying the indices of imputed datasets to plot. Default is NULL (plot all).
#' @param interactive A logical value indicating whether to create an interactive plotly plot (TRUE
#' by default) or a static ggplot2 plot (FALSE).
#' @param title A string specifying the title of the plot. Default is "auto" (automatic title based on \code{x,y,z} input). If NULL, no title is shown.
#' @param subtitle A string specifying the subtitle of the plot. Default is "auto" (automatic subtitle based on \code{x,y,z} input). If NULL, no subtitle is shown.
#' @param integerAsFactor A logical value indicating whether to treat integer variables as factors
#' (TRUE) or numeric (FALSE). Default is FALSE.
#' @param plotly_style A named list of style overrides for interactive
#'   (\code{interactive = TRUE}) plots. Unrecognised keys are silently ignored.
#'   Default is \code{list()} (use all defaults). Valid keys:
#'   \describe{
#'     \item{\code{title_size}}{Title font size (default 20).}
#'     \item{\code{title_color}}{Title colour (default \code{"#242429"}).}
#'     \item{\code{title_font}}{Title font family
#'       (default \code{"Helvetica, Arial, sans-serif"}).}
#'     \item{\code{axis_title_size}}{Axis title font size (default 14).}
#'     \item{\code{axis_title_font}}{Axis title font family
#'       (default \code{"Arial Black"}).}
#'     \item{\code{axis_title_color}}{Axis title colour
#'       (default \code{"#35353d"}).}
#'     \item{\code{plot_bgcolor}}{Background colour of the plot area
#'       (default \code{"#f2f7fc"}).}
#'     \item{\code{paper_bgcolor}}{Background colour of the full figure
#'       (default \code{"#fff"}).}
#'     \item{\code{gridcolor}}{Grid line colour (default \code{"#999"}).}
#'   }
#'   Additional keys for 3D numeric plots (\code{x}, \code{y}, \code{z} all
#'   numeric):
#'   \describe{
#'     \item{\code{scene3d_domain_x}}{Numeric vector of length 2 defining the
#'       horizontal extent of the 3D scene on the paper
#'       (default \code{c(0, 1)}, full width).}
#'     \item{\code{scene3d_domain_y}}{Numeric vector of length 2 defining the
#'       vertical extent of the 3D scene on the paper
#'       (default \code{c(0.05, 0.95)}).}
#'     \item{\code{scene3d_title_y}}{Vertical position of the title in paper
#'       coordinates (default \code{0.9}). Decrease to move the title down
#'       and reduce the gap between the title and the 3D box.}
#'     \item{\code{scene3d_margin_t}}{Top margin in pixels (default \code{0}).}
#'     \item{\code{scene3d_margin_r}}{Right margin in pixels (default
#'       \code{0}).}
#'     \item{\code{scene3d_margin_b}}{Bottom margin in pixels (default
#'       \code{0}).}
#'     \item{\code{scene3d_margin_l}}{Left margin in pixels (default
#'       \code{0}).}
#'     \item{\code{scene3d_eye_x}}{Camera eye x position (default
#'       \code{1.25}). Negative values flip the viewing direction.}
#'     \item{\code{scene3d_eye_y}}{Camera eye y position (default
#'       \code{1.25}).}
#'     \item{\code{scene3d_eye_z}}{Camera eye z position (default
#'       \code{1.25}). Smaller values lower the viewing angle; values near
#'       \code{0} give a near-horizontal view.}
#'   }
#' @param gg_style A named list of style overrides for static
#'   (\code{interactive = FALSE}) plots. Unrecognised keys are silently
#'   ignored. Default is \code{list()} (use all defaults). Valid keys:
#'   \describe{
#'     \item{\code{gg_title_size}}{Title font size (default 14).}
#'     \item{\code{gg_title_face}}{Title font face, e.g. \code{"bold"}
#'       (default).}
#'     \item{\code{title_color}}{Title colour (default \code{"#242429"}).}
#'     \item{\code{gg_subtitle_size}}{Subtitle font size (default 14).}
#'     \item{\code{gg_subtitle_face}}{Subtitle font face (default
#'       \code{"plain"}).}
#'     \item{\code{subtitle_color}}{Subtitle colour
#'       (default \code{"#242429"}).}
#'     \item{\code{gg_axis_title_size}}{Axis title font size (default 10).}
#'     \item{\code{gg_axis_title_face}}{Axis title font face (default
#'       \code{"bold"}).}
#'     \item{\code{axis_title_color}}{Axis title colour
#'       (default \code{"#35353d"}).}
#'     \item{\code{gg_axis_text_size}}{Axis tick label font size
#'       (default 9).}
#'     \item{\code{panel_bg_fill}}{Panel background fill colour
#'       (default \code{"gray95"}).}
#'     \item{\code{panel_bg_color}}{Panel background border colour
#'       (default \code{NA}).}
#'     \item{\code{strip_bg_fill}}{Facet strip background fill
#'       (default \code{"gray85"}).}
#'     \item{\code{strip_bg_color}}{Facet strip background border colour
#'       (default \code{NA}).}
#'     \item{\code{grid_major_color}}{Major grid line colour
#'       (default \code{"white"}).}
#'     \item{\code{grid_major_linewidth}}{Major grid line width
#'       (default \code{0.3}).}
#'     \item{\code{grid_minor_color}}{Minor grid line colour
#'       (default \code{"white"}).}
#'     \item{\code{grid_minor_linewidth}}{Minor grid line width
#'       (default \code{0.2}).}
#'   }
#' @param color_pal A named vector of colors for different imputation sets. If NULL
#' (default), a default color palette is used.
#' @param marginal_x A character string specifying the type of marginal plot to add for the x variable in 2D plots.
#' Options are "hist", "box", "rug", "box+rug"(default), or NULL when interactive = TRUE.
#' Options are "box", "rug", "box+rug"(default), or NULL when interactive = FALSE.
#' @param marginal_y A character string specifying the type of marginal plot to add
#' for the y variable in 2D plots. Options are "hist", "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = TRUE. Options are "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = FALSE.
#' @param verbose A logical value indicating whether to print extra information. Default is FALSE.
#' @param ... Additional arguments passed to the underlying plotting functions, such as point_size, alpha, nbins, width, and boxpoints.
#' @return A plotly or ggplot2 object visualising the multiply-imputed data.
#' @export
#' @examples
#' vismi(data = nhanes3, imp_list = imp_nhanes3, x = "weight_kg", y = "head_circumference_cm", z="sex")
vismi <- function(data, imp_list, x = NULL, y = NULL, z = NULL,
                  m = NULL, imp_idx = NULL, interactive = FALSE,
                  integerAsFactor = FALSE, title = "auto", subtitle = "auto",
                  color_pal = NULL, plotly_style = list(), gg_style = list(),
                  marginal_x = "box+rug", marginal_y = NULL,
                  verbose = FALSE, ...) {
  # check data
  data <- .validate_data(data = data, integerAsFactor = integerAsFactor, max_levels = round(0.5 * nrow(data)),verbose = verbose)

  Types <- attr(data, "Types")
  attr(data, "Types") <- NULL

  #data <- out$data
  #Types <- out$Types

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

  out<-.validate_m_imp_idx(imp_list = imp_list, m = m, imp_idx = imp_idx)
  imp_idx <- out$imp_idx
  plot_idx_msg<- out$plot_idx_msg

  # users_params<-list()

  users_params <- list(...)

  if (interactive) {
    params <- modifyList(.vismi_interactive_params(), users_params)
    resolved_plotly_style <- modifyList(.vismi_plotly_style(), plotly_style)
  } else {
    params <- modifyList(.vismi_static_params(), users_params)
    resolved_gg_style <- modifyList(.vismi_gg_style(), gg_style)
  }

  point_size <- params$point_size
  alpha <- params$alpha
  nbins <- params$nbins
  width <- params$width
  boxpoints <- params$boxpoints

  # preprocess data
  pre <- preprocess(data = data, imp_list = imp_list, imp_idx = imp_idx, vars = vars, integerAsFactor = integerAsFactor)
  all_dt <- pre$all_dt
  if (is.null(color_pal)) {
    color_pal <- pre$color_pal
  }
  no_missing <- pre$no_missing

  # only print out data summary if verbose = TRUE
  if(isTRUE(verbose)){
    .data_summary(pre = pre, plot_idx_msg = plot_idx_msg)
  }

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
    boxpoints = boxpoints,
    plotly_style = if (interactive) resolved_plotly_style else list(),
    gg_style = if (!interactive) resolved_gg_style else list()
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
#' @return A \code{vismi} object, returned invisibly.
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
