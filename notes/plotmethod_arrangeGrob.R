

#' print method for overimp_plot objects
#' @describeIn vismi Print method for overimp_plot objects
#' @param x An object of class 'overimp_plot' created by the \code{vismi.overimp()} function.
#' @param ... Additional arguments (not used).
#' @exportS3Method
print.overimp_plot <- function(x, ...) {
  # Check if it's a gtable/grob (from arrangeGrob)
  if (inherits(x, "gtable") || inherits(x, "grob")) {
    grid::grid.newpage()  # Clear the plot area
    grid::grid.draw(x)    # Draw the plot
  } else if (inherits(x, "ggplot")) {
    # It's a ggplot object - use ggplot's print method
    NextMethod("print")  # Call print.ggplot
  }
  invisible(x)
}

#' plot method for overimp_plot objects
#' @describeIn vismi Plot method for overimp_plot objects
#' @param x An object of class 'overimp_plot' created by the \code{vismi.overimp()} function.
#' @param ... Additional arguments (not used).
#' @exportS3Method
plot.overimp_plot <- function(x, ...) {
  print(x, ...)
}
