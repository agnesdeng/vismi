# Visualise onvergence diagnostics

Visualise onvergence diagnostics

Visualise onvergence diagnostics for a variable imputed by mixgb

Visualise convergence diagnostics for a variable imputed by MICE

## Usage

``` r
vismi_converge(obj, var, tick_vals = NULL, color_pal = NULL)

# S3 method for class 'mixgb'
vismi_converge(obj, var, tick_vals = NULL, color_pal = NULL)

# S3 method for class 'mids'
vismi_converge(obj, var, tick_vals = NULL, color_pal = NULL)
```

## Arguments

- obj:

  A 'mixgb' object returned by `mixgb()` function or a 'mids' object
  returned by the `mice()` function.

- var:

  The name of the variable to plot convergence for.

- tick_vals:

  Optional numeric vector specifying x-axis tick values for iterations.

- color_pal:

  A vector of m color codes (e.g., hex codes). If NULL, default colors
  will be used.

## Value

A ggplot2 object showing the convergence plot for the specified
variable.
