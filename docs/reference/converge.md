# Convergence Dignostics Plot

Convergence Dignostics Plot

Plot convergence diagnostics for a variable imputed by mixgb

Plot convergence diagnostics for a variable imputed by MICE

## Usage

``` r
converge(obj, var, tick_vals = NULL, color_pal = NULL)

# S3 method for class 'mixgb'
converge(obj, var, tick_vals = NULL, color_pal = NULL)

# S3 method for class 'mids'
converge(obj, var, tick_vals = NULL, color_pal = NULL)
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
