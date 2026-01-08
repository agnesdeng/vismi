# Trelliscope Visualisation of Convergence Diagnostics

Generates a Trelliscope display for convergence diagnostics across all
variables.

## Usage

``` r
converge_trelliscope(
  obj,
  tick_vals = NULL,
  color_pal = NULL,
  nrow = 2,
  ncol = 4,
  path = NULL,
  ...
)
```

## Arguments

- obj:

  An object of class 'mixgb' or 'mids' containing intermediate imputed
  result for each iteration.

- tick_vals:

  A numeric vector specifying the tick values for the x-axis
  (iterations). If NULL, default tick values will be used.

- color_pal:

  A vector of colors to use for the imputation lines. If NULL, default
  colors will be used.

- nrow:

  Number of rows in the Trelliscope display. Default is 2.

- ncol:

  Number of columns in the Trelliscope display. Default is 2.

- path:

  Optional path to save the Trelliscope display. If NULL, the display
  will not be saved to disk.

- ...:

  Additional arguments to customize the Trelliscope display.

## Value

A Trelliscope display object visualising convergence diagnostics for all
variables.
