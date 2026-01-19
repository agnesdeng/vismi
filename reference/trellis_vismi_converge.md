# Trelliscope Visualisation of Convergence Diagnostics

Generates a Trelliscope display for convergence diagnostics across all
variables.

## Usage

``` r
trellis_vismi_converge(
  obj,
  tick_vals = NULL,
  color_pal = NULL,
  title = "auto",
  subtitle = "auto",
  nrow = 2,
  ncol = 3,
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

- title:

  A string specifying the title of the plot. If NULL, no title is shown.
  If "auto", a title will be generated based on the input. Default is
  "auto".

- subtitle:

  A string specifying the subtitle of the plot. If NULL, no subtitle is
  shown. If "auto", a title will be generated based on the input.
  Default is "auto".

- nrow:

  Number of rows in the Trelliscope display. Default is 2.

- ncol:

  Number of columns in the Trelliscope display. Default is 4.

- path:

  Optional path to save the Trelliscope display. If NULL, the display
  will not be saved to disk.

- ...:

  Additional arguments to customize the Trelliscope display.

## Value

A Trelliscope display object visualising convergence diagnostics for all
variables.
