# Trelliscope Visualisation of Overimputation Diagnostics

Generates a Trelliscope display for overimputation diagnostics across
all variables.

## Usage

``` r
vismi_trelliscope(
  obj,
  m = NULL,
  imp_idx = NULL,
  integerAsFactor = TRUE,
  num_plot = "cv",
  fac_plot = "cv",
  train_color_pal = NULL,
  test_color_pal = NULL,
  stack_y = FALSE,
  diag_color = NULL,
  seed = 2025,
  ...
)
```

## Arguments

- obj:

  An object of class 'overimp' containing imputed datasets and
  parameters.

- m:

  A single positive integer specifying the number of imputed datasets to
  plot. It should be smaller than the total number of imputed datasets
  in the object. Default is NULL ( plot all).

- imp_idx:

  A vector of integers specifying the indices of imputed datasets to
  plot. Default is NULL (plot all).

- integerAsFactor:

  A logical indicating whether integer variables should be treated as
  factors. Default is FALSE (treated as numeric).

- num_plot:

  A character string specifying the type of plot for numeric variables.
  Options are "cv" (cross-validation), "ridge", or "density". Default is
  "cv".

- fac_plot:

  A character string specifying the type of plot for categorical
  variables. Options are "cv" (cross-validation), "bar", or "dodge".
  Default is "cv".

- train_color_pal:

  A vector of colors for the training data. If NULL, default colors will
  be used.

- test_color_pal:

  A vector of colors for the test data. If NULL, default colors will be
  used.

- stack_y:

  A logical indicating whether to stack y-values in the plots. Default
  is FALSE.

- diag_color:

  A color specification for the diagonal line in the plots. Default is
  NULL.

- seed:

  An integer seed for reproducibility. Default is 2025.

- ...:

  Additional arguments to customize the plots, such as point_size, xlim,
  ylim.

## Value

A Trelliscope display object visualising overimputation diagnostics for
all variables.
