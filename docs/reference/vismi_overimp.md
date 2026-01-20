# Visualise Multiple Imputation Through Overimputation

This function provides visual diagnostic tools for assessing multiply
imputed datasets imputed with 'mixgb' or other imputers through
overimputation. It supports visualisation for both training and test
set.

## Usage

``` r
vismi_overimp(
  obj,
  x = NULL,
  y = NULL,
  z = NULL,
  m = NULL,
  imp_idx = NULL,
  integerAsFactor = FALSE,
  title = "auto",
  subtitle = "auto",
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

  Overimputation object of class 'overimp' created by the
  [`overimp()`](overimp.md) function.

- x:

  A character string specifying the name of the variable to plot on the
  x

- y:

  A character string specifying the name of the variable to plot on the
  y

- z:

  A character string specifying the name of the variable to plot on the
  z

- m:

  A single positive integer specifying the number of imputed datasets to
  plot. It should be smaller than the total number of imputed datasets
  in the object.

- imp_idx:

  A vector of integers specifying the indices of imputed datasets to
  plot.

- integerAsFactor:

  A logical indicating whether integer variables should be treated as
  factors. Default is FALSE (treated as numeric).

- title:

  A string specifying the title of the plot. Default is "auto"
  (automatic title based on `x,y,z` input). If NULL, no title is shown.

- subtitle:

  A string specifying the subtitle of the plot. Default is "auto"
  (automatic subtitle based on `x,y,z` input). If NULL, no subtitle is
  shown.

- num_plot:

  A character string specifying the type of plot for numeric variables.

- fac_plot:

  A character string specifying the type of plot for categorical
  variables.

- train_color_pal:

  A vector of colors for the training data. If NULL, default colors will
  be used.

- test_color_pal:

  A vector of colors for the test data. If NULL, default colors will be
  used.

- stack_y:

  A logical indicating whether to stack y values in certain plots.
  Default is FALSE.

- diag_color:

  A character string specifying the color of the diagonal line in
  scatter plots. Default is NULL.

- seed:

  An integer specifying the random seed for reproducibility. Default is
  2025.

- ...:

  Additional arguments to customize the plots, such as position,
  point_size, linewidth, alpha, xlim, ylim, boxpoints, width.

## Examples

``` r
if (FALSE) { # \dontrun{
obj <- overimp(data = newborn, m = 5, p = 0.2, test_ratio = 0.2, method = "mixgb")
vismi_overimp(obj = obj, x = "head_circumference_cm", num_plot = "cv")
} # }
```
