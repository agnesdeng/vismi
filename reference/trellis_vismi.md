# Trelliscope Visualisation of Distributional Characteristics

Generates a Trelliscope display for distributional characteristics
across all variables.

## Usage

``` r
trellis_vismi(
  data,
  imp_list,
  m = NULL,
  imp_idx = NULL,
  integerAsFactor = FALSE,
  title = "auto",
  subtitle = "auto",
  color_pal = NULL,
  marginal_x = "box+rug",
  nrow = 2,
  ncol = 4,
  path = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing the original data with missing values.

- imp_list:

  A list of imputed data frames.

- m:

  An integer specifying the number of imputed datasets to plot. It
  should be smaller than `length(imp_list)`. Default is NULL (plot all).

- imp_idx:

  A vector of integers specifying the indices of imputed datasets to
  plot. Default is NULL (plot all).

- integerAsFactor:

  A logical value indicating whether to treat integer variables as
  factors (TRUE) or numeric (FALSE). Default is FALSE.

- title:

  A string specifying the title of the plot. Default is "auto"
  (automatic title based on `x,y,z` input). If NULL, no title is shown.

- subtitle:

  A string specifying the subtitle of the plot. Default is "auto"
  (automatic subtitle based on `x,y,z` input). If NULL, no subtitle is
  shown.

- color_pal:

  A named vector of colors for different imputation sets. If NULL
  (default), a default color palette is used.

- marginal_x:

  A character string specifying the type of marginal plot to add for the
  x variable in 2D plots. Options are "hist", "box", "rug", "box+rug",
  or NULL (default, no marginal plot) when interactive = TRUE. Options
  are "box", "rug", "box+rug", or NULL (default, no marginal plot) when
  interactive = FALSE.

- nrow:

  Number of rows in the Trelliscope display. Default is 2.

- ncol:

  Number of columns in the Trelliscope display. Default is 4.

- path:

  Optional path to save the Trelliscope display. If NULL, the display
  will not be saved to disk.

- verbose:

  A logical value indicating whether to print extra information. Default
  is FALSE.

- ...:

  Additional arguments passed to the underlying plotting functions, such
  as point_size, alpha, nbins, width, and boxpoints.

## Value

A Trelliscope display object visualising distributional characteristics
for all variables.

## Examples

``` r
trellis_vismi(data = nhanes3, imp_list = imp_nhanes3, marginal_x = "box")

{"x":{"id":"a2c128eb","config_info":"'appfiles/config.jsonp'","self_contained":false,"latest_display":{"name":"Distributional_characteristics_for_multiply_imputed_values_across_all_variables","group":"common"},"spa":true,"in_knitr":false,"in_shiny":false,"in_notebook":false},"evals":[],"jsHooks":[]}
```
