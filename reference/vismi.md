# Visualise Multiple Imputations Through Distributional Characteristics

This function provides visual diagnostic tools for assessing multiply
imputed datasets created with 'mixgb' or other imputers through
inspecting the distributional characteristics of imputed variables. It
supports 1D, 2D, and 3D visualisations for numeric and categorical
variables using either interactive or static plots.

## Usage

``` r
vismi(
  data,
  imp_list,
  x = NULL,
  y = NULL,
  z = NULL,
  m = NULL,
  imp_idx = NULL,
  interactive = FALSE,
  integerAsFactor = FALSE,
  title = "auto",
  subtitle = "auto",
  color_pal = NULL,
  plotly_style = list(),
  gg_style = list(),
  marginal_x = "box+rug",
  marginal_y = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing the original data with missing values.

- imp_list:

  A list of imputed data frames.

- x:

  A character string specifying the name of the variable to plot on the
  x axis. Default is NULL.

- y:

  A character string specifying the name of the variable to plot on the
  y axis. Default is NULL.

- z:

  A character string specifying the name of the variable to plot on the
  z axis. Default is NULL.

- m:

  An integer specifying the number of imputed datasets used for
  visualisation. It should be smaller than `length(imp_list)`. Default
  is NULL (plot all).

- imp_idx:

  A vector of integers specifying the indices of imputed datasets to
  plot. Default is NULL (plot all).

- interactive:

  A logical value indicating whether to create an interactive plotly
  plot (TRUE by default) or a static ggplot2 plot (FALSE).

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

- plotly_style:

  A named list of style overrides for interactive (`interactive = TRUE`)
  plots. Unrecognised keys are silently ignored. Default is
  [`list()`](https://rdrr.io/r/base/list.html) (use all defaults). Valid
  keys:

  `title_size`

  :   Title font size (default 20).

  `title_color`

  :   Title colour (default `"#242429"`).

  `title_font`

  :   Title font family (default `"Helvetica, Arial, sans-serif"`).

  `axis_title_size`

  :   Axis title font size (default 14).

  `axis_title_font`

  :   Axis title font family (default `"Arial Black"`).

  `axis_title_color`

  :   Axis title colour (default `"#35353d"`).

  `plot_bgcolor`

  :   Background colour of the plot area (default `"#f2f7fc"`).

  `paper_bgcolor`

  :   Background colour of the full figure (default `"#fff"`).

  `gridcolor`

  :   Grid line colour (default `"#999"`).

  Additional keys for 3D numeric plots (`x`, `y`, `z` all numeric):

  `scene3d_domain_x`

  :   Numeric vector of length 2 defining the horizontal extent of the
      3D scene on the paper (default `c(0, 1)`, full width).

  `scene3d_domain_y`

  :   Numeric vector of length 2 defining the vertical extent of the 3D
      scene on the paper (default `c(0.05, 0.95)`).

  `scene3d_title_y`

  :   Vertical position of the title in paper coordinates (default
      `0.9`). Decrease to move the title down and reduce the gap between
      the title and the 3D box.

  `scene3d_margin_t`

  :   Top margin in pixels (default `0`).

  `scene3d_margin_r`

  :   Right margin in pixels (default `0`).

  `scene3d_margin_b`

  :   Bottom margin in pixels (default `0`).

  `scene3d_margin_l`

  :   Left margin in pixels (default `0`).

  `scene3d_eye_x`

  :   Camera eye x position (default `1.25`). Negative values flip the
      viewing direction.

  `scene3d_eye_y`

  :   Camera eye y position (default `1.25`).

  `scene3d_eye_z`

  :   Camera eye z position (default `1.25`). Smaller values lower the
      viewing angle; values near `0` give a near-horizontal view.

- gg_style:

  A named list of style overrides for static (`interactive = FALSE`)
  plots. Unrecognised keys are silently ignored. Default is
  [`list()`](https://rdrr.io/r/base/list.html) (use all defaults). Valid
  keys:

  `gg_title_size`

  :   Title font size (default 14).

  `gg_title_face`

  :   Title font face, e.g. `"bold"` (default).

  `title_color`

  :   Title colour (default `"#242429"`).

  `gg_subtitle_size`

  :   Subtitle font size (default 14).

  `gg_subtitle_face`

  :   Subtitle font face (default `"plain"`).

  `subtitle_color`

  :   Subtitle colour (default `"#242429"`).

  `gg_axis_title_size`

  :   Axis title font size (default 10).

  `gg_axis_title_face`

  :   Axis title font face (default `"bold"`).

  `axis_title_color`

  :   Axis title colour (default `"#35353d"`).

  `gg_axis_text_size`

  :   Axis tick label font size (default 9).

  `panel_bg_fill`

  :   Panel background fill colour (default `"gray95"`).

  `panel_bg_color`

  :   Panel background border colour (default `NA`).

  `strip_bg_fill`

  :   Facet strip background fill (default `"gray85"`).

  `strip_bg_color`

  :   Facet strip background border colour (default `NA`).

  `grid_major_color`

  :   Major grid line colour (default `"white"`).

  `grid_major_linewidth`

  :   Major grid line width (default `0.3`).

  `grid_minor_color`

  :   Minor grid line colour (default `"white"`).

  `grid_minor_linewidth`

  :   Minor grid line width (default `0.2`).

- marginal_x:

  A character string specifying the type of marginal plot to add for the
  x variable in 2D plots. Options are "hist", "box", "rug",
  "box+rug"(default), or NULL when interactive = TRUE. Options are
  "box", "rug", "box+rug"(default), or NULL when interactive = FALSE.

- marginal_y:

  A character string specifying the type of marginal plot to add for the
  y variable in 2D plots. Options are "hist", "box", "rug", "box+rug",
  or NULL (default, no marginal plot) when interactive = TRUE. Options
  are "box", "rug", "box+rug", or NULL (default, no marginal plot) when
  interactive = FALSE.

- verbose:

  A logical value indicating whether to print extra information. Default
  is FALSE.

- ...:

  Additional arguments passed to the underlying plotting functions, such
  as point_size, alpha, nbins, width, and boxpoints.

## Value

A plotly or ggplot2 object visualising the multiply-imputed data.

## Examples

``` r
vismi(data = nhanes3, imp_list = imp_nhanes3, x = "weight_kg", y = "head_circumference_cm", z="sex")
```
