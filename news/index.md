# Changelog

## vismi 0.9.0

#### New Features

- Convergence diagnostics visualisation through `converge()` function.
- Convergence diagnostics with trelliscope display through
  `converge_trelliscope()` function.

## vismi 0.8.1

#### Bug Fixes

- Fixed overlapping issues in interative visualisation:
  - x-axis title with legend items
  - left and right axis titles in subplots

## vismi 0.8.0

#### New Features

- Support static visualisation for 3 numeric variables and 3 factor
  variables for `vismi.data.frame()` method.
- Support interactive visualisation for 3 factor variables for
  `vismi.data.frame()` method.
- Added `verbose` argument to print missing data summary \###
  Refactoring
- Validation checks
- Aesthetics for panel labels and titles

## vismi 0.7.0

#### Deprecations

- Deprecated `trelliscopejs_con()` and `trelliscopejs_cat()`. \### New
  Features
- `ts_overimp()` Support showing all overimputation 1D plots for all
  numeric and factors variables through `trelliscopejs`

## vismi 0.6.0

#### New Features

- subset imputed datasets for plotting via `imp_idx` or `m` argument in
  [`vismi()`](../reference/vismi.md) function. \### Bug Fixes
- Misalignment issue of using plotly subplots in `.plotly_box_facet()`.
  \### Refactoring
- Refactored other functions.

## vismi 0.5.0

#### Refactoring

- Refactored all overimputation visualisation functions for 1D & 2D.
- `overimpute()` function refactored to improve code readability and
  maintainability.
- Added S3 methods for visualising overimputation object.
- Added `vismi.data.frame` method.
  - `vismi(data, imp_list, x, y, z,...)` visualises multiple imputed
    datasets for up to 3 variables.
- Added `vismi.overimp` method.
  - `vismi(obj, x, y, z,...)` visualises overimputation results

## vismi 0.4.0

#### New Features

- Added 1D, 2D, and 3D interactive inspection visualisation using
  `plotly`.

## vismi 0.3.0

#### New Features

- Added more features for static visualisation using`ggplot2`.

## vismi 0.2.0

#### Refactoring

- Added preprocessing function
- Default parameters handling
- A high-level main function [`vismi()`](../reference/vismi.md) will be
  used to call all other inspection functions.

## vismi 0.1.0

#### Initial Release

- First development version releases on Github.
- As illustrated in my PhD thesis.
