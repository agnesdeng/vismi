
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vismi

Visualisation Tools for Multiple imputation <!-- badges: start -->
<!-- badges: end -->

## Overview

The package has been completely redesigned. Please try the new
development version on GitHub and I’ll submit to CRAN soon :)

- `vismi()`: visualise multiply-imputed missing data through
  distributional characteristics

- `vismi_overimp()`: visualise multiply-imputed missing data through
  overimputation

- `vismi_converge()`: visualise convergence disgnostics for imputed
  values of an incomplete variable

With the support of
[trelliscopejs](https://hafen.github.io/trelliscopejs/), **vismi**
provides trelliscope displays to inspect all variables at once via

- `trellis_vismi()`

- `trellis_vismi_overimp()`

- `trellis_vismi_converge()`

## Installation

You can install the current development version of vismi from
[GitHub](https://github.com/agnesdeng) with:

``` r
# install.packages("devtools")
devtools::install_github("agnesdeng/vismi")
```
