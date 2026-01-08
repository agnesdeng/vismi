# vismi

Visualisation Tools for Multiple imputation

## Overview

The package has been completely redesigned. Please try the new
development version on GitHub and I’ll submit to CRAN soon :)

- [`vismi()`](reference/vismi.md): visualise multiply-imputed missing
  data through distributional characteristics

- [`vismi_overimp()`](reference/vismi_overimp.md): visualise
  multiply-imputed missing data through overimputation

- [`vismi_converge()`](reference/vismi_converge.md): visualise
  convergence disgnostics for imputed values of an incomplete variable

With the support of
[trelliscopejs](https://hafen.github.io/trelliscopejs/), **vismi**
provides trelliscope displays to inspect all variables at once via

- [`trellis_vismi()`](reference/trellis_vismi.md)

- [`trellis_vismi_overimp()`](reference/trellis_vismi_overimp.md)

- [`trellis_vismi_converge()`](reference/trellis_vismi_converge.md)

## Installation

You can install the current development version of vismi from
[GitHub](https://github.com/agnesdeng) with:

``` r
# install.packages("devtools")
devtools::install_github("agnesdeng/vismi")
```
