# Inspect all variables at once with trelliscopejs

With the support of
[trelliscopejs](https://hafen.github.io/trelliscopejs/), **vismi**
provides trelliscope displays to inspect all variables at once via

- [`trellis_vismi()`](../reference/trellis_vismi.md)

- [`trellis_vismi_overimp()`](../reference/trellis_vismi_overimp.md)

- [`trellis_vismi_converge()`](../reference/trellis_vismi_converge.md)

``` r
library(vismi)
library(mice)
library(mixgb)
```

## Distributional characteristics via `trellis_vismi()`

``` r
trellis_vismi(data = newborn, imp_list = imp_newborn, marginal_x = "box+rug")
```
