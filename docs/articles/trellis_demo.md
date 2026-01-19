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

## Overimputation diagnostics via `trellis_overimp()`

``` r
obj <- overimp(data = newborn, p = 0.2, m = 5, test_ratio = 0.2,
    method = "mixgb")
```

``` r
trellis_vismi_overimp(obj = obj, stack_y = TRUE)
```

## Convergence diagnostics via `trellis_vismi_converge()`

``` r
library(mixgb)
set.seed(2026)
mixgb_obj <- mixgb(data = newborn, m = 5, maxit = 5, save.models = TRUE)
```

``` r
trellis_vismi_converge(obj = mixgb_obj)
```

``` r
library(mice)
set.seed(2026)
mice_obj <- mice(data = newborn, m = 5, maxit = 5, printFlag = FALSE)
```

``` r
trellis_vismi_converge(obj = mice_obj)
```
