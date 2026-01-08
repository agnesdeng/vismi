# vismi-vignettes

``` r
# devtools::document() devtools::load_all()
library(vismi)
```

``` r
vismi(data = newborn, imp_list = imp_newborn, x = "BMPHEAD",
    interactive = TRUE)
#>   Variable Count
#> 1  BMPHEAD   124
```

``` r
vismi(data = newborn, imp_list = imp_newborn, x = "BMPHEAD",
    interactive = FALSE)
#>   Variable Count
#> 1  BMPHEAD   124
```

![](vismi-vignettes_files/figure-html/unnamed-chunk-3-1.png)
