
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vismi

<!-- badges: start -->
<!-- badges: end -->

Visualisation Tools for Multiple imputation

## Installation

You can install the current development version of mixgb from
[GitHub](https://github.com/agnesdeng) with:

``` r
# install.packages("devtools")
devtools::install_github("agnesdeng/vismi")
```

# 1. Introduction

It is important to assess the plausibility of imputations before doing
analysis. The `mixgb` package provides several visual diagnostic
functions using `ggplot2` to compare multiply imputed values versus
observed data.

## 1.1 Inspecting imputed values

We will demonstrate these functions using the `nhanes3_newborn` dataset.
In the original data, almost all missing values occurred in numeric
variables. Only seven observations are missing in the binary factor
variable `HFF1` .

``` r
devtools::load_all()
library(vismi)
colSums(is.na(nhanes3_newborn))
#> HSHSIZER  HSAGEIR    HSSEX DMARACER DMAETHNR DMARETHN  BMPHEAD BMPRECUM 
#>        0        0        0        0        0        0      124      114 
#>   BMPSB1   BMPSB2   BMPTR1   BMPTR2    BMPWT   DMPPIR     HFF1     HYD1 
#>      161      169      124      167      117      192        7        0
```

In order to visualize some imputed values for other types of variables,
we create some extra missing values in `HSHSIZER` (integer), `HSAGEIR`
(integer), `HSSEX` (binary factor), `DMARETHN` (multiclass factor) and
`HYD1` (Ordinal factor) under MCAR.

``` r
withNA.df <- createNA(data = nhanes3_newborn, var.names = c("HSHSIZER",
    "HSAGEIR", "HSSEX", "DMARETHN", "HYD1"), p = 0.1)
colSums(is.na(withNA.df))
#> HSHSIZER  HSAGEIR    HSSEX DMARACER DMAETHNR DMARETHN  BMPHEAD BMPRECUM 
#>      211      211      211        0        0      211      124      114 
#>   BMPSB1   BMPSB2   BMPTR1   BMPTR2    BMPWT   DMPPIR     HFF1     HYD1 
#>      161      169      124      167      117      192        7      211
```

We then impute this dataset using `mixgb` with default settings. A list
of five imputed datasets are assigned to `imputed.data`. The dimension
of each imputed dataset will be the same as the original data.

``` r
library(mixgb)
imputed.data <- mixgb(data = withNA.df, m = 5)
```

We can check the actual imputed values of a single variable using the
function `show_var().` It will return a data.table of `m` columns, each
column contains one set of imputed values for the specified variable.
Only the imputed values of missing entries will be shown.

``` r
show_var(imputation.list = imputed.data, var.name = "BMPHEAD",
    original.data = withNA.df)
#>        m1   m2   m3   m4   m5
#>   1: 43.9 45.8 43.7 43.5 42.2
#>   2: 45.1 45.7 46.2 44.5 45.5
#>   3: 43.6 42.6 42.2 44.3 41.7
#>   4: 44.9 43.8 39.6 43.0 45.0
#>   5: 43.1 44.2 44.0 45.8 43.2
#>  ---                         
#> 120: 48.0 44.1 47.8 45.2 47.1
#> 121: 43.4 43.8 43.6 45.1 44.2
#> 122: 41.5 41.3 42.0 41.5 41.0
#> 123: 43.7 43.9 42.0 45.6 42.6
#> 124: 44.3 45.0 46.4 45.2 44.3
show_var(imputation.list = imputed.data, var.name = "HFF1", original.data = withNA.df)
#>    m1 m2 m3 m4 m5
#> 1:  2  2  2  2  2
#> 2:  2  2  2  2  2
#> 3:  1  1  2  1  1
#> 4:  1  2  1  1  2
#> 5:  2  1  1  1  1
#> 6:  1  1  1  2  1
#> 7:  2  1  1  1  1
```

# 2 Visual diagnostics plots

The `mixgb` package provides the following visual diagnostics functions:

1)  Single variable: `plot_hist()`, `plot_box()`, `plot_bar()` ;

2)  Two variables: `plot_2num()`, `plot_2fac()`, `plot_1num1fac()` ;

3)  Three variables: `plot_2num1fac()`, `plot_1num2fac()`.

Each function will return `m+1` panels to compare the observed data with
`m` sets of actual imputed values.

## 2.1 Single variable

Only the imputations of missing entries in the specified variable will
be ploted in panels `m1` to `m5`. If a variable is fully observed, it
will return an error.

### 2.1.1 Numeric

We can plot an imputed numeric variable by histogram or boxplot.

-   `plot_hist()`: plot histograms with density curves.

    Histograms are good at displaying the distribution of numeric data.
    Users can identify any unusual pattern of the imputed values from
    their shapes. Under MCAR, we would expect the distribution of
    imputed values to be the same as that of the observed ones. Under
    MAR, the distributions of observed and imputed values can be quite
    different. However, it is still worth plotting the imputed data as
    any to odd values may indicate that the imputation procedure is
    unsatisfactory.

    ``` r
    plot_hist(imputation.list = imputed.data, var.name = "BMPHEAD",
        original.data = withNA.df)
    ```

    <img src="man/figures/README-unnamed-chunk-6-1.png" width="95%" />

-   `plot_box()`: plot box plots with overlaying data points.

    Users can use `plot_box()` to compare the median, lower and upper
    quantiles of imputed values with that of the observed ones. Also, we
    can see the difference between the number of missing values and
    observed values in the specified variable.

    ``` r
    plot_box(imputation.list = imputed.data, var.name = "BMPHEAD",
        original.data = withNA.df)
    ```

    <img src="man/figures/README-unnamed-chunk-7-1.png" width="95%" />

### 2.1.2 Factor

-   `plot_bar()`: plot bar plots

    The proportion of each level in a factor will be shown by
    `plot_bar()`.

    ``` r
    plot_bar(imputation.list = imputed.data, var.name = "HSSEX",
        original.data = withNA.df)
    ```

    <img src="man/figures/README-unnamed-chunk-8-1.png" width="95%" />

    ``` r
    plot_bar(imputation.list = imputed.data, var.name = "DMARETHN",
        original.data = withNA.df)
    ```

    <img src="man/figures/README-unnamed-chunk-8-2.png" width="95%" />

### 2.1.3 Integer

When producing a plot, an integer variable can be treated as either
numeric or factor. Users can plot an imputed integer variable according
to their preferences using the following functions:

-   `plot_hist()`: plot histograms with density curves

-   `plot_box()`: plot box plot with overlaying data points

-   `plot_bar()`: plot bar plot (treat an integer variable as a factor)

    ``` r
    plot_hist(imputation.list = imputed.data, var.name = "HSHSIZER",
        original.data = withNA.df)
    ```

    <img src="man/figures/README-unnamed-chunk-9-1.png" width="95%" />

    ``` r
    plot_box(imputation.list = imputed.data, var.name = "HSHSIZER",
        original.data = withNA.df)
    ```

    <img src="man/figures/README-unnamed-chunk-9-2.png" width="95%" />

    ``` r
    plot_bar(imputation.list = imputed.data, var.name = "HSHSIZER",
        original.data = withNA.df)
    ```

    <img src="man/figures/README-unnamed-chunk-9-3.png" width="95%" />

### 2.1.4 Ordinal factor

The function `mixgb()` will not convert any ordinal factor to integers
by default. So we can just plot ordinal factors as factors. (see Section
2.1.2).

However, setting `ordinalAsInteger = TRUE` in `mixgb()` may speed up the
imputation process, but users need to decide whether to transform them
back. In this case, we can just plot the imputed values as integers (see
Section 2.1.3).

``` r
imputed.data2 <- mixgb(data = withNA.df, m = 5, ordinalAsInteger = TRUE)

plot_bar(imputation.list = imputed.data2, var.name = "HYD1",
    original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="95%" />

``` r
plot_hist(imputation.list = imputed.data2, var.name = "HYD1",
    original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-10-2.png" width="95%" />

``` r
plot_box(imputation.list = imputed.data2, var.name = "HYD1",
    original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-10-3.png" width="95%" />

## 2.2 Two variables

To plot the multiply imputed values of two variables, at least one of
them has to be incomplete in the original data. In panels `m1` to `m5`,
missing data either occurs in either one of the variables.

### 2.2.1 Two numeric variables

We can plot scatter plots of two numeric variables using `plot_2num()`.
We can specify the name of a numeric variable on the x-axis in `var.x` ,
and another numeric variable on the y-axis in `var.y`.

Users can choose to plot the shapes of different types of missing values
by setting `shape = TRUE`. We only recommend plotting the shapes when
the dataset is small. By default, `shape = FALSE` to speed up the
plotting process.

``` r
plot_2num(imputation.list = imputed.data, var.x = "BMPHEAD",
    var.y = "BMPRECUM", original.data = withNA.df, shape = TRUE)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="95%" />

**NA.condition** represents the following types of missing values.

1)  `both.observed`: Both `var.x` and`var.y` are observed. This only
    appears in the first panel - `Observed` (Shape: diamond).

2)  `both.missing`: Imputed values where both `var.x` and `var.y` are
    originally missing (Shape: circle);

3)  `var.x.missing`: Imputed values where `var.x` is originally missing
    and `var.y` isn’t (Shape: X);

4)  `var.y.missing`: Imputed values where `var.y` is originally missing
    and `var.x` isn’t (Shape: Y).

### 2.2.2 One numeric vs one factor

We can plot a numeric variable versus a factor using `plot_1num1fac()`.
The output is a box plot with overlaying points. Users need to specify a
numeric variable in `var.num` and a factor in `var.fac`.

**NA.condition** is similar to the definition in Section 2.2.1.

``` r
plot_1num1fac(imputation.list = imputed.data, var.num = "BMPHEAD",
    var.fac = "HSSEX", original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="95%" />

### 2.2.3 Two factors

We can plot bar plots to show the relationship between two factors using
`plot_2fac()`. Note that this function requires at least one of the
variables to be incomplete in the original dataset.

``` r
plot_2fac(imputation.list = imputed.data, var.fac1 = "HYD1",
    var.fac2 = "HFF1", original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="95%" />

### 2.2.4 One numeric vs one integer

We can use `plot_2num()` to plot a numeric variable versus an integer
variable. Note that the plots would look different if we swap the
variables `var.x` and `var.y`.

``` r
plot_2num(imputation.list = imputed.data, var.x = "BMPHEAD",
    var.y = "HSAGEIR", original.data = withNA.df, shape = TRUE)
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="95%" />

``` r
plot_2num(imputation.list = imputed.data, var.x = "HSAGEIR",
    var.y = "BMPHEAD", original.data = withNA.df, shape = TRUE)
```

<img src="man/figures/README-unnamed-chunk-14-2.png" width="95%" />

If we treat an integer variable as factor, we can use `plot_1num1fac()`
.

``` r
plot_1num1fac(imputation.list = imputed.data, var.num = "BMPHEAD",
    var.fac = "HSAGEIR", original.data = withNA.df, shape = TRUE)
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="95%" />

### 2.2.5 Two integers

We can plot two variables of integer type using either `plot_2num()`
,`plot_1num1fac()` or `plot_2fac()`. Users should choose the plotting
functions based on the nature of the variable. For example, if an
integer variable `age` has data ranging from 0 to 110, it may be easier
to treat `age` as numeric rather than a factor. On the other hand, if we
have an integer variable which only has several distinct values
(e.g. 1,2,3), it may be better to treat them as factor for plotting. In
this dataset, we only have two variables of integer
type-`HSHSIZER`(household size) and `HSAGEIR`(baby’s age ranging from 2
to 11 months). We would not expect any obvious relation between these
two, but we plot them anyway.

``` r
plot_2num(imputation.list = imputed.data, var.x = "HSHSIZER",
    var.y = "HSAGEIR", original.data = withNA.df, shape = TRUE)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="95%" />

``` r
plot_1num1fac(imputation.list = imputed.data, var.num = "HSHSIZER",
    var.fac = "HSAGEIR", original.data = withNA.df, shape = TRUE)
```

<img src="man/figures/README-unnamed-chunk-16-2.png" width="95%" />

``` r
plot_2fac(imputation.list = imputed.data, var.fac1 = "HSHSIZER",
    var.fac2 = "HSAGEIR", original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-16-3.png" width="95%" />

## 2.3 Three variables

To plot the multiply imputed values of three variables, at least one of
them has to be incomplete in the original data.

### 2.3.1 Two numeric variables conditional on one factor

We can generate a conditional scatter plots for two numeric variables
using `plot_2num1fac()` . The variable on the x-axis should be specified
in `var.x`, while the y-axis one should be in `var.y` . The factor we
want to be conditional on is `con.fac`.

``` r
plot_2num1fac(imputation.list = imputed.data, var.x = "BMPHEAD",
    var.y = "BMPRECUM", con.fac = "HFF1", original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="95%" />

When we have three variables, there are
![2^3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2%5E3 "2^3")
different types of missing patterns, consisting of all possible
combinations of zero to three variables missing. However, it would be
hard to differentiate all 8 types of missingness in the same plots,
especially in the case when the dataset is not small. Therefore, we only
choose to show the following three types in **NA.condition** in the plot
when `shape = TRUE`.

**NA.condition** represents the following types of missing values.

1)  `all.observed`: Observations where all three variables are observed.
    This only appears in the first panel - `Observed` .

2)  `con.fac.observed`: Imputed values where `con.fac` is originally
    observed.

    (These points are originally missing in either `var.x` or `var.y` or
    both)

3)  `con.fac.missing`: imputed values where `con.fac` is originally
    missing. (These points can be originally observed, or missing in
    either `var.x` or `var.y` or both)

``` r
plot_2num1fac(imputation.list = imputed.data, var.x = "BMPHEAD",
    var.y = "BMPRECUM", con.fac = "DMARETHN", original.data = withNA.df,
    shape = TRUE)
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="95%" />

If we want to treat integer variables as numeric, we can put them in
either `var.x` or `var.y`. Here is an example, where `HSAGEIR` is an
integer variable with values ranging from 2 to 11.

``` r
plot_2num1fac(imputation.list = imputed.data, var.x = "HSAGEIR",
    var.y = "BMPRECUM", con.fac = "DMARETHN", original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-19-1.png" width="95%" />

### 2.3.2 One numeric variable and one factor conditional on another factor

`plot_1num2fac()` will generate boxplots with overlaying data points of
one numeric variable vs a factor, conditional on another factor.

``` r
plot_1num2fac(imputation.list = imputed.data, var.fac = "DMARETHN",
    var.num = "BMPRECUM", con.fac = "HSSEX", original.data = withNA.df)
```

<img src="man/figures/README-unnamed-chunk-20-1.png" width="95%" />

# 3 Color options

By default, the observed panel is gray and the other `m` panels use
`ggplot2`’s default color scheme.

``` r
plot_2num(imputation.list = imputed.data, var.x = "BMPHEAD",
    var.y = "BMPRECUM", original.data = withNA.df, color.pal = NULL)
```

<img src="man/figures/README-unnamed-chunk-21-1.png" width="95%" />

We can change the colors by providing a vector of color hex codes in the
argument `color.pal`. For example, we can use one of the
colorblind-friendly palette `Set2` from the R package `RColorBrewer`.
Note that if we have `m` imputed datasets, we need `m+1` hex codes
because we also have to display the `Observed` panel.

``` r
library(RColorBrewer)
color.codes <- brewer.pal(n = 6, name = "Set2")
color.codes
#> [1] "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F"

plot_2num(imputation.list = imputed.data, var.x = "BMPHEAD",
    var.y = "BMPRECUM", original.data = withNA.df, color.pal = color.codes)
```

<img src="man/figures/README-unnamed-chunk-22-1.png" width="95%" />

Otherwise, we can provide a vector of R’s built-in color names.

``` r
color.names <- c("gray50", "coral2", "goldenrod3", "darkolivegreen4",
    "slateblue1", "plum3")

plot_2num(imputation.list = imputed.data, var.x = "BMPHEAD",
    var.y = "BMPRECUM", original.data = withNA.df, color.pal = color.names)
```

<img src="man/figures/README-unnamed-chunk-23-1.png" width="95%" />

Here is a very useful R colors names cheat-sheet created by Dr Ying Wei.

<a href="http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
class="uri">http://www.stat.columbia.edu/\~tzheng/files/Rcolor.pdf</a>

# 4 Plot multiply imputed data from other packages

We can also plot multiply imputed datasets obtaining from other
packages, such as `mice`. Here is an example using the `nhanes2` data
from `mice`.

Note that this dataset is quite small, only has 25 rows and 4 columns
(`age`, `bmi`, `hyp` and `chl`). There are only 9, 8 and 10 missing
values in the variables `bmi`, `hyp` and `chl` respectively. Imputed
values are volatile when the dataset is small.

``` r
library(mice)
dim(nhanes2)
#> [1] 25  4
colSums(is.na(nhanes2))
#> age bmi hyp chl 
#>   0   9   8  10

imp <- mice(data = nhanes2, m = 5, printFlag = FALSE)
mice.data <- complete(imp, action = "all")
```

``` r
plot_hist(imputation.list = mice.data, var.name = "bmi", original.data = nhanes2)
```

<img src="man/figures/README-unnamed-chunk-25-1.png" width="95%" />

``` r
plot_box(imputation.list = mice.data, var.name = "chl", original.data = nhanes2)
```

<img src="man/figures/README-unnamed-chunk-25-2.png" width="95%" />

``` r
plot_bar(imputation.list = mice.data, var.name = "hyp", original.data = nhanes2)
```

<img src="man/figures/README-unnamed-chunk-25-3.png" width="95%" />

``` r
plot_2num(imputation.list = mice.data, var.x = "bmi", var.y = "chl",
    original.data = nhanes2)
```

<img src="man/figures/README-unnamed-chunk-25-4.png" width="95%" />

``` r
plot_1num1fac(imputation.list = mice.data, var.num = "chl", var.fac = "hyp",
    original.data = nhanes2)
```

<img src="man/figures/README-unnamed-chunk-25-5.png" width="95%" />

``` r
plot_2num1fac(imputation.list = mice.data, var.x = "chl", var.y = "bmi",
    con.fac = "age", original.data = nhanes2)
```

<img src="man/figures/README-unnamed-chunk-25-6.png" width="95%" />

``` r
plot_2num1fac(imputation.list = mice.data, var.x = "chl", var.y = "bmi",
    con.fac = "hyp", original.data = nhanes2)
```

<img src="man/figures/README-unnamed-chunk-25-7.png" width="95%" />

# 5 Plot against masked true values

In general, we wouldn’t know the true values of the missing data, so we
can only plot the imputed values versus the observed data. However, if
we happen to know the true value, we can compare the imputed values with
them.

Let’s generate a simple dataset `full.df` and create 30% missing values
in variable `norm1` and `norm2`. We then impute `MCAR.df` using
`mixgb()`.

``` r
N <- 1000
norm1 <- rnorm(n = N, mean = 1, sd = 1)
norm2 <- rnorm(n = N, mean = 1, sd = 1)
y <- norm1 + norm2 + norm1 * norm2 + rnorm(n = N, mean = 0, sd = 1)
full.df <- data.frame(y = y, norm1 = norm1, norm2 = norm2)
MCAR.df <- createNA(data = full.df, var.names = c("norm1", "norm2"),
    p = c(0.3, 0.3))

mixgb.data <- mixgb(data = MCAR.df, m = 5, nrounds = 10)
```

Now since we know the true data, we can specify it in the plotting
functions. It will generate an extra panel called `MaskedTrue`, which
shows values that are originally observed but intentionally made
missing.

``` r
plot_hist(imputation.list = mixgb.data, var.name = "norm1", original.data = MCAR.df,
    true.data = full.df)
```

<img src="man/figures/README-unnamed-chunk-27-1.png" width="95%" />

``` r
plot_box(imputation.list = mixgb.data, var.name = "norm2", original.data = MCAR.df,
    true.data = full.df)
```

<img src="man/figures/README-unnamed-chunk-27-2.png" width="95%" />

``` r
plot_2num(imputation.list = mixgb.data, var.x = "norm1", var.y = "y",
    original.data = MCAR.df, true.data = full.df)
```

<img src="man/figures/README-unnamed-chunk-27-3.png" width="95%" />
