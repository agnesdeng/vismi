# Overimpute main function

Overimp main function to call different imputation methods.

## Usage

``` r
overimp(
  data,
  m = 5,
  p = 0.2,
  test_ratio = 0,
  method = "mixgb",
  seed = NULL,
  ...
)
```

## Arguments

- data:

  A data frame with missing values.

- m:

  The number of imputation.

- p:

  The extra proportion of missing values.

- test_ratio:

  The proportion of test set. Default is 0, meaning no test set.

- method:

  Can be one of the following: "mixgb","mice", and more in the future.

- seed:

  Random seed.

- ...:

  Other arguments to be passed into the overimp function.

## Value

An `overimp` object containing imputed training, test data (if
applicable) and essential parameters required for plotting.

## Examples

``` r
obj <- overimp(data = nhanes3, m = 3, p = 0.2, test_ratio = 0.2, method = "mixgb")
```
