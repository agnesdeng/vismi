# Overimpute main function

overimp wrapper function to call different imputation methods

## Usage

``` r
overimp(
  data,
  p = 0.2,
  m = 5,
  test_ratio = 0,
  method = "mixgb",
  seed = NULL,
  ...
)
```

## Arguments

- data:

  a data frame with missing values

- p:

  the extra proportion of missing values

- m:

  the number of imputation

- test_ratio:

  the proportion of test set. Default is 0, meaning no test set.

- method:

  can be one of the following: "mixgb", "midae", "mivae", "mice","cart"
  or "ranger"

- seed:

  random seed

- ...:

  other arguments to be passed into the overimp function
