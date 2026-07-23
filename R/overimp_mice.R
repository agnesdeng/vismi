overimp_mice <- function(data, p = 0.2, m = 5, test_ratio = 0, seed = NULL,
                         method = NULL, maxit = 5, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop(
      'Package "mice" is required for method = "mice". ',
      'Please install it with install.packages("mice").'
    )
  }

  if (!is.null(method) && method == "ranger" &&
      !requireNamespace("ranger", quietly = TRUE)) {
    stop(
      'Package "ranger" is required for method = "mice-ranger". ',
      'Please install it with install.packages("ranger").'
    )
  }

  if (test_ratio > 0) {
    warning(
      "mice does not support saving an imputation model to impute new data. ",
      "test_ratio has been forced to 0."
    )
    test_ratio <- 0
  }

  params <- .overimp_preprocess(
    data, p = p, test_ratio = test_ratio, seed = seed
  )

  trainNA_data <- as.data.frame(params$trainNA_data)

  train_mids <- mice::mice(
    trainNA_data, m = m, method = method, maxit = maxit,
    seed = if (is.null(seed)) NA else seed,
    printFlag = FALSE, ...
  )

  imputed_train <- lapply(
    mice::complete(train_mids, action = "all"),
    data.table::as.data.table
  )

  list(
    imputed_train = imputed_train,
    imputed_test  = NULL,
    params        = params
  )
}
