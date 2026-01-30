#' Overimpute main function
#' @description Overimp main function to call different imputation methods.
#' @param data A data frame with missing values.
#' @param m The number of imputation.
#' @param p The extra proportion of missing values.
#' @param test_ratio The proportion of test set. Default is 0, meaning no test set.
#' @param method Can be one of the following: "mixgb","mice", and more in the future.
#' @param seed Random seed.
#' @param ... Other arguments to be passed into the overimp function.
#' @return An \code{overimp} object containing imputed training, test data (if applicable) and essential parameters required for plotting.
#' @export
#' @examples
#' obj <- overimp(data = nhanes3, m = 3, p = 0.2, test_ratio = 0.2, method = "mixgb")
overimp <- function(data, m = 5, p = 0.2, test_ratio = 0, method = "mixgb", seed = NULL, ...) {
  overimp_fun <- paste0("overimp_", method)

  obj <- do.call(overimp_fun,
    args = list(
      data = data,
      p = p,
      m = m,
      test_ratio = test_ratio,
      seed = seed,
      ...
    )
  )
  class(obj) <- "overimp"
  obj
}
