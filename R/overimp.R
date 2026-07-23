#' Overimpute main function
#' @description Overimp main function to call different imputation methods.
#' @param data A data frame with missing values.
#' @param m The number of imputation.
#' @param p The extra proportion of missing values.
#' @param test_ratio The proportion of test set. Default is 0, meaning no test set.
#' @param method The imputation method to use. One of \code{"mixgb"} (default),
#'   \code{"mice"} (mice with default per-variable methods), \code{"mice-cart"}
#'   (mice with classification and regression trees), or \code{"mice-ranger"}
#'   (mice with random forests via the ranger engine).
#' @param seed Random seed.
#' @param ... Other arguments to be passed into the overimp function.
#' @return An \code{overimp} object containing imputed training, test data (if applicable) and essential parameters required for plotting.
#' @export
#' @examples
#' obj <- overimp(data = nhanes3, m = 3, p = 0.2, test_ratio = 0.2, method = "mixgb")
overimp <- function(data, m = 5, p = 0.2, test_ratio = 0, method = "mixgb", seed = NULL, ...) {
  if (method == "mixgb") {
    overimp_fun <- "overimp_mixgb"
    extra_args  <- list()
  } else if (method == "mice") {
    overimp_fun <- "overimp_mice"
    extra_args  <- list()
  } else if (method == "mice-cart") {
    overimp_fun <- "overimp_mice"
    extra_args  <- list(method = "cart")
  } else if (method == "mice-ranger") {
    overimp_fun <- "overimp_mice"
    extra_args  <- list(method = "rf")
  } else {
    stop('"method" must be one of "mixgb", "mice", "mice-cart", or "mice-ranger".')
  }

  obj <- do.call(overimp_fun,
    args = c(
      list(data = data, p = p, m = m, test_ratio = test_ratio, seed = seed),
      extra_args,
      list(...)
    )
  )
  obj$method <- method
  class(obj) <- "overimp"
  obj
}
