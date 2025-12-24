#' Overimpute main function
#' @description overimp wrapper function to call different imputation methods
#' @param data a data frame with missing values
#' @param p the extra proportion of missing values
#' @param m the number of imputation
#' @param test_ratio the proportion of test set. Default is 0, meaning no test set.
#' @param method can be one of the following: "mixgb", "midae", "mivae", "mice","cart" or "ranger"
#' @param seed random seed
#' @param ... other arguments to be passed into the overimp function
#' @export
overimp<-function(data, p = 0.2, m = 5, test_ratio = 0, method = "mixgb", seed = NULL, ...){

  overimp_fun<-paste0("overimp_",method)

  obj <- do.call(overimp_fun,
          args=list(data = data,
                    p = p,
                    m = m,
                    test_ratio = test_ratio,
                    seed = seed,
                    ...))
  class(obj) <- "overimp"
  obj
}
