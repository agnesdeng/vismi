#' Overimputation using mice default (can only do with training data)
#' @description Create extra missing values and overimpute them
#' @param train.data A data.frame with missing values
#' @param p The proportion of extra missing values
#' @param m The number of overimputed datasets. Default: 5
#' @param maxit The number of imputation iterations. Default: 1
#' @param ... Extra arguments to pass to mice
#' @importFrom mice mice complete
#' @return an overimpute object
#' @export
overimpute_mice <- function(train.data, test.data = NULL, p = 0.2, seed = NULL,
                             m = 5, maxit = 5, printFlag = FALSE,...) {

  params <- list()

  Nrow <- nrow(train.data)
  Ncol <- ncol(train.data)
  addNA.df <- train.data
  Names <- colnames(train.data)
  Types <- sapply(train.data, class)




  # store originally missing values location
  trainNA.m <- is.na(train.data)

  # create additionally missing values
  obs.idx <- which(!is.na(train.data))
  num.obs <- length(obs.idx)

  if (length(num.obs) == 0) {
    stop("All values in train.data are missing. Please check your dataset.")
  } else {
    total <- Nrow * Ncol
    addNA.loc <- rep(FALSE, total)
    if(!is.null(seed)){
      set.seed(seed)
    }
    addNA.loc[sample(obs.idx, num.obs * p)] <- TRUE
    addNA.m <- matrix(addNA.loc, nrow = Nrow, ncol = Ncol)
    colnames(addNA.m) <- colnames(train.data)
    addNA.df[addNA.m] <- NA
  }


  train.imp <- mice::mice(data = addNA.df, m = m, maxit = maxit, printFlag = printFlag, ...)

  imputed.traindata <- mice::complete(train.imp, action = "all")


  # save params
  params$addNA.m <- addNA.m
  params$m <- m
  params$Types <- Types
  params$Names <- Names



  return(list("imputed.traindata" = imputed.traindata, "params" = params, "loggedEvents" = train.imp$loggedEvents))


}
