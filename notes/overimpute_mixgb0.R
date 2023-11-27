#' Overimputation using mixgb0
#' @description Create extra missing values and overimpute them
#' @param train.data A data.frame with missing values
#' @param test.data A data.frame with missing values
#' @param p The proportion of extra missing values
#' @param m The number of overimputed datasets. Default: 5
#' @param maxit The number of imputation iterations. Default: 1
#' @param ordinalAsInteger Whether to convert ordinal factors to integers. By default, \code{ordinalAsInteger = FALSE}. Setting \code{ordinalAsInteger = TRUE} may speed up the imputation process for large datasets.
#' @param bootstrap Whether to use bootstrapping for multiple imputation. By default, \code{bootstrap = TRUE}. Setting \code{bootstrap = FALSE} would underestimate imputation variability if sampling-related hyperparameters of XGBoost are set to 1 (default).
#' @param pmm.type The types of predictive mean matching (PMM). Possible values:
#' \itemize{
#'  \item \code{NULL}: Imputations without PMM;
#'  \item \code{0}: Imputations with PMM type 0;
#'  \item \code{1}: Imputations with PMM type 1;
#'  \item \code{2}: Imputations with PMM type 2;
#'  \item \code{"auto"} (Default): Imputations with PMM type 2 for numeric/integer variables; imputations without PMM for categorical variables.
#' }
#' @param pmm.k The number of donors for predictive mean matching. Default: 5
#' @param pmm.link The link for predictive mean matching binary variables
#' \itemize{
#'  \item \code{"prob"} (Default): use probabilities;
#'  \item \code{"logit"}: use logit values.
#' }
#' @param initial.num Initial imputation method for numeric type data:
#' \itemize{
#'  \item \code{"normal"} (Default);
#'  \item \code{"mean"};
#'  \item \code{"median"};
#'  \item \code{"mode"};
#'  \item \code{"sample"}.
#' }
#' @param initial.int Initial imputation method for integer type data:
#' \itemize{
#'  \item \code{"mode"} (Default);
#'  \item \code{"sample"}.
#' }
#' @param initial.fac Initial imputation method for factor type data:
#' \itemize{
#'  \item \code{"mode"} (Default);
#'  \item \code{"sample"}.
#' }
#' @param save.models Whether to save models for imputing new data later on. Default: \code{FALSE}
#' @param save.vars Response models for variables specified in \code{save.vars} will be saved for imputing new data. Can be a vector of names or indices. By default, \code{save.vars = NULL}, response models for variables with missing values will be saved. To save all models, please specify \code{save.vars = colnames(data)}.
#' @param verbose Verbose setting for mixgb. If \code{TRUE}, will print out the progress of imputation. Default: \code{FALSE}.
#' @param xgb.params A list of XGBoost parameters. For more details, please check \href{https://xgboost.readthedocs.io/en/stable/parameter.html}{XGBoost documentation on parameters}.
#' @param nrounds The maximum number of boosting iterations for XGBoost. Default: 100
#' @param early_stopping_rounds An integer value \code{k}. XGBoost training will stop if the validation performance hasn't improved for \code{k} rounds. Default: 10.
#' @param print_every_n Print XGBoost evaluation information at every nth iteration if \code{xgboost_verbose > 0}.
#' @param xgboost_verbose Verbose setting for XGBoost training: 0 (silent), 1 (print information) and 2 (print additional information). Default: 0
#' @param ... Extra arguments to pass to XGBoost
#' @importFrom mixgb mixgb impute_new
#' @return an overimpute object
#' @export
overimpute_mixgb <- function(train.data, test.data = NULL, p = 0.3,
                             m = 5, maxit = 1, ordinalAsInteger = FALSE, bootstrap = TRUE,
                             pmm.type = "auto", pmm.k = 5, pmm.link = "prob",
                             initial.num = "normal", initial.int = "mode", initial.fac = "mode",
                             save.models = TRUE, save.vars = colnames(train.data), verbose = F,
                             xgb.params = list(max_depth = 3, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 1, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1, tree_method = "auto", gpu_id = 0, predictor = "auto"),
                             nrounds = 100, early_stopping_rounds = 10, print_every_n = 10L, xgboost_verbose = 0, ...) {

   params <- list()

  Nrow <- nrow(train.data)
  Ncol <- ncol(train.data)
  addNA.df <- train.data
  Names <- colnames(train.data)
  Types <- sapply(train.data, class)


 check.types <- lapply(Types,function(x) x %in% c("numeric","integer","factor","ordered"))
  if (!all(unlist(check.types))) {
    stop("Variables need to be of type numeric, integer or factor.")
  }

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
    addNA.loc[sample(obs.idx, num.obs * p)] <- TRUE
    addNA.m <- matrix(addNA.loc, nrow = Nrow, ncol = Ncol)
    colnames(addNA.m) <- colnames(train.data)
    addNA.df[addNA.m] <- NA
  }


  train.obj <- mixgb(data = addNA.df, m = m, maxit = maxit, ordinalAsInteger = ordinalAsInteger, bootstrap = bootstrap,
                     pmm.type = pmm.type, pmm.k = pmm.k, pmm.link = pmm.link,
                     initial.num = initial.num, initial.int = initial.int, initial.fac = initial.fac,
                     save.models = save.models, save.vars = save.vars, verbose = verbose,
                     xgb.params = xgb.params,
                     nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, xgboost_verbose = xgboost_verbose, ...)

  imputed.traindata <- train.obj$imputed.data


  # save params
  params$addNA.m <- addNA.m
  params$m <- m
  params$Types <- Types
  params$Names <- Names

  ################ test data
  if(!is.null(test.data)){
    Nrow2 <- nrow(test.data)
    Ncol2 <- ncol(test.data)
    addNA.df2 <- test.data

    # store originally missing values location
    testNA.m <- is.na(test.data)

    # create additional missing values
    obs.idx2 <- which(!is.na(test.data))
    num.obs2 <- length(obs.idx2)
    Names2 <- colnames(test.data)
    Types2 <- sapply(test.data, class)

    # check whether test.data match with train.data
    if (!all(Names2 %in% Names)) {
      stop("Variables in test.data are not exactly the same as variables in train.data")
    }

    if (Ncol2 != Ncol) {
      stop("The number of variables in test.data is not the same as the number of variables in train.data")
    }

    if (!all(Names2 == Names)) {
      stop("Variables in test.data are not in the same order as variables in train.data")
    }

    if (!all(unlist(Types2) == unlist(Types))) {
      stop("Types of variables in test data are not the same as the types of variables in train.data")
    }

    if (length(num.obs2) == 0) {
      stop("All values in test.data are missing. Please check your dataset.")
    } else {
      total2 <- Nrow2 * Ncol2
      addNA.loc2 <- rep(FALSE, total2)
      addNA.loc2[sample(obs.idx2, num.obs2 * p)] <- TRUE
      addNA.m2 <- matrix(addNA.loc2, nrow = Nrow2, ncol = Ncol2)
      colnames(addNA.m2) <- colnames(test.data)
      addNA.df2[addNA.m2] <- NA
    }




    imputed.testdata <-  mixgb::impute_new(object = train.obj,newdata = addNA.df2)

    params$addNA.m2 <- addNA.m2

    return(list("imputed.traindata" = imputed.traindata, "imputed.testdata" = imputed.testdata, "params" = params))

  }else{

    return(list("imputed.traindata" = imputed.traindata, "params" = params))
  }


}
