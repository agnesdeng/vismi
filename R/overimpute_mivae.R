#' Overimputation using mivae
#' @return an overimpute object
#' @export
overimpute_mivae <- function(train.data, test.data = NULL, p = 0.2, seed = NULL,
                             m = 5, categorical.encoding = "onehot", device = "cpu",
                             epochs = 100, batch.size = 512,
                             subsample = 1,
                             early.stopping.epochs = 1,
                             vae.params = list(),
                             pmm.type = NULL, pmm.k = 5, pmm.link = "prob", pmm.save.vars = NULL,
                             loss.na.scale = FALSE,
                             verbose = TRUE, print.every.n = 1,
                             save.model = TRUE, path = NULL
                             #save.models = FALSE, save.vars = colnames(train.data), save.models.folder = NULL,
) {


  if (is.null(path)) {
    stop("Please specify a path to save the imputation model.")
  }


  params <- list()

  if(!is.null(test.data)){
    save.models<-TRUE
  }

  Nrow <- nrow(train.data)
  Ncol <- ncol(train.data)
  addNA.df <- train.data
  Names <- colnames(train.data)
  Types <- sapply(train.data, class)


  check.types <- lapply(Types,function(x) x %in% c("numeric","integer","logical","factor","ordered"))
  if (!all(unlist(check.types))) {
    stop("Variables need to be of type `numeric`, `integer`, `logical`, `factor` or `ordinal factor`.")
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
    if(!is.null(seed)){
      set.seed(seed)
    }
    addNA.loc[sample(obs.idx, num.obs * p)] <- TRUE
    addNA.m <- matrix(addNA.loc, nrow = Nrow, ncol = Ncol)
    colnames(addNA.m) <- colnames(train.data)
    addNA.df[addNA.m] <- NA
  }


  train.obj <- miae::mivae(data = addNA.df, m = m,
                           categorical.encoding = categorical.encoding, device = device,
                           epochs = epochs, batch.size = batch.size,
                           subsample = subsample,
                           early.stopping.epochs = early.stopping.epochs,
                           vae.params = vae.params,
                           pmm.type = pmm.type, pmm.k = pmm.k, pmm.link = pmm.link, pmm.save.vars = pmm.save.vars,
                           loss.na.scale = loss.na.scale,
                           verbose = verbose, print.every.n = print.every.n,
                           save.model = save.model, path = path)



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
      if(!is.null(seed)){
        set.seed(seed)
      }
      addNA.loc2[sample(obs.idx2, num.obs2 * p)] <- TRUE
      addNA.m2 <- matrix(addNA.loc2, nrow = Nrow2, ncol = Ncol2)
      colnames(addNA.m2) <- colnames(test.data)
      addNA.df2[addNA.m2] <- NA
    }



    imputed.testdata <-  miae::impute_new(object = train.obj,newdata = addNA.df2)

    params$addNA.m2 <- addNA.m2

    return(list("imputed.traindata" = imputed.traindata, "imputed.testdata" = imputed.testdata, "params" = params))

  }else{

    return(list("imputed.traindata" = imputed.traindata, "params" = params))
  }


}
