overimp_mixgb <- function(data, p = 0.2, m = 5, test_ratio = 0, seed = NULL,
                             maxit = 1, ordinalAsInteger = FALSE,
                             pmm.type = NULL, pmm.k = 5, pmm.link = "prob",
                             initial.num = "normal", initial.int = "mode", initial.fac = "mode",
                             save.models = FALSE, save.vars = colnames(data), save.models.folder = NULL,
                             verbose = FALSE,
                             xgb.params = list(),
                             nrounds = 100, early_stopping_rounds = NULL, print_every_n = 10L, xgboost_verbose = 0, ...) {

  # Use internal preprocessing
  params <- .overimp_preprocess(data, p = p, test_ratio = test_ratio, seed = seed)

  train_data = params$train_data
  test_data = params$test_data
  trainNA_data = params$trainNA_data
  trainNA_loc = params$trainNA_loc
  testNA_data = params$testNA_data
  testNA_loc = params$testNA_loc
  Names <- params$Names
  Types <- params$Types





  # Automatically save models if test data is provided
  if(!is.null(test_data)) save.models <- TRUE

  # Run mixgb
  train_obj <- mixgb(
    data = trainNA_data,
    m = m,
    maxit = maxit,
    ordinalAsInteger = ordinalAsInteger,
    pmm.type = pmm.type,
    pmm.k = pmm.k,
    pmm.link = pmm.link,
    initial.num = initial.num,
    initial.int = initial.int,
    initial.fac = initial.fac,
    save.models = save.models,
    save.vars = save.vars,
    save.models.folder = save.models.folder,
    verbose = verbose,
    xgb.params = xgb.params,
    nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    print_every_n = print_every_n,
    xgboost_verbose = xgboost_verbose,
    ...
  )

  # Prepare output train data
  imputed_train <- if(save.models) train_obj$imputed.data else train_obj


  # Handle test data if provided
  if(!is.null(test_data)) {
    imputed_test <- impute_new(object = train_obj, newdata = testNA_data)
  } else {
    imputed_test <- NULL
  }

  list(
    imputed_train = imputed_train,
    imputed_test = imputed_test,
    params = params
  )


}
