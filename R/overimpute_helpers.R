# reusable function to add extra missing values
addNA <- function(data, p = 0.2, seed = NULL) {
  Nrow <- nrow(data)
  Ncol <- ncol(data)
  addNA_data <- data
  obs_idx <- which(!is.na(data))
  if(length(obs_idx) == 0) stop("All values are missing in data.")
  if(!is.null(seed)) set.seed(seed)

  addNA_loc <- matrix(FALSE, nrow = Nrow, ncol = Ncol)
  addNA_loc[sample(obs_idx, length(obs_idx) * p)] <- TRUE
  addNA_loc <-matrix(addNA_loc,nrow=Nrow, ncol=Ncol)
  colnames(addNA_loc) <- colnames(data)
  addNA_data[addNA_loc] <- NA
  list(addNA_data = addNA_data, addNA_loc = addNA_loc)
}

# Internal preprocessing function
.overimpute_preprocess <- function(data, p = 0.2, test_ratio = 0, seed = NULL) {
  stopifnot(is.data.frame(data))

  if(!is.null(seed)) set.seed(seed)

  N <- nrow(data)
  Names <- colnames(data)
  Types <- sapply(data, class)

  check.types <- lapply(Types,function(x) x %in% c("numeric","integer","logical","factor","ordered"))
  if (!all(unlist(check.types))) {
    stop("Variables need to be of type `numeric`, `integer`, `logical`, `factor` or `ordinal factor`.")
  }



  # train/test split
  if(test_ratio > 0) {
    train_idx <- sample(seq_len(N), size = floor((1 - test_ratio) * N))
    test_idx <- setdiff(seq_len(N), train_idx)
    train_data <- data[train_idx, , drop = FALSE]
    test_data <- data[test_idx, , drop = FALSE]
  } else {
    train_data <- data
    test_data <- NULL
  }

  # add missing values
  trainNA <- addNA(train_data, p = p)
  testNA <- if(!is.null(test_data)) addNA(test_data, p = p) else NULL

  list(
    train_data = train_data,
    test_data = test_data,
    #trainNA_data = trainNA_data,
    #trainNA_loc = trainNA_loc,
   # testNA_data = if(!is.null(testNA)) testNA_data else NULL,
    #testNA_loc = if(!is.null(testNA)) testNA_loc else NULL,
    trainNA_data = trainNA$addNA_data,
    trainNA_loc = trainNA$addNA_loc,
    testNA_data = if(!is.null(testNA)) testNA$addNA_data else NULL,
    testNA_loc = if(!is.null(testNA)) testNA$addNA_loc else NULL,
    Names = Names,
    Types = Types
  )
}


# Helper: convert imputed object to long format for plotting
.prepare_imputed_long <- function(obj, x) {

  #m <- obj$params$m
  Types <- obj$params$Types
  Names <- obj$params$Names


  # internal function to process one dataset
  .make_long <- function(imputed_list, NA_loc, data, int_colors = c("#002cb3","#85aeff")) {
    N_imp <- length(imputed_list)
    L <- lapply(imputed_list, function(dt) dt[[x]][NA_loc[, x]])
    Nrow <- length(L[[1]])
    Mat <- matrix(unlist(L), nrow = Nrow, ncol = length(imputed_list))
    True <- data[, x, drop=TRUE][NA_loc[, x]]
    combine_df <- cbind.data.frame(Mat, True)
    colnames(combine_df) <- c(paste0("Imputed set", seq_len(N_imp)), "Masked true")
    long_df <- tidyr::pivot_longer(combine_df, cols = everything(), names_to = "Group", values_to = x)
    long_df$Group <- factor(long_df$Group, levels = c(paste0("Imputed set", rev(seq_len(N_imp))), "Masked true"))
    colfunc <- grDevices::colorRampPalette(int_colors)
    colors <-c(colfunc(N_imp), "gray40")
    list(long_df = long_df, colors = colors)
  }



  train_long <- .make_long(obj$imputed_train, obj$params$trainNA_loc, obj$params$train_data, int_colors = c("#002cb3","#85aeff"))
  test_long <- if(!is.null(obj$params$test_data)){
    .make_long(obj$imputed_test, obj$params$testNA_loc, obj$params$test_data,int_colors = c("#cc7700","#ffd24d"))
  }else{
    NULL
  }

  list(train = train_long, test = test_long)
}

