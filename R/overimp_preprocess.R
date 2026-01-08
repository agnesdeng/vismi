# Prepare data for overimputation
.overimp_preprocess <- function(data, p = 0.2, test_ratio = 0, seed = NULL) {
  if (!inherits(data, c("data.frame", "data.table", "tbl_df", "tbl"))) {
    stop("data must be a data.frame, tibble, or data.table")
  }
  # convert to data.table
  data <- as.data.table(data)


  if (!is.null(seed)) set.seed(seed)

  # prestore Names and Types of variables
  N <- nrow(data)
  # Names for all variables
  Names <- colnames(data)

  Types <- sapply(data, function(col) {
    if (inherits(col, "ordered")) {
      "factor"
    }else if (inherits(col, "logical")){
      "factor"
    }else{
      class(col)
    }
  })


  if (!all(Types %in% c("numeric", "integer", "factor"))) {
    stop("Variables need to be of type `numeric`, `integer`, `logical`, `factor` or `ordinal factor`.")
  }


  # check_types <- lapply(Types,function(x) x %in% c("numeric","integer","logical","factor","ordered"))
  # if (!all(unlist(check_types))) {
  # stop("Variables need to be of type `numeric`, `integer`, `logical`, `factor` or `ordinal factor`.")
  # }


  # Split to training test sets
  # train/test split
  if (test_ratio > 0) {
    train_idx <- sample(seq_len(N), size = floor((1 - test_ratio) * N))
    test_idx <- setdiff(seq_len(N), train_idx)
    train_data <- data[train_idx, ]
    test_data <- data[test_idx, ]
  } else {
    train_data <- data
    test_data <- NULL
  }

  # Add extra missing values - Masked true
  trainNA <- .add_extra_NA(train_data, p = p)
  testNA <- if (!is.null(test_data)) .add_extra_NA(test_data, p = p) else NULL

  list(
    train_data = train_data,
    test_data = test_data,
    # trainNA_data = trainNA_data,
    # trainNA_loc = trainNA_loc,
    # testNA_data = if(!is.null(testNA)) testNA_data else NULL,
    # testNA_loc = if(!is.null(testNA)) testNA_loc else NULL,
    trainNA_data = trainNA$addNA_data,
    trainNA_loc = trainNA$addNA_loc,
    testNA_data = if (!is.null(testNA)) testNA$addNA_data else NULL,
    testNA_loc = if (!is.null(testNA)) testNA$addNA_loc else NULL,
    Names = Names,
    Types = Types,
    p = p
  )
}


# add extra missing values
.add_extra_NA <- function(data, p = 0.2, seed = NULL) {
  Nrow <- nrow(data)
  Ncol <- ncol(data)
  addNA_data <- data

  addNA_loc <- matrix(FALSE, nrow = Nrow, ncol = Ncol)
  colnames(addNA_loc) <- colnames(data)

  if (!is.null(seed)) set.seed(seed)

  if (is.list(p)) {
    if (is.null(names(p))) {
      stop("If `p` is a list, it must be a named list with variable names and their values. For example, p = list('BMPHEAD'=0.2, 'HSSEX'=0.1).")
    }

    for (var in names(p)) {
      if (!var %in% colnames(data)) stop("Variable ", var, " not found in data. Please check your spelling.")
      obs_idx <- which(!is.na(data[[var]]))
      addNA_loc[sample(obs_idx, length(obs_idx) * p[[var]]), var] <- TRUE
    }
    # addNA_loc <-matrix(addNA_loc,nrow=Nrow, ncol=Ncol)
  } else {
    obs_idx <- which(!is.na(data))
    addNA_loc[sample(obs_idx, length(obs_idx) * p)] <- TRUE
    # addNA_loc <-matrix(addNA_loc,nrow=Nrow, ncol=Ncol)
  }


  addNA_data[addNA_loc] <- NA
  list(addNA_data = addNA_data, addNA_loc = addNA_loc)
}
