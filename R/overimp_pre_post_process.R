
# Prepare plot data (dt and colors) for overimputation plots
.overimp_postprocess<- function(obj, vars, m , imp_idx, integerAsFactor) {

  .get_plot_data <- function(imp_list, NA_loc, dt, vars, m, imp_idx, integerAsFactor) {

    #vars=c("BMPHEAD","BMPRECUM")

    na_loc <- is.na(dt)
    #original missing value indices
    na_indices <- which(rowSums(na_loc[,vars, drop=FALSE])>0)
    #masked true indices - not the original missing value indices, but the additionally masked ones
    mt_indices <-which(rowSums(NA_loc[,vars, drop=FALSE])>0)
    #only keep those in mt_indices but not in na_indices
    mt_indices <- setdiff(mt_indices, na_indices)


    #subset for plotting
    N_imp <- length(imp_list)
    if(!is.null(imp_idx)){
      # priority subset by imp_idx
      .validate_indices(indices=imp_idx, indices_name = "imp_idx", limit = N_imp)
      imp_list <- imp_list[imp_idx]


    }else if(!is.null(m)){
      # then subset by m
      .validate_m(m=m, N_imp=N_imp)

      if(m < N_imp){
        imp_idx <-sort(sample.int(N_imp,m))
        message(paste0(m, " imputed datasets are randomly selected for plotting.Their indices are: ", paste(imp_idx, collapse = ", ")))
        imp_list <- imp_list[imp_idx]
      }else if(m > N_imp){
        imp_idx <- seq_len(N_imp)
        warning(paste0("m is larger than the available number of imputed datasets. Using all ", N_imp, " imputed datasets for plotting."))
      }else{
        imp_idx <- seq_len(N_imp)
      }

    }else{
      # use all
      imp_idx <- seq_len(N_imp)
    }


    # Extract imputed rows for each imputation
    imp_names <- paste0("Imputed set", imp_idx)


    # Multiply-imputed masked true
    imp_dt <- rbindlist(lapply(seq_along(imp_names), function(i) {
      dt <- imp_list[[i]][mt_indices, ..vars]
      dt[, `:=`(row_index = mt_indices, Group = imp_names[i])]
      dt
    }))

    # Extract Masked true
    mt_dt <- dt[mt_indices, ..vars]
    mt_dt[, row_index := mt_indices]
    mt_dt[, Group := "Masked true"]

    # Combine
    all_dt <- rbindlist(list(mt_dt, imp_dt), use.names = TRUE)
    all_dt[, Group := factor(Group, levels = c("Masked true", imp_names))]

    # integerAsFactor
    int <- sapply(all_dt[, ..vars], is.integer)

    if (integerAsFactor) {
      for (v in names(int)[int]) {
        all_dt[, (v) := as.factor(get(v))]
      }
    } else {
      for (v in names(int)[int]) {
        all_dt[, (v) := as.numeric(get(v))]
      }
    }
    # colors
    color_pal <- .vismi_colors(N_imp = length(imp_names))
    names(color_pal)<-levels(all_dt$Group)
    list(all_dt = all_dt, color_pal = color_pal, imp_idx = imp_idx)
  }



  train_out<- .get_plot_data(imp_list= obj$imputed_train, NA_loc = obj$params$trainNA_loc, dt = obj$params$train_data, vars = vars, m = m, imp_idx = imp_idx, integerAsFactor = integerAsFactor)

  test_out <- if(!is.null(obj$params$test_data)){
    .get_plot_data(imp_list=obj$imputed_test,  NA_loc =obj$params$testNA_loc, dt = obj$params$test_data,vars = vars,  m = m, imp_idx = train_out$imp_idx, integerAsFactor = integerAsFactor)
  }else{
    NULL
  }

  list(train = train_out, test = test_out)
}



#Prepare data for overimputation
.overimp_preprocess<- function(data, p = 0.2, test_ratio = 0, seed = NULL) {

  if (!inherits(data, c("data.frame", "data.table", "tbl_df", "tbl"))) {
    stop("data must be a data.frame, tibble, or data.table")
  }
  # convert to data.table
  data<-as.data.table(data)


  if(!is.null(seed)) set.seed(seed)

  #prestore Names and Types of variables
  N <- nrow(data)
  #Names for all variables
  Names <- colnames(data)
  #Types for all variables
  Types <- sapply(data, class)
  Types <- unlist(Types)
  #if is ordered factor, the names will change from HYD1 to HYD11, to avoid this, need to rename it
  names(Types)<-Names
  if(!all(Types %in% c("numeric","integer","logical","factor","ordered"))){
    stop("Variables need to be of type `numeric`, `integer`, `logical`, `factor` or `ordinal factor`.")
  }
  #Types conversion: ordered to factor, logical to factor
  Types[Types=="ordered"] <- "factor"
  Types[Types=="logical"] <- "factor"

  #check_types <- lapply(Types,function(x) x %in% c("numeric","integer","logical","factor","ordered"))
  #if (!all(unlist(check_types))) {
   # stop("Variables need to be of type `numeric`, `integer`, `logical`, `factor` or `ordinal factor`.")
 # }


  #Split to training test sets
  # train/test split
  if(test_ratio > 0) {
    train_idx <- sample(seq_len(N), size = floor((1 - test_ratio) * N))
    test_idx <- setdiff(seq_len(N), train_idx)
    train_data <- data[train_idx, ]
    test_data <- data[test_idx, ]
  } else {
    train_data <- data
    test_data <- NULL
  }

  #Add extra missing values - Masked true
  trainNA <- .add_extra_NA(train_data, p = p)
  testNA <- if(!is.null(test_data)) .add_extra_NA(test_data, p = p) else NULL

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



#add extra missing values
.add_extra_NA <- function(data, p = 0.2, seed = NULL) {
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



