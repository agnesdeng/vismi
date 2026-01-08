
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
