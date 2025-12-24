#' Preprocess input of `vismi.data.frame()` for plotting
#' @param data Original data frame with missing values
#' @param imp_list A list of imputed data frames
#' @param m A single positive integer specifying the number of imputed datasets to plot
#' @param imp_idx A vector of integers specifying the indices of imputed datasets to plot
#' @param vars A vector of variable names to include in the plot
#' @param integerAsFactor Logical, whether to treat integer variables as factors
preprocess <- function(data, imp_list, m, imp_idx, vars, integerAsFactor) {
  #.imp_long()
  #vars<- c(x,y,z)
  # Convert everything to data.table but keep original class for slicing
  dt <- as.data.table(data)

  # Find missing rows for any variable
  na_indices <- unique(unlist(lapply(vars, function(v) which(is.na(dt[[v]])))))
  N_mis <- length(na_indices)
  N_obs <- nrow(dt) - N_mis



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

  imp_list <- lapply(imp_list, function(imp_data) {
    dt <- as.data.table(imp_data)[na_indices, ..vars]
    dt[, row_index := na_indices]
    dt
  })

  # Combine, label imputation sets

  imp_dt <- rbindlist(imp_list)
  imp_dt[, Group := rep(imp_names, each = N_mis)]

  # Extract observed rows
  obs_indices <- setdiff(seq_len(nrow(dt)), na_indices)
  obs_dt <- dt[obs_indices, ..vars]
  obs_dt[, row_index := obs_indices]
  obs_dt[, Group := "Observed"]

  # Combine observed + imputed
  all_dt <- rbindlist(list(obs_dt, imp_dt), use.names = TRUE)
  all_dt[, Group := factor(Group, levels = c("Observed", imp_names))]


  color_pal <- .vismi_colors(N_imp = length(imp_names))
  names(color_pal)<-levels(all_dt$Group)


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


  list(all_dt = all_dt,
       vars = vars,
       na_indices = na_indices,
       obs_indices = obs_indices,
       color_pal = color_pal)
}




.validate_indices<-function(indices, indices_name, limit){
  if(any(indices!= floor(indices))){
    stop(paste(indices_name, "must be integers"))
  }

  if(!is.vector(indices)){
    stop(paste(indices_name, "must be a vector"))

  }

  if(any(indices <= 0)){
    stop(paste(indices_name, "must be positive"))
  }

  if(any(duplicated(indices))){
    warning(paste(indices_name, "contains duplicate values"))
  }

  if(any(indices > limit)){
    stop(paste(indices_name, "must not exceed", limit))
  }


}



.validate_m <-function(m, N_imp){
  if(length(m)!=1| m <=0 | m!=floor(m)){
    stop("m must be a single positive integer.")
  }

}
