#' Preprocess data for plotting
#' @param data Original data frame with missing values
#' @param imp_list A list of imputed data frames
#' @param vars A vector of variable names to include in the plot
#' @param integerAsFactor Logical, whether to treat integer variables as factors
preprocess <- function(data, imp_list, vars, integerAsFactor) {
  #vars<- c(x,y,z)
  # Convert everything to data.table but keep original class for slicing
  dt <- as.data.table(data)

  # Find missing rows for any variable
  na_indices <- unique(unlist(lapply(vars, function(v) which(is.na(dt[[v]])))))
  N_mis <- length(na_indices)
  N_obs <- nrow(dt) - N_mis

  # Extract imputed rows for each imputation
  imp_list <- lapply(imp_list, function(imp_data) {
    dt <- as.data.table(imp_data)[na_indices, ..vars]
    dt[, row_index := na_indices]
    dt
  })

  # Combine, label imputation sets
  N_imp <- length(imp_list)
  imp_names <- paste0("Imputed set ", seq_len(N_imp))
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

  color_pal <- c("#666666", hue_pal()(N_imp))
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
