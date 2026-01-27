# Preprocess input of `vismi.data.frame()` for plotting
preprocess <- function(data, imp_list, imp_idx, vars, integerAsFactor) {
  # vars<- c(x,y,z)
  # Convert everything to data.table but keep original class for slicing
  dt <- as.data.table(data)

  # Find missing indices for each variable
  na_xyz <- setNames(lapply(vars, function(v) which(is.na(dt[[v]]))), vars)

  na_comb <- list()
  comb_list <- unlist(
    lapply(1:length(vars), function(k) combn(vars, k, simplify = FALSE)),
    recursive = FALSE
  )

  for (comb in comb_list) {
    # Rows missing all variables in comb
    rows <- Reduce(intersect, na_xyz[comb])
    # Exclude rows that have extra missing values in other variables
    other_vars <- setdiff(vars, comb)
    if (length(other_vars) > 0) {
      for (v in other_vars) {
        rows <- setdiff(rows, na_xyz[[v]])
      }
    }
    na_comb[[paste(comb, collapse = ", ")]] <- rows
  }
  counts <- vapply(na_comb, function(x) length(x), FUN.VALUE = integer(1))

  # Convert to data.frame
  na_pattern <- data.frame(
    Variable = names(counts),
    Count = as.integer(counts),
    row.names = NULL
  )

  custom_order <- sapply(comb_list, function(x) paste(x, collapse = ", "))

  # Turn Variable into a factor with this order
  na_pattern$Variable <- factor(na_pattern$Variable, levels = custom_order)

  # Sort by factor levels
  na_pattern <- na_pattern[order(na_pattern$Variable), ]

  na_indices <- unique(unlist(na_xyz))

  no_missing <- ifelse(length(na_indices) == 0, TRUE, FALSE)

  N_mis <- length(na_indices)
  N_obs <- nrow(dt) - N_mis


  # subset for plotting
  N_imp <- length(imp_list)

  imp_list <- imp_list[imp_idx]

  # Extract imputed rows for each imputation
  imp_names <- paste("Imp set", imp_idx)
  #imp_names <- paste0("Imputed set", imp_idx)

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
  names(color_pal) <- levels(all_dt$Group)


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


  list(
    all_dt = all_dt,
    vars = vars,
    na_xyz = na_xyz,
    na_pattern = na_pattern,
    na_indices = na_indices,
    obs_indices = obs_indices,
    color_pal = color_pal,
    no_missing = no_missing
  )
}


