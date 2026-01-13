# Preprocess input of `vismi.data.frame()` for plotting
preprocess <- function(data, imp_list, m, imp_idx, vars, integerAsFactor, verbose) {
  # .imp_long()
  # vars<- c(x,y,z)
  # Convert everything to data.table but keep original class for slicing
  dt <- as.data.table(data)

  # Find missing indices for each variable
  na_xyz <- setNames(lapply(vars, function(v) which(is.na(dt[[v]]))), vars)
  ####


  ###
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

  if (verbose) {
    cli::cli_h1("Missing data summary")

    cli::cli_inform(
      lapply(vars, function(v) {
        cli::format_inline(
          "Variable {.var {v}} has {length(na_xyz[[v]])} missing values."
        )
      })
    )


    cli::cli_h1("Breakdown of missing data patterns")
    print(na_pattern)

    cli::cli_h1("Imputed data visualisation")
  }


  na_indices <- unique(unlist(na_xyz))

  if (length(na_indices) == 0) {
    if (verbose) {
      cli::cli_inform(
        "No missing values were found in the specified {cli::qty(vars)}variable{?s} ({.var {cli::cli_vec(vars)}}). Only the observed data are shown in the plot."
      )
    }

    no_missing <- TRUE
  } else {
    if (verbose) {
      if (length(vars == 1)) {
        cli::cli_inform(
          "For each imputed set, a total of {length(na_indices)} observations with missingness in the specified variable {.var {vars}} are shown."
        )
      } else {
        cli::cli_inform(
          "For each imputed set, a total of {length(na_indices)} observations with at least one missing entry across the specified {cli::qty(vars)}variable{?s} ({.var {cli::cli_vec(vars)}}) are shown."
        )
      }
    }

    no_missing <- FALSE
  }

  N_mis <- length(na_indices)
  N_obs <- nrow(dt) - N_mis


  # subset for plotting
  N_imp <- length(imp_list)
  if (!is.null(imp_idx)) {
    # priority subset by imp_idx
    .validate_indices(indices = imp_idx, indices_name = "imp_idx", limit = N_imp)
    imp_list <- imp_list[imp_idx]
  } else if (!is.null(m)) {
    # then subset by m
    .validate_m(m = m, N_imp = N_imp)

    if (m < N_imp) {
      imp_idx <- sort(sample.int(N_imp, m))
      message(paste0(m, " imputed datasets are randomly selected for plotting.Their indices are: ", paste(imp_idx, collapse = ", ")))
      imp_list <- imp_list[imp_idx]
    } else if (m > N_imp) {
      imp_idx <- seq_len(N_imp)
      warning(paste0("m is larger than the available number of imputed datasets. Using all ", N_imp, " imputed datasets for plotting."))
    } else {
      imp_idx <- seq_len(N_imp)
    }
  } else {
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
    na_indices = na_indices,
    obs_indices = obs_indices,
    color_pal = color_pal,
    no_missing = no_missing
  )
}


.validate_indices <- function(indices, indices_name, limit) {
  if (any(indices != floor(indices))) {
    stop(paste(indices_name, "must be integers"))
  }

  if (!is.vector(indices)) {
    stop(paste(indices_name, "must be a vector"))
  }

  if (any(indices <= 0)) {
    stop(paste(indices_name, "must be positive"))
  }

  if (any(duplicated(indices))) {
    warning(paste(indices_name, "contains duplicate values"))
  }

  if (any(indices > limit)) {
    stop(paste(indices_name, "must not exceed", limit))
  }
}
