.data_summary<-function(pre, plot_idx_msg){

  vars <- pre$vars
  na_xyz <- pre$na_xyz
  na_pattern <- pre$na_pattern
  na_indices <- pre$na_indices


  cli::cli_h1("Missing data summary")

  cli::cli_inform(
    lapply(vars, function(v) {
      n_mis<-length(na_xyz[[v]])
      cli::format_inline(
        "Variable {.var {v}} has {n_mis} missing values."
      )
    })
  )


  cli::cli_h1("Breakdown of missing data patterns")
  print(na_pattern)

  cli::cli_h1("Imputed data used for plotting")

  if (length(na_indices) == 0) {
      cli::cli_inform(
        "No missing values were found in the specified {cli::qty(vars)}variable{?s} ({.var {cli::cli_vec(vars)}}). Only the observed data are shown in the plot."
      )
  } else {
    # only one variable
      if (length(vars == 1)) {
        cli::cli_inform(
          "For each imputed set, a total of {length(na_indices)} observations with missingness in the specified variable {.var {vars}} are shown."
        )
      } else {
    # 2 or 3 variables
        cli::cli_inform(
          "For each imputed set, a total of {length(na_indices)} observations with at least one missing entry across the specified {cli::qty(vars)}variable{?s} ({.var {cli::cli_vec(vars)}}) are shown."
        )
      }

  }

 #info for plotting subset panels
  cli::cli_h1("Imputed sets selected for plotting")
  cli::cli_inform(plot_idx_msg)

}



.validate_m_imp_idx<-function(imp_list, m, imp_idx) {
  # subset for plotting
  N_imp <- length(imp_list)

  if(is.null(m) & is.null(imp_idx)) {
    # plot all imputed set
    imp_idx_final <- seq_len(N_imp)
    plot_idx_msg <-cli::format_inline("Both 'm' and 'imp_idx' are NULL. Using all {N_imp} imputed datasets for plotting.")
  }

  if(is.null(m) & !is.null(imp_idx)){
    # only imp_idx is provided
    .validate_indices(indices = imp_idx, indices_name = "imp_idx", limit = N_imp)
    imp_idx_final <- imp_idx
    plot_idx_msg <- cli::format_inline("Using the provided indices in 'imp_idx' for plotting: {cli::cli_vec(imp_idx)}")
  }

  if(!is.null(m) & !is.null(imp_idx)){
    # both m and imp_idx are provided
    if(length(imp_idx) != m){
      stop(paste("The length of 'imp_idx' does not equal 'm'. Please check your inputs."))
    }

    .validate_indices(indices = imp_idx, indices_name = "imp_idx", limit = N_imp)
    imp_idx_final <- imp_idx
    plot_idx_msg <- cli::format_inline("Using the provided indices in 'imp_idx' for plotting: {cli::cli_vec(imp_idx)}")
  }

  if(!is.null(m) & is.null(imp_idx)){
    # only m is provided
    .validate_m(m = m, N_imp = N_imp)

    if(m < N_imp) {
      imp_idx_final <- sort(sample.int(N_imp, m))
      plot_idx_msg <-cli::format_inline("{m} imputed datasets are randomly selected for plotting. Their indices are: {cli::cli_vec(imp_idx_final)}")
    }

    if(m == N_imp){
      imp_idx_final <- seq_len(N_imp)
      plot_idx_msg <-cli::format_inline("{m} is the same as the available number of imputed datasets. Using all {N_imp} imputed datasets for plotting.")
    }
  }



  out <- list(
    imp_idx = imp_idx_final,
    plot_idx_msg = plot_idx_msg
  )
  out

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
    stop(paste(indices_name, "contains duplicate values"))
  }

  if (any(indices > limit)) {
    stop(paste(indices_name, "must not exceed", limit))
  }
}


.validate_m <- function(m, N_imp) {
  if (length(m) != 1 | m <= 0 | m != floor(m)) {
    stop("m must be a single positive integer.")
  }
  if (m > N_imp){
    stop(paste0("m cannot be larger than the total number of imputed datasets (", N_imp, ")."))
  }
}


.validate_incomplete_variable<-function(obj, x){

  if(inherits(obj,"mixgb")){
    missing_vars <- obj$params$missing.vars
  }else if(inherits(obj,"mids")){
    missing_vars <- names(obj$nmis)[obj$nmis>0]
  }
  if(!(x %in% missing_vars)){
    stop(paste0("Variable '", x, "' not found in the imputed object. Please check your spelling. Valid incomplete variables are", paste(missing_vars, collapse = ", "), "."))
  }
}



.validate_data <- function(data,  integerAsFactor, max_levels, verbose) {


  if (isTRUE(verbose)) {
    cli::cli_h1("Sanity checks")
  }

  data <- data |>
    check_data_format() |>
    check_NaN(verbose) |>
    check_inf(verbose) |>
    check_empty(verbose) |>
    check_single_level(verbose) |>
    check_ID(verbose, max_levels)|>
    resolve_column_types(integerAsFactor, verbose)


  data

  #list(data = data, Types = Types)
}


# check data format -------------------------------------------------------
check_data_format <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame, tibble or data.table")
  }

  # Ensure data.table is handled safely
  if (inherits(data, "data.table")) {
    data <- data.table::copy(data)
  }
  data
}





# check NaN ---------------------------------------------------------------
check_NaN <- function(data, verbose) {
  nan_num <- do.call(cbind, lapply(data, is.nan))
  if (any(nan_num)) {
    col_idx <- unique(which(nan_num, arr.ind = TRUE)[, 2])
    nan_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Numeric {.emph NaN} values detected in {.var {nan_col}}.",
        " They have been converted to {.emph NA}."
      ))
    }
    data[nan_num] <- NA
  }

  nan_chr <- data == "NaN"
  nan_chr[is.na(nan_chr)] <- FALSE

  if (any(nan_chr)) {
    col_idx <- unique(which(nan_chr, arr.ind = TRUE)[, 2])
    nan_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Character {.emph \"NaN\"} values detected in {.var {nan_col}}.",
        " They have been converted to {.emph NA}."
      ))
    }
    data[nan_chr] <- NA
    # drop "NaN" level
    data <- droplevels(data)
  }
  data
}


# check Inf and -Inf ------------------------------------------------------
check_inf <- function(data, verbose) {
  inf_num <- do.call(cbind, lapply(data, is.infinite))

  if (any(inf_num)) {
    col_idx <- unique(which(inf_num, arr.ind = TRUE)[, 2])
    inf_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Numeric {.emph Inf/-Inf} values detected in {.var {inf_col}}.",
        "They have been converted to {.emph NA}."
      ))
    }
    data[inf_num] <- NA
  }

  inf_chr <- (data == "Inf" | data == "-Inf")
  inf_chr[is.na(inf_chr)] <- FALSE

  if (any(inf_chr)) {
    col_idx <- unique(which(inf_chr, arr.ind = TRUE)[, 2])
    inf_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Character / factor {.emph \"Inf\" / \"-Inf\"} values detected in {.var {inf_col}}",
        "They have been converted to {.emph NA}."
      ))
    }
    data[inf_chr] <- NA
    # drop "Inf"/"-Inf" level
    data <- droplevels(data)
  }
  data
}


# check empty cells "" ----------------------------------------------------
check_empty <- function(data, verbose) {
  empty_chr <- data == ""
  empty_chr[is.na(empty_chr)] <- FALSE

  if (any(empty_chr)) {
    col_idx <- unique(which(empty_chr, arr.ind = TRUE)[, 2])
    empty_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Empty string {.emph \"\"} values detected in {.var {empty_col}}.",
        " They have been converted to {.emph NA}."
      ))
    }
    # replace empty strings with NA
    data[empty_chr] <- NA

    # drop empty string level from factors
    data <- droplevels(data)
  }
  data
}


# check single level factor -----------------------------------------------
check_single_level <- function(data, verbose) {
  # nlevels only work for factor
  # idx <- which(sapply(data, nlevels) == 1)
  n_levels <- vapply(data, function(x) length(unique(x)), numeric(1))
  idx <- which(n_levels == 1)

  if (length(idx) > 0) {
    single_cols <- colnames(data)[idx]

    msg <- "Variable(s) with only one level detected"
    question <- "Do you want to remove them from the data and proceed?"
    proceed <- .user_action(msg = msg, question = question, cols = single_cols)

    if (proceed) {
      if (isTRUE(verbose)) {
        cli::cli_alert_warning("Removing variables with only one level: {.var {single_cols}}")
      }

      if (inherits(data, "data.table")) {
        data[, (single_cols) := NULL]
      } else {
        data <- data[, -idx, drop = FALSE]
      }
    } else {
      cli::cli_abort("User chose not to proceed. Please inspect variables with only one level: {.var {single_cols}}")
    }
  }
  data
}

# check ID-like columns: factors with too many levels -------------------------------------
check_ID <- function(data, verbose, max_levels = 0.5 * nrow(data)) {
  num_levels <- sapply(data, nlevels)
  num_row <- nrow(data)

  idx <- which(num_levels > max_levels)
  if (length(idx) > 0) {
    ID_cols <- colnames(data)[idx]

    msg <- paste("Factor variable(s) with more than", max_levels, "levels detected")
    question <- "Do you want to remove them from the data and proceed?"
    proceed <- .user_action(msg = msg, question = question, cols = ID_cols)

    if (proceed) {
      if (isTRUE(verbose)) {
        cli::cli_alert_warning("Removing ID-like variables with too many levels: {.var {ID_cols}}")
      }
    } else {
      cli::cli_abort("User chose not to proceed. Please inspect ID-like variables with too many levels: {.var {ID_cols}}")
    }
  }
  data
}

# helper: ask for user input
.user_action <- function(msg, question = NULL, cols) {
  if (!interactive()) {
    return(FALSE)
  }
  cat("\n", msg, "in: ", paste(cli::col_blue(cols), collapse = ", "), "\n")

  if (is.null(question)) {
    question <- "Do you want to proceed with the fix?"
  }

  choice <- utils::menu(
    choices = c("Yes, please.", "Stop, I'd like to take a closer look at these flagged columns first."),
    title = paste("\n>", question)
  )

  # Return TRUE if they picked option 1, FALSE otherwise
  return(choice == 1)
}



# resolve column types ----------------------------------------------------
resolve_column_types <- function(data, integerAsFactor, verbose) {

  # Map and transform columns
  col_names<- colnames(data)
  col_types <- vapply(colnames(data), function(name) {
    col <- data[[name]]

    # 1. Ordered Factors & Logicals -> Factor
    if (inherits(col, "ordered") || inherits(col, "logical")) {
      if (isTRUE(verbose)) {
        type_lbl <- if(inherits(col, "ordered")) "ordered factor" else "logical variable"
        cli::cli_alert_info("{.var {name}}: {type_lbl} detected; treated as factor for plotting.")
      }
      return("factor")
    }

    # 2. Integers -> Factor or Numeric
    else if (inherits(col, "integer")) {
      if (isTRUE(verbose)) {
        msg <- if(integerAsFactor) "treated as factor" else "treated as numeric"
        cli::cli_alert_info("{.var {name}}: integer variable detected; {msg} for plotting.")
      }
      if (isTRUE(integerAsFactor)) {
        data[[name]] <<- as.factor(col) # Use super-assignment to modify data in parent scope
        return("factor")
      }
      return("numeric")
    }

    # 3. Characters -> Factor
    else if (inherits(col, "character")) {
      if (isTRUE(verbose)) {
        cli::cli_alert_warning("{.var {name}}: character variable detected; treated as factor for plotting.")
      }
      data[[name]] <<- as.factor(col)
      return("factor")
    }

    # 4. Fallback
    else {
      return(class(col)[1])
    }
  }, FUN.VALUE = character(1))

  invalid_idx <- !col_types %in% c("numeric", "factor")

  if (any(invalid_idx)) {
    # Identify which columns failed
    bad_cols <- col_names[invalid_idx]
    bad_types <- col_types[invalid_idx]

    cli::cli_abort(c(
      "x" = "Unsupported data types detected in the input data.",
      "i" = "Only {.val numeric}, {.val factor}, {.val integer}, {.val logical}, and {.val character} are allowed.",
      "!" = "Problematic column{?s}: {.var {bad_cols}} (Type{?s}: {.val {bad_types}})"
    ))
  }

  # Return both modified data and the types vector
  attr(data, "Types") <- col_types
  return(data)
}
