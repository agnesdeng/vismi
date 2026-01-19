.validate_m <- function(m, N_imp) {
  if (length(m) != 1 | m <= 0 | m != floor(m)) {
    stop("m must be a single positive integer.")
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
  }, character(1))

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
