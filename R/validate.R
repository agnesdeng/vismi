.validate_incomplete_variable<-function(obj, var){

  if(inherits(obj,"mixgb")){
    missing_vars <- obj$params$missing.vars
  }else if(inherits(obj,"mids")){
    missing_vars <- names(obj$nmis)[obj$nmis>0]
  }
  if(!(var %in% missing_vars)){
    stop(paste0("Variable '", var, "' not found in the imputed object. Please check your spelling. Valid incomplete variables are", paste(missing_vars, collapse = ", "), "."))
}
}

.validate_data <- function(data, verbose, integerAsFactor, max_levels = 50) {
  # check if data is data.frame, tibble or data.table -----------------------
  if (!is.data.frame(data)) {
    stop("data must be a data.frame, tibble or data.table.")
  }

  # abvoid modifying original data in-place
  if (inherits(data, "data.table")) {
    data <- data.table::copy(data)
  }

  Names <- colnames(data)

  # check type of variables -------------------------------------------------

  if (verbose) {
    cli::cli_h1("Sanity Check")
  }


  Types <- vapply(
    Names, function(name) {
      col <- data[[name]]
      if (inherits(col, "ordered")) {
        if (isTRUE(verbose)) {
          cli::cli_alert_info(
            "{.var {name}}: ordered factor detected; treated as factor for plotting."
          )
        }
        "factor"
      } else if (inherits(col, "logical")) {
        if (isTRUE(verbose)) {
          cli::cli_alert_info(
            "{.var {name}}: logical variable detected; treated as factor for plotting."
          )
        }

        "factor"
      } else if (inherits(col, "integer")) {
        if (isTRUE(verbose) & integerAsFactor) {
          cli::cli_alert_info(
            "{.var {name}}: integer variable detected; treated as factor for plotting as `integerAsFactor = TRUE`."
          )
        } else if (isTRUE(verbose) & integerAsFactor == FALSE) {
          cli::cli_alert_info(
            "{.var {name}}: integer variable detected; treated as numeric for plotting as `integerAsFactor = FALSE`."
          )
        }

        if (isTRUE(integerAsFactor)) {
          data[[name]] <- as.factor(col)
          "factor"
        } else {
          "numeric"
        }
      } else if (inherits(col, "character")) {
        if (isTRUE(verbose)) {
          cli::cli_alert_warning(
            "{.var {name}}: character variable detected; treated as factor for plotting."
          )
        }

        data[[name]] <- as.factor(col)
        "factor"
      } else {
        class(col)[1]
      }
    },
    character(1)
  )


  if (!all(Types %in% c("numeric", "factor"))) {
    stop("Variables need to be of type `numeric`, `integer`, `logical`, `character`, `factor` or `ordinal factor`.")
  }


  # check NaN ---------------------------------------------------------------
  nan_num <- do.call(cbind, lapply(data, is.nan))
  if (any(nan_num)) {
    col_idx <- unique(which(nan_num, arr.ind = TRUE)[, 2])
    nan_col <- Names[col_idx]
    if (verbose) {
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
    nan_col <- Names[col_idx]

    cli::cli_alert_warning(c(
      "Character {.emph \"NaN\"} values detected in {.var {nan_col}}.",
      " They have been converted to {.emph NA}."
    ))

    data[nan_chr] <- NA
    # drop "NaN" level
    data <- droplevels(data)
  }

  # check Inf and -Inf ------------------------------------------------------
  inf_num <- do.call(cbind, lapply(data, is.infinite))

  if (any(inf_num)) {
    col_idx <- unique(which(inf_num, arr.ind = TRUE)[, 2])
    inf_col <- names(data)[col_idx]

    cli::cli_alert_warning(c(
      "Numeric {.emph Inf/-Inf} values detected in {.var {inf_col}}.",
      "They have been converted to {.emph NA}."
    ))

    data[inf_num] <- NA
  }


  inf_chr <- (data == "Inf" | data == "-Inf")
  inf_chr[is.na(inf_chr)] <- FALSE

  if (any(inf_chr)) {
    col_idx <- unique(which(inf_chr, arr.ind = TRUE)[, 2])
    inf_col <- names(data)[col_idx]

    cli::cli_alert_warning(c(
      "Character / factor {.emph \"Inf\" / \"-Inf\"} values detected in {.var {inf_col}}",
      "They have been converted to {.emph NA}."
    ))

    data[inf_chr] <- NA
    # drop "Inf"/"-Inf" level
    data <- droplevels(data)
  }

  # check empty cells "" ----------------------------------------------------
  empty_chr <- data == ""
  empty_chr[is.na(empty_chr)] <- FALSE

  if (any(empty_chr)) {
    col_idx <- unique(which(empty_chr, arr.ind = TRUE)[, 2])
    empty_col <- Names[col_idx]

    cli::cli_alert_warning(c(
      "Empty string {.emph \"\"} values detected in {.var {empty_col}}.",
      " They have been converted to {.emph NA}."
    ))

    # replace empty strings with NA
    data[empty_chr] <- NA

    # drop empty string level from factors
    data <- droplevels(data)
  }


  # check single level factor -----------------------------------------------
  idx <- which(sapply(data, nlevels) == 1)

  if (length(idx) >= 1) {
    single_col <- names(data)[idx]

    if (interactive()) {
      cat("Factor variable(s) with only one level detected in:", paste(single_col, collapse = ", "), "\n")
      proceed<-askYesNo("Do you want to remove them from the data and proceed?")
    } else {
      proceed <- FALSE # default for non-interactive sessions
    }

    if (proceed) {
      cli::cli_alert_warning("Removing columns with only one level: {.var {single_col}}")
      if (inherits(data, "data.table")) {
        data[, (single_col) := NULL]
      } else {
        data <- data[, -idx, drop = FALSE]
      }
    } else {
      stop("User chose not to proceed. Please check the data")
    }
  }


  # check factors with too many levels -------------------------------------
  num_levels <- sapply(data, nlevels)
  num_row <- nrow(data)

  idx <- which(num_levels > max_levels)
  if (length(idx) > 0) {
    toomany_col <- names(data)[idx]


    if (interactive()) {
      cat("Factor variable(s) with more than", max_levels, "levels detected in:",
          paste(toomany_col, collapse = ", "), "\n")
      proceed<-askYesNo("Do you want to keep them and proceed?")
    } else {
      proceed <- FALSE # default for non-interactive sessions
    }

    if (proceed) {
      cli::cli_alert_warning("User chose to keep columns with too many levels: {.var {toomany_col}}")
    } else {
      cli::cli_abort("User chose not to proceed. Please check variables with too many levels: {.var {toomany_col}}")
    }
  }

  list(data = data, Types = Types)
}


.validate_m <- function(m, N_imp) {
  if (length(m) != 1 | m <= 0 | m != floor(m)) {
    stop("m must be a single positive integer.")
  }
}
