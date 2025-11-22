#' Internal helpers for overimputation plotting
#' @name overimp-helpers
#' @keywords internal
NULL

#' Coerce tabular inputs to plain data frames
#' @keywords internal
.overimp_as_data_frame <- function(data, label) {
  if (is.null(data)) {
    stop("`", label, "` must be supplied to draw overimputation diagnostics.", call. = FALSE)
  }

  if (data.table::is.data.table(data) || tibble::is_tibble(data)) {
    data <- as.data.frame(data)
  }

  data
}

#' Retrieve the masking matrix and imputed datasets for a given split
#' @keywords internal
.overimp_components <- function(obj, dataset = c("train", "test")) {
  dataset <- match.arg(dataset)
  if (dataset == "train") {
    list(mask = obj$params$addNA.m, imputations = obj$imputed.traindata)
  } else {
    list(mask = obj$params$addNA.m2, imputations = obj$imputed.testdata)
  }
}

#' Build a long data frame of observed and imputed values for a variable
#' @keywords internal
.overimp1d_long_data <- function(obj, var.name, data, dataset = c("train", "test")) {
  dataset <- match.arg(dataset)
  comps <- .overimp_components(obj, dataset)
  mask <- comps$mask
  imputations <- comps$imputations

  if (is.null(mask) || is.null(imputations)) {
    stop("Missing overimputation components for the `", dataset, "` dataset.", call. = FALSE)
  }

  if (is.null(mask[, var.name])) {
    stop("Mask for variable `", var.name, "` was not found in the `", dataset, "` dataset.", call. = FALSE)
  }

  idx <- mask[, var.name]
  if (!any(idx)) {
    stop("No artificially masked values detected for variable `", var.name, "` in the `", dataset, "` dataset.", call. = FALSE)
  }

  base_df <- .overimp_as_data_frame(data, sprintf("%s.data", dataset))
  reference <- base_df[, var.name][idx]

  values <- lapply(imputations, function(x) x[[var.name]][idx])
  names(values) <- paste0("m", seq_along(values))
  combined <- as.data.frame(values, stringsAsFactors = FALSE)
  combined$True <- reference

  measure_cols <- c(names(values), "True")
  combined_dt <- data.table::as.data.table(combined)
  data.table::melt(
    combined_dt,
    measure.vars = measure_cols,
    variable.name = "set",
    value.name = var.name
  )
}

#' Subtitle used across overimputation displays
#' @keywords internal
.overimp1d_subtitle <- function(m, var.name) {
  paste(
    paste("Distribution of", m, "imputed values"),
    paste("in variable", var.name),
    sep = "\n"
  )
}

#' Build long-format data for two-variable overimputation plots
#' @keywords internal
.overimp2d_long_data <- function(obj, var.x, var.y, data, dataset = c("train", "test")) {
  dataset <- match.arg(dataset)
  comps <- .overimp_components(obj, dataset)
  mask <- comps$mask
  imputations <- comps$imputations

  if (is.null(mask) || is.null(imputations)) {
    stop(sprintf("Missing overimputation components for the %s dataset.", dataset), call. = FALSE)
  }

  cols <- c(var.x, var.y)
  base_df <- .overimp_as_data_frame(data, sprintf("%s.data", dataset))
  missing_idx <- which(is.na(base_df[[var.x]]) | is.na(base_df[[var.y]]))
  observed_idx <- which(mask[, var.x] | mask[, var.y])
  idx <- observed_idx[!(observed_idx %in% missing_idx)]

  if (!length(idx)) {
    stop(
      sprintf("No overimputed entries found for %s and %s in the %s dataset.", var.x, var.y, dataset),
      call. = FALSE
    )
  }

  true_df <- base_df[idx, cols, drop = FALSE]

  extract_xy <- function(tbl) {
    if (data.table::is.data.table(tbl)) {
      out <- tbl[idx, cols, with = FALSE]
    } else {
      out <- tbl[idx, cols, drop = FALSE]
    }
    as.data.frame(out, stringsAsFactors = FALSE)
  }

  imputed_list <- lapply(imputations, extract_xy)
  names(imputed_list) <- paste0("m", seq_along(imputed_list))

  stacked <- c(list(True = true_df), imputed_list)
  combined <- data.table::rbindlist(stacked, idcol = "m")
  combined$m <- factor(combined$m, levels = names(stacked))
  attr(combined, "n_obs") <- length(idx)
  combined
}

#' Subtitle helper for two-variable displays
#' @keywords internal
.overimp2d_subtitle <- function(var.x, var.y) {
  paste("Distribution of overimputed values:", paste(var.y, "vs", var.x), sep = "\n")
}
