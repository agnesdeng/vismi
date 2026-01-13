# tells data.table that we are using it
.datatable.aware <- TRUE

# Avoid R CMD check : 'no visible binding for global variable' NOTE
#' @importFrom utils globalVariables
#' @importFrom grDevices colorRampPalette
#' @keywords internal
utils::globalVariables(c(
  "Group", "..vars", "row_index", "prop", "count", "label_value"
))
